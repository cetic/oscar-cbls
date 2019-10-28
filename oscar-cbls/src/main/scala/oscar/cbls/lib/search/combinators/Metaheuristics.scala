package oscar.cbls.lib.search.combinators

import oscar.cbls._
import oscar.cbls.core.objective.{CascadingObjective, FunctionObjective, Objective}
import oscar.cbls.core.search.{NoMoveFound, _}

import scala.language.postfixOps

object Restart{
  /**
    * performs a restart of the search for a number of time.
    * it queries neighborhood on the left every time (this is the search neighborhood)
    * if the search neighborhood is exhausted, it queries the randomizationNeighborhood once (this is the randomization neighborhood, and resets the neighborhood on the left
    * the process of restarting is allowed maxRestartWithoutImprovement time without improvement over the objective function obj,
    * that is: every time the search neighborhood is exhausted, it checks if the search delivered an improvement over the objective function,
    * and the restart is only performed if it could find an improvement at least once in the last "maxRestartWithoutImprovement" descents.
    *
    * the best solution is reloaded at exhaustion of this neighborhood.
    *
    * @param randomizationNeighborhood the neighborhood that will randomize the current solution
    * @param maxRestartWithoutImprovement the stop criterion of the restarting
    * @param obj the objective function
    */
  def apply(n:Neighborhood,randomizationNeighborhood:Neighborhood, maxRestartWithoutImprovement:Long, obj:Objective, restartFromBest:Boolean=false) = {
    ((if(restartFromBest) n saveBestOnExhaustAndRestoreOnExhaust obj else n) orElse (randomizationNeighborhood
      maxMoves maxRestartWithoutImprovement withoutImprovementOver obj improvementBeignMeasuredBeforeNeighborhoodExploration)
      ) saveBestAndRestoreOnExhaust obj
  }
}

/**
  * this combinator injects a metropolis acceptation function.
  * the criterion accepts all improving moves, and for worsening moves, it applies the metropolis criterion:
  * accept if math.random(0.0; 1.0) < base exponent (-gain / temperatureValue)
  *
  * @param a the original neighborhood
  * @param iterationToTemperature a function that inputs the number of moves of a that have been actually taken,
  *                    and outputs a temperature, for use in the criterion
  *                    the number of steps is reset to zero when the combinator is reset
  *                    by default, it is the constant function returning 100L
  * @param base the base for the exponent calculation. default is 2L
  */
class Metropolis(a: Neighborhood, iterationToTemperature: Long => Double = _ => 100, base: Double = 2) extends NeighborhoodCombinator(a) {

  var moveCount = 0L
  var temperatureValue: Double = iterationToTemperature(moveCount)

  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult =
    a.getMove(obj, initialObj:Long, acceptation) match {
      case NoMoveFound => NoMoveFound
      case MoveFound(m) => InstrumentedMove(m, notifyMoveTaken _)
    }

  def acceptation(oldObj: Long, newObj: Long): Boolean = {
    val gain = oldObj - newObj
    if (gain > 0L){
      true
    } else {
      // metropolis criterion

      val relativeIncrease = - gain.toFloat / oldObj.toFloat

      //println("relativeIncrease: " + relativeIncrease)
      //println("temp:" + temperatureValue)

      val toReturn = math.random < math.pow(base, - relativeIncrease / temperatureValue)

      //println("metropolis decision: " + toReturn)

      toReturn
    }
  }

  def notifyMoveTaken() {
    moveCount += 1L
    temperatureValue = iterationToTemperature(moveCount)
  }

  //this resets the internal state of the move combinators
  override def reset() {
    super.reset()
    moveCount = 0L
    temperatureValue = iterationToTemperature(moveCount)
  }
}

/**
  * This is a combination of a constraint with an objective function.
  * the idea is to consider the constraint as a weak constraint, and sum this violation to the objective function with weighting.
  * throughout the search, the relative weighing of the constraint is increased until it gets to a strong constraint.
  *
  * @param a the neighborhood to consider
  * @param additionalConstraint an additional constraint, considered as a weak constraint at startup, and gradually, as a strong constraint.
  * @maxValueForObj the maximal value for the objective function and for the constraint (do not exceed MaxInt)
  */
class GeneralizedLocalSearch(a: Neighborhood,
                             additionalConstraint:Objective,
                             iterationToWeight:Int => Int,
                             allowSwitchToStrongAfterIt:Int) extends NeighborhoodCombinator(a) {

  val maxValueForWeighting: Int = iterationToWeight(0)

  var it: Int = 0
  var currentWeightOfObj: Int = maxValueForWeighting

  val store = additionalConstraint.model

  override def reset(): Unit = {
    it = 0
    currentWeightOfObj = maxValueForWeighting
    super.reset()
    println("resetting GLS currentWeightOfObj=" + currentWeightOfObj)
  }

  override def getMove(obj: Objective, initialObj: Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {

    //println("GLS getMove currentWeightOfObj:" + currentWeightOfObj)
    if (currentWeightOfObj > 0) {
      //it is still a soft constraint
      val initValueOFConstaint = additionalConstraint.value
      if (initValueOFConstaint == 0 && it >= allowSwitchToStrongAfterIt) {
        //we are going GeneralizedLocalSearch, but the strong constraint is fine,
        //we can swith to a string constraint
        currentWeightOfObj = 0
        //println("GLS getMove, strong constraints are fine, so switching to Strong (it:" + it + ")")
        return getMove(obj, initialObj, acceptanceCriterion)
      }

      //println("GLS getMove, soft constraint currentWeightOfObj:" + currentWeightOfObj + " initValueOFConstaint:" + initValueOFConstaint)

      a.getMove(
        new FunctionObjective(() => {
          val objValue = obj.value
          if (objValue == Long.MaxValue) objValue
          else (maxValueForWeighting * additionalConstraint.value) + (currentWeightOfObj * objValue)
        }, store),
        (maxValueForWeighting * initValueOFConstaint) + (currentWeightOfObj * initialObj),
        acceptanceCriterion) match {
        case NoMoveFound =>
          //println("NoMoveFound")

          //it's time to change the weighting?
          if (initValueOFConstaint == 0 || currentWeightOfObj == 1) {
            //srong constraints are fine, or weighting is close to strong constraints
            // so we switch to strong constraints
            currentWeightOfObj = 0
            this.getMove(obj, initialObj, acceptanceCriterion)
          } else {
            //assume 100 iterations, and continue
            it += 100
            currentWeightOfObj = 0 max iterationToWeight(it)

            this.getMove(obj, initialObj, acceptanceCriterion)
          }
        case m: MoveFound =>
          //println("MoveFound " + m)
          //a move was found,
          //we decrease the weighting anyway, s othe next iteration will be more directed towards target

          it += 1
          currentWeightOfObj = 0 max iterationToWeight(it)

          MoveFound(new MoveWithOtherObj(m.m, Long.MaxValue))
      }
    } else if (currentWeightOfObj == 0) {
      //strong constraint

      val constraintViolation = additionalConstraint.value
      //println("GLS getMove, strong constraint; violation should be zero: is:" + constraintViolation)
      if (constraintViolation != 0) {
        //println("violation is not zero, so we only optimize on the violation")

        //System.err.println("GLS getMove, error stuff")
        //there is a problem; we are supposed to deal with enforced constraints here, so we reset the counter
        //we have a problem; there is a violation and we cannot go smaller, so temporarily, we forget the obj at all
        a.getMove(additionalConstraint, constraintViolation, acceptanceCriterion) match{
          case NoMoveFound => NoMoveFound
          case m: MoveFound =>
            //println("MoveFound " + m)
            MoveFound(new MoveWithOtherObj(m.m, Long.MaxValue))
        }

      } else {
        //great, we can just post it as a strong constraint
        a.getMove(new CascadingObjective(additionalConstraint, obj), initialObj, acceptanceCriterion) match{
          case NoMoveFound => NoMoveFound
          case m: MoveFound =>
            //println("MoveFound " + m)
            MoveFound(new MoveWithOtherObj(m.m, Long.MaxValue))
        }
      }
    } else {
      //solving violation, forget about obj

      require(false, "should not happen")
      null
    }
  }
}

