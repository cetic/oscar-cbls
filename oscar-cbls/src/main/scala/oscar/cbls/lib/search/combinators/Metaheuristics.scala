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


/*système de transition:
* trouvé =>
* pas trouvé
* contrainte forte
*
* */


/**
 * This is a combination of a constraint with an objective function.
 * the idea is to consider the constraint as a weak constraint, and sum this violation to the objective function with weighting.
 * throughout the search, the relative weighing of the constraint is increased until it gets to a strong constraint.
 *
 * @param a the neighborhood to consider
 * @param additionalConstraint an additional constraint, considered as a weak constraint at startup, and gradually, as a strong constraint.
 * @maxValueForObj the maximal value for the objective function and for the constraint (do not exceed MaxInt)
 */
class GuidedLocalSearch(a: Neighborhood,
                        additionalConstraint:Objective,
                        iterationToWeight:Int => Int,
                        allowSwitchToStrongAfterIt:Int,
                        emulateOriginalObj:Boolean = false) extends NeighborhoodCombinator(a) {

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

    println("GLS getMove currentWeightOfObj:" + currentWeightOfObj)
    if (currentWeightOfObj > 0) {
      //it is still a soft constraint
      val initValueOFConstaint = additionalConstraint.value
      if (initValueOFConstaint == 0 && it >= allowSwitchToStrongAfterIt) {
        //we are going GeneralizedLocalSearch, but the strong constraint is fine,
        //we can swith to a strong constraint
        currentWeightOfObj = 0
        println("GLS getMove, strong constraints are fine, so switching to Strong (it:" + it + ")")
        return getMove(obj, initialObj, acceptanceCriterion) //recursive call
      }

      println("GLS getMove, soft constraint currentWeightOfObj:" + currentWeightOfObj + " initValueOFConstaint:" + initValueOFConstaint)

      val initCompositeObj = (maxValueForWeighting * initValueOFConstaint) + (currentWeightOfObj * initialObj)
      var bestCompositeObj:Long = initCompositeObj
      var baseOBjAtBestCompositeObj:Long = initialObj

      a.getMove(
        new FunctionObjective(() => {
          val objValue = obj.value
          if (objValue == Long.MaxValue) objValue
          else {
            val compositeObj = (maxValueForWeighting * additionalConstraint.value) + (currentWeightOfObj * objValue)
            if(compositeObj < bestCompositeObj){
              baseOBjAtBestCompositeObj = objValue
              bestCompositeObj = compositeObj
            }else if(compositeObj == bestCompositeObj){
              //in this case there is potentially an ambiguity on the composite vs the base
              if(objValue != baseOBjAtBestCompositeObj){
                baseOBjAtBestCompositeObj = Long.MaxValue
              }
            }
            compositeObj
          }
        }, store),
        initCompositeObj,
        acceptanceCriterion) match {
        case NoMoveFound =>
          println("NoMoveFound")

          //it's time to change the weighting?
          if (initValueOFConstaint == 0 || currentWeightOfObj == 1) {
            //srong constraints are fine, or weighting is close to strong constraints
            // so we switch to strong constraints
            currentWeightOfObj = 0
            this.getMove(obj, initialObj, acceptanceCriterion)
          } else {
            //assume 100 iterations, and continue
            it += 100
            currentWeightOfObj = 1 max iterationToWeight(it)

            this.getMove(obj, initialObj, acceptanceCriterion)
          }
        case m: MoveFound =>
          println("MoveFound " + m)
          //a move was found,
          //we decrease the weighting anyway, so the next iteration will be more directed towards target

          //it += 1
          currentWeightOfObj = 0 max iterationToWeight(it)

          val replacementOBjValue = if(emulateOriginalObj && bestCompositeObj == m.objAfter){
            baseOBjAtBestCompositeObj
          }else{
            System.err.println("could not emulate bestCompositeObj:" + bestCompositeObj + "!= m.objAfter:" + m.objAfter)
            Long.MaxValue
          }
          MoveFound(new MoveWithOtherObj(m.m, replacementOBjValue))
        //TODO: here, the obj should be re-interpreted otherwise meta-heuristics cannot be added.
        //alternative is to decompose obj from the composite; is it feasible?
        // we can record the best obj and its related base obj, but we have no proof that it will be the returned one or the proper combination
        //so let's put it as an option.
        //also in case of VLSN, this just does not work at all



      }
    } else if (currentWeightOfObj == 0) {
      //strong constraint

      val constraintViolation = additionalConstraint.value
      //println("GLS getMove, strong constraint; violation should be zero: is:" + constraintViolation)
      if (constraintViolation != 0) {
        println("violation is not zero, so we only optimize on the violation")

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
        println("as strong constraint")
        //great, we can just post it as a strong constraint
        a.getMove(new CascadingObjective(additionalConstraint, obj), initialObj, acceptanceCriterion) match{
          case NoMoveFound => NoMoveFound
          case m: MoveFound =>
            //println("MoveFound " + m)
            m
        }
      }
    } else {
      //solving violation, forget about obj

      require(false, "should not happen")
      null
    }
  }
}


object GuidedLocalSearch3{

}

/**
 *
 * @param a
 * @param additionalConstraint
 * @param startWeightForBase
 * @param constantWeightForAdditionalConstraint
 * @param foundAndOldWeightAndConstraintEnforcedToNewWeight
 */
class GuidedLocalSearch3(a: Neighborhood,
                         additionalConstraint:Objective,
                         startWeightForBase:Long,
                         constantWeightForAdditionalConstraint:Long,
                         foundAndOldWeightAndConstraintEnforcedToNewWeight:(Boolean,Long,Boolean) => Long =
                         {
                           //this method is to be called before exploration takes place
                           //weight = 0 => usedOBj = additionalConstraint (forget about obj)
                           //weight = 1 => usedOBj = cascading(constraint,obj)
                           //weight > 1 => usedOBj = weight*obj + constantWeightForConstraint*constraint
                           case (true, weight, false) if weight != 0 => weight //keep the same weight
                           case (true, weight, false) if weight == 0 => 10 // try to reconsider obj, a little bit
                           case (false, weight, false) if weight > 1 => 1L max (weight/2) //we push more on obj
                           case (false, weight, false) if weight == 1 => 0 //forget about obj, only constraints for now
                           case (false, weight, false) if weight == 0 => 0 //we are really desperate
                           case (true, weight, true) => weight //we push more on obj
                           case (false, weight, true) => weight //we did not find do not change yet to strong constraints
                         }) extends NeighborhoodCombinator(a) {

  def compositeObjectiveAndOriginalOBjConverter(baseObj:Objective,
                                                initValForBase:Long,
                                                weightForBaseOBj:Long,
                                                additionalConstraint:Objective,
                                                initValForAdditional:Long,
                                                weightForAdditionalConstraint:Long):(Objective,Long,Long=>Long) = {

    val initCompositeObj = (initValForBase * weightForBaseOBj) + (initValForAdditional * weightForAdditionalConstraint)
    var bestCompositeObj:Long = initCompositeObj
    var baseOBjAtBestCompositeObj:Long = initValForBase

    def logObj(baseObjValue:Long,compositeObjValue:Long): Unit ={
      if(compositeObjValue < bestCompositeObj){
        bestCompositeObj = compositeObjValue
        baseOBjAtBestCompositeObj = baseObjValue
      }else if(compositeObjValue == bestCompositeObj){
        //in this case there is potentially an ambiguity on the composite vs the base
        if(baseObjValue != baseOBjAtBestCompositeObj){
          //we destroy the best because we cannot guarantee unicity
          baseOBjAtBestCompositeObj = Long.MaxValue
        }
      }
    }

    def foundCompositeObjToBaseObj(foundObj:Long):Long =
      if(foundObj == bestCompositeObj) baseOBjAtBestCompositeObj else Long.MaxValue

    val (fun,initObj) = weightForBaseOBj match{
      case 0 => //neglect the primary objective, only the additional one is considered;
        // the primary does not need to be evaluated, so we put a dedicated code here to do that.
        //the bestBaseOBj in this case is not logged
        baseOBjAtBestCompositeObj = Long.MaxValue
        (() => {
          val objValue = baseObj.value
          if (objValue == Long.MaxValue) {
            objValue
          } else {
            weightForAdditionalConstraint * additionalConstraint.value
          }
        },
          weightForAdditionalConstraint * initValForAdditional)
      case 1 => //flag telling that the additional constraints are strong, so cascading objective
        require(initValForAdditional==0, "cannot go for hard constraint while additional constraints are still violated")
        (() => {
          if (additionalConstraint.value == 0) {
            val toReturn = baseObj.value //we do not need multiply because weightForBaseOBj == 1
            logObj(toReturn,toReturn)
            toReturn
          } else {
            Long.MaxValue
          }
        },if(initValForAdditional==0) initValForBase else Long.MaxValue) //we leave here the cascading init if someone ever removes teh require hereabove
      case x => //otherwise use the linear combination, as usual
        require(x >=0)
        (() => {
          val baseObjValue = baseObj.value
          val toReturn = if (baseObjValue == Long.MaxValue) {
            baseObjValue
          } else {
            (weightForAdditionalConstraint * additionalConstraint.value) + (weightForBaseOBj * baseObjValue)
          }
          logObj(baseObjValue,toReturn)
          toReturn
        },
          if (initValForBase == Long.MaxValue) {
            Long.MaxValue
          } else {
            (weightForAdditionalConstraint * initValForAdditional) + (weightForBaseOBj * initValForBase)
          })
    }

    (new FunctionObjective(fun),initObj, foundCompositeObjToBaseObj)
  }

  val store = additionalConstraint.model

  var weightForBase: Long = startWeightForBase

  override def reset(): Unit = {
    weightForBase = startWeightForBase
    super.reset()
    println("resetting GLS")
  }

  override def getMove(obj: Objective, initialObj: Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {

    val initValForAdditional = additionalConstraint.value

    val (compositeObj,initCompositeObj,compositeObjToBaseOBj) = compositeObjectiveAndOriginalOBjConverter(
      obj:Objective,
      initialObj,
      weightForBase,
      additionalConstraint,
      initValForAdditional,
      constantWeightForAdditionalConstraint)

    a.getMove(compositeObj,initCompositeObj, acceptanceCriterion) match {
      case NoMoveFound =>
        println("NoMoveFound")

        //we try and update the weight
        val oldWeightForBase = weightForBase
        weightForBase = foundAndOldWeightAndConstraintEnforcedToNewWeight(false,weightForBase,initValForAdditional == 0)

        if(oldWeightForBase == weightForBase){
          //nothing changed, so we fail
          NoMoveFound
        }else {
          //policy has changed,so we try again (infinite looop danger here!!
          getMove(obj, initialObj, acceptanceCriterion)
        }
      case m: MoveFound =>
        println("MoveFound " + m)
        //a move was found, good

        weightForBase = foundAndOldWeightAndConstraintEnforcedToNewWeight(true,weightForBase,initValForAdditional == 0)
        val correctedObj = compositeObjToBaseOBj(m.objAfter)
        if(m.objAfter == correctedObj){
          m
        }else{
          MoveFound(new MoveWithOtherObj(m.m, correctedObj))
        }
    }
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
class GuidedLocalSearch2(a: Neighborhood,
                         additionalConstraint:Objective,
                         weightVariableForExtraObjective:CBLSIntVar,
                         iterationToWeight:Int => Int,
                         allowSwitchToStrongAfterIt:Int,
                         emulateOriginalObj:Boolean = false) extends NeighborhoodCombinator(a) {

  val maxValueForWeighting: Int = iterationToWeight(0)
  weightVariableForExtraObjective := maxValueForWeighting


  def compositeObjective(obj:Objective):Objective = { //the one that we expect to receive and that we will send to the listening constraints.
    new FunctionObjective(() => {
      val currentWeightOfObj = weightVariableForExtraObjective.value
      currentWeightOfObj match{
        case 0 => //neglect the primary objective, only the additional one is considered;
          // this does not need a particular code to be correct, bu the primary does not need to be evaluated
          val objValue = obj.value
          if (objValue == Long.MaxValue){
            objValue
          } else {
            maxValueForWeighting * additionalConstraint.value
          }
        case 1 => //flag telling that the additional constraints are strong, so cascading objective
          if (additionalConstraint.value == 0) {
            obj.value //we do not need multiply because currentWeightOfObj == 1
          }else{
            Long.MaxValue
          }
        case x => //otherwise use the linear combination, as usual
          val objValue = obj.value
          if (objValue == Long.MaxValue){
            objValue
          } else {
            (maxValueForWeighting * additionalConstraint.value) + (currentWeightOfObj * objValue)
          }
      }
    })
  }


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

    println(" GLS2 currentWeightOfObj" + currentWeightOfObj)
    //we expect obj to be our composite!!

    //println("GLS getMove currentWeightOfObj:" + currentWeightOfObj)
    if (currentWeightOfObj > 0) {
      //it is still a soft constraint
      val initValueOfConstraint = additionalConstraint.value
      if (initValueOfConstraint == 0 && it >= allowSwitchToStrongAfterIt) {
        //we are going GeneralizedLocalSearch, but the strong constraint is fine,
        //we can switch to a strong constraint
        currentWeightOfObj = 0
        println("GLS getMove, strong constraints are fine, so switching to Strong (it:" + it + ")")
        return getMove(obj, initialObj, acceptanceCriterion) //recursive call
      }

      //println("GLS getMove, soft constraint currentWeightOfObj:" + currentWeightOfObj + " initValueOFConstraint:" + initValueOFConstraint)
      val initCompositeObj = (maxValueForWeighting * initValueOfConstraint) + (currentWeightOfObj * initialObj)
      var bestCompositeObj:Long = initCompositeObj
      var baseOBjAtBestCompositeObj:Long = initialObj

      a.getMove(
        new FunctionObjective(() => { //= compositeObj?? non parce qu'on reçoit déjà un composite en fait.
          val objValue = obj.value
          if (objValue == Long.MaxValue) objValue
          else {
            val compositeObj = (maxValueForWeighting * additionalConstraint.value) + (currentWeightOfObj * objValue)
            if(compositeObj < bestCompositeObj){
              baseOBjAtBestCompositeObj = objValue
              bestCompositeObj = compositeObj
            }else if(compositeObj == bestCompositeObj){
              //in this case there is potentially an ambiguity on the composite vs the base
              if(objValue != baseOBjAtBestCompositeObj){
                baseOBjAtBestCompositeObj = Long.MaxValue
              }
            }
            compositeObj
          }
        }, store),
        initCompositeObj,
        acceptanceCriterion) match {
        case NoMoveFound =>
          //println("NoMoveFound")
          println("no move found recur currentWeightOfObj:" + currentWeightOfObj)

          //it's time to change the weighting?
          if (initValueOfConstraint == 0 || currentWeightOfObj == 1) {
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
          //we decrease the weighting anyway, so the next iteration will be more directed towards target

          it += 1
          currentWeightOfObj = 0 max iterationToWeight(it)

          val replacementOBjValue = if(emulateOriginalObj && bestCompositeObj == m.objAfter){
            baseOBjAtBestCompositeObj
          }else{
            if(emulateOriginalObj && bestCompositeObj != m.objAfter){
              bestCompositeObj != m.objAfter
            }
            System.err.println("could not emulate bestCompositeObj:" + bestCompositeObj + "!= m.objAfter:" + m.objAfter)
            Long.MaxValue
          }
          println("standard currentWeightOfObj:" + currentWeightOfObj)
          MoveFound(new MoveWithOtherObj(m.m, replacementOBjValue))

        //TODO: here, the obj should be re-interpreted otherwise meta-heuristics cannot be added.
        //alternative is to decompose obj from the composite; is it feasible?
        // we can record the best obj and its related base obj, but we have no proof that it will be the returned one or the proper combination
        //so let's put it as an option.
        //also in case of VLSN, this just does not work at all

      }
    } else if (currentWeightOfObj == 0) {
      //strong constraint

      val constraintViolation = additionalConstraint.value
      //println("GLS getMove, strong constraint; violation should be zero: is:" + constraintViolation)
      if (constraintViolation != 0) {
        //println("violation is not zero, so we only optimize on the violation")

        println("lost!!")

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
        println("strong constraint")

        //great, we can just post it as a strong constraint
        a.getMove(new CascadingObjective(additionalConstraint, obj), initialObj, acceptanceCriterion) match{
          case NoMoveFound => NoMoveFound
          case m: MoveFound =>
            //println("MoveFound " + m)
            println("strong constraint!")
            m
          //MoveFound(new MoveWithOtherObj(m.m, Long.MaxValue))
        }
      }
    } else {
      //solving violation, forget about obj

      require(false, "should not happen")
      null
    }
  }
}
