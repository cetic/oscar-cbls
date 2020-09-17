package oscar.cbls.lib.search.combinators

import oscar.cbls.core.objective.{CascadingObjective, FunctionObjective, Objective}
import oscar.cbls.core.search.{InstrumentedMove, MoveFound, Neighborhood, NeighborhoodCombinator, NoMoveFound, OverrideObj, SearchResult}

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

  def notifyMoveTaken(): Unit ={
    moveCount += 1L
    temperatureValue = iterationToTemperature(moveCount)
  }

  //this resets the internal state of the move combinators
  override def reset(): Unit ={
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
@deprecated("use GLS3 instead","")
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
    println(s"resetting GLS currentWeightOfObj=$currentWeightOfObj")
  }

  override def getMove(obj: Objective, initialObj: Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {

    println(s"GLS getMove currentWeightOfObj:$currentWeightOfObj")
    if (currentWeightOfObj > 0) {
      //it is still a soft constraint
      val initValueOFConstaint = additionalConstraint.value
      if (initValueOFConstaint == 0 && it >= allowSwitchToStrongAfterIt) {
        //we are going GeneralizedLocalSearch, but the strong constraint is fine,
        //we can swith to a strong constraint
        currentWeightOfObj = 0
        println(s"GLS getMove, strong constraints are fine, so switching to Strong (it:$it)")
        return getMove(obj, initialObj, acceptanceCriterion) //recursive call
      }

      println(s"GLS getMove, soft constraint currentWeightOfObj:$currentWeightOfObj initValueOFConstraint:$initValueOFConstaint")

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
          println(s"MoveFound $m")
          //a move was found,
          //we decrease the weighting anyway, so the next iteration will be more directed towards target

          //it += 1
          currentWeightOfObj = 0 max iterationToWeight(it)

          val replacementOBjValue = if(emulateOriginalObj && bestCompositeObj == m.objAfter){
            baseOBjAtBestCompositeObj
          }else{
            System.err.println(s"could not emulate bestCompositeObj:$bestCompositeObj!= m.objAfter:${m.objAfter}")
            Long.MaxValue
          }
          MoveFound(new OverrideObj(m.m, replacementOBjValue))
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
            MoveFound(new OverrideObj(m.m, Long.MaxValue))
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


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

object GuidedLocalSearch3 {
  /**
   * This is a combination of a constraint with an objective function.
   * the idea is to consider the constraint as a weak constraint, and sum this violation to the objective function with weighting.
   *
   * It overrides the objective function and uses CompositeObj, which is declared as follows:
   * compositeObj = obj*weightForBase + constraint*weightForAdditionalConstraint
   ** weightForAdditionalConstraint is a constant value set by the weightCorrectionStrategy
   ** weightForBase is a value that is controlled is a progressive way and decreases throughout the search.
   *
   * Throughout the search, the relative weighing of the constraint is increased (that is, the weightForBase decreases)
   * until it gets to a strong constraint, and this process is controlled by the weightCorrectionStrategy.
   * This process has been defined to ensure that a decrease in the objective function is an improve on the overall situation,
   * and ensure that a progress in the weighting is not considering as a worsening of obj.
   *
   * This strategy is as follows:
   *
   * At startup, the weights are as given in parameters:
   ** weightForAdditionalConstraint = constantWeightForAdditionalConstraint
   ** weightForBase = startWeightForBase
   *
   * At each moveFound, weightForBase decreases by one unit, to mut more pressure on the constraints
   * the constraints can be set to strong constraints if these two conditions are met
   ** there at at least minimumIterationsBeforeStrong iterations
   ** there are nbConsecutiveIterationWithConstraintTrueBeforeStrong iterations where the constraints have been true
   *
   * When there is NoMoveFound,
   ** either the weight is decreased by one unit, to try a slightly different objective
   ** after consecutiveFailsBeforeDivByTwo iterations with no moveFound, the weight is divided by two, to try radically different compositions
   *   and ensure that the local minimum is not artificially generated by the compositeObj.
   *   this consecutiveFailsBeforeDivByTwo is reset and if to be performed again if there are no new MoveFound
   ** After maxAttemptsBeforeStop with NoMoveFound,
   *** if tryWeight2WhenNomoveFound  is set and weight is not two, the strategy performs a last attempts before stopping
   *** the strategy stops
   *
   * A very progressive and safe way of using the strategy is as follows:
   * {{{
   * progressive(
   *   startWeightForBase = n, //some metrics of your problem
   *   constantWeightForAdditionalConstraint = 2*n,
   *   minimumIterationsBeforeStrong = n/10, //to allow the tool violating the constraints and focus on obj
   *   nbConsecutiveIterationWithConstraintTrueBeforeStrong = n/10,
   *   consecutiveFailsBeforeDivByTwo = 2,
   *   maxAttemptsBeforeStop = 6,
   *   tryWeight2WhenNoMoveFound=true))
   *}}}
   * A much more aggressive way of doing is as follows:
   * {{{
   * progressive(
   *   startWeightForBase = n, //some metrics of your problem
   *   constantWeightForAdditionalConstraint = n, //wit this setting, constraint and OBj are equal, so they have less freedom to be violated (although it really is dependent of the unit used in your problem)
   *   minimumIterationsBeforeStrong = 10 min (n/100), //to allow the tool violating the constraints and focus on obj
   *   nbConsecutiveIterationWithConstraintTrueBeforeStrong = 10,
   *   consecutiveFailsBeforeDivByTwo = 0,
   *   maxAttemptsBeforeStop = 3,
   *   tryWeight2WhenNoMoveFound=false))
   *}}}
   *
   * @param a the neighborhood to consider
   * @param additionalConstraint an additional constraint, considered as a weak constraint at startup, and gradually, as a strong constraint.
   * @param startWeightForBase the initial weight for obj
   * @param constantWeightForAdditionalConstraint the weight for the additional constraint
   * @param minimumIterationsBeforeStrong the number of iteration before the constraints can be considered as strong (if they have been true a consecutive number of iterations >= nbConsecutiveIterationWithConstraintTrueBeforeStrong)
   * @param nbConsecutiveIterationWithConstraintTrueBeforeStrong an additional restriction on hen constraints ca be considered as strong (provided they are true)
   * @param consecutiveFailsBeforeDivByTwo the number of attempts to find a move before the weight is divided by two (otherwise it is only decreased by one)
   * @param maxAttemptsBeforeStop the number of consecutive NoMoveFound before the strategy globally return NoMoveFound
   * @param tryWeight2WhenNoMoveFound when there ias a series of NoMoveFound and the strategy is about to stop, perform a last attempt with weight set to min,
   *                                  to ensure that we are not stuck in a local minimum artificially created by the composition
   */
  def progressiveGuidedLocalSearch(a: Neighborhood,
                                   additionalConstraint:Objective,
                                   startWeightForBase: Long,
                                   constantWeightForAdditionalConstraint:Long,
                                   minimumIterationsBeforeStrong: Long,
                                   nbConsecutiveIterationWithConstraintTrueBeforeStrong:Long,
                                   consecutiveFailsBeforeDivByTwo:Long,
                                   maxAttemptsBeforeStop:Int,
                                   tryWeight2WhenNoMoveFound:Boolean):GuidedLocalSearch3 =
    new GuidedLocalSearch3(a: Neighborhood,
      additionalConstraint:Objective,
      weightCorrectionStrategy = new Progressive(startWeightForBase: Long,
        constantWeightForAdditionalConstraint:Long,
        minimumIterationsBeforeStrong: Long,
        nbConsecutiveIterationWithConstraintTrueBeforeStrong:Long,
        consecutiveFailsBeforeDivByTwo:Long,
        maxAttemptsBeforeStop:Int,
        tryWeight2WhenNoMoveFound:Boolean),
      maxAttemptsBeforeStop)
}

abstract class WeightCorrectionStrategy{
  /**
   * this method is called before exploration takes place
   * weight < 0 => stop the search. unless there is a reset
   * weight = 0 => additionalConstraint (forget about obj)
   * weight = 1 =>  cascading(constraint,obj)
   * weight > 1 =>  weight*obj + constantWeightForConstraint*constraint
   *
   * @param found
   * @param weight
   * @param sCViolation
   * @return new neight
   */
  def getNewWeight(found:Boolean, weight:Long, sCViolation:Long):Long

  def startWeightForBase:Long
  def constantWeightForAdditionalConstraint:Long

  def weightForBaseReset():Unit = {}
}

/**
 * This is a weightCorrectionStrategy for the GLS heuristics.
 *
 * The GLS overrides the objective function and uses CompositeObj, which is declared as follows:
 * compositeObj = obj*weightForBase + constraint*weightForAdditionalConstraint
 ** weightForAdditionalConstraint is a constant value set by the weightCorrectionStrategy
 ** weightForBase is a value that is controlled by the weightCorrectionStrategy. It must decrease throughout the search.
 *
 * Throughout the search, the relative weighing of the constraint is increased (that is, the weightForBase decreases)
 * until it gets to a strong constraint, and this process is controlled by the weightCorrectionStrategy.
 * This process has been defined to ensure that a decrease in the objective function is an improve on the overall situation,
 * and ensure that a progress in the weighting is not considering as a worsening of obj.
 *
 * This progressive strategy is as follows:
 *
 * At startup, the weights are as given in parameters:
 ** weightForAdditionalConstraint = constantWeightForAdditionalConstraint
 ** weightForBase = startWeightForBase
 *
 * At each moveFound, weightForBase decreases by one unit, to mut more pressure on the constraints
 * the constraints can be set to strong constraints if these two conditions are met
 ** there at at least minimumIterationsBeforeStrong iterations
 ** there are nbConsecutiveIterationWithConstraintTrueBeforeStrong iterations where the constraints have been true
 *
 * When there is NoMoveFound,
 ** either the weight is decreased by one unit, to try a slightly different objective
 ** after consecutiveFailsBeforeDivByTwo iterations with no moveFound, the weight is divided by two, to try radically different compositions
 *   and ensure that the local minimum is not artificially generated by the compositeObj.
 *   this consecutiveFailsBeforeDivByTwo is reset and if to be performed again if there are no new MoveFound
 ** After maxAttemptsBeforeStop with NoMoveFound,
 *** if tryWeight2WhenNomoveFound  is set and weight is not two, the strategy performs a last attempts before stopping
 *** the strategy stops
 *
 * A very progressive and safe way of using the strategy is as follows:
 * {{{
 * progressive(
 *   startWeightForBase = n, //some metrics of your problem
 *   constantWeightForAdditionalConstraint = 2*n,
 *   minimumIterationsBeforeStrong = n/10, //to allow the tool violating the constraints and focus on obj
 *   nbConsecutiveIterationWithConstraintTrueBeforeStrong = n/10,
 *   consecutiveFailsBeforeDivByTwo = 2,
 *   maxAttemptsBeforeStop = 6,
 *   tryWeight2WhenNoMoveFound=true))
 *}}}
 * A much more aggressive way of doing is as follows:
 * {{{
 * progressive(
 *   startWeightForBase = n, //some metrics of your problem
 *   constantWeightForAdditionalConstraint = n, //wit this setting, constraint and OBj are equal, so they have less freedom to be violated (although it really is dependent of the unit used in your problem)
 *   minimumIterationsBeforeStrong = 10 min (n/100), //to allow the tool violating the constraints and focus on obj
 *   nbConsecutiveIterationWithConstraintTrueBeforeStrong = 10,
 *   consecutiveFailsBeforeDivByTwo = 0,
 *   maxAttemptsBeforeStop = 3,
 *   tryWeight2WhenNoMoveFound=false))
 *}}}
 *
 * @param startWeightForBase the initial weight for obj
 * @param constantWeightForAdditionalConstraint the weight for the additional constraint
 * @param minimumIterationsBeforeStrong the number of iteration before the constraints can be considered as strong (if they have been true a consecutive number of iterations >= nbConsecutiveIterationWithConstraintTrueBeforeStrong)
 * @param nbConsecutiveIterationWithConstraintTrueBeforeStrong an additional restriction on hen constraints ca be considered as strong (provided they are true)
 * @param consecutiveFailsBeforeDivByTwo the number of attempts to find a move before the weight is divided by two (otherwise it is only decreased by one)
 * @param maxAttemptsBeforeStop the number of consecutive NoMoveFound before the strategy globally return NoMoveFound
 * @param tryWeight2WhenNoMoveFound when there ias a series of NoMoveFound and the strategy is about to stop, perform a last attempt with weight set to min,
 *                                  to ensure that we are not stuck in a local minimum artificially created by the composition
 */
class Progressive(override val startWeightForBase: Long,
                  override val constantWeightForAdditionalConstraint:Long,
                  minimumIterationsBeforeStrong: Long,
                  nbConsecutiveIterationWithConstraintTrueBeforeStrong:Long,
                  consecutiveFailsBeforeDivByTwo:Long,
                  maxAttemptsBeforeStop:Int,
                  tryWeight2WhenNoMoveFound:Boolean) extends WeightCorrectionStrategy {

  var remainingConsecutiveIterationWithConstraintTrueBeforeStrong = nbConsecutiveIterationWithConstraintTrueBeforeStrong
  var remainingConsecutiveFailsBeforeDivByTwo = consecutiveFailsBeforeDivByTwo
  var remainingFailsBeforeStop = maxAttemptsBeforeStop

  override def weightForBaseReset(): Unit = {
    remainingConsecutiveIterationWithConstraintTrueBeforeStrong = nbConsecutiveIterationWithConstraintTrueBeforeStrong
  }

  override def getNewWeight(found: Boolean, weight: Long, sCViolation: Long): Long = {
    //this method is called before exploration takes place
    //weight < 0 => stop the search. unless there is a reset
    //weight = 0 => additionalConstraint (forget about obj)
    //weight = 1 =>  cascading(constraint,obj)
    //weight > 1 =>  weight*obj + constantWeightForConstraint*constraint
    if(sCViolation !=0) {
      remainingConsecutiveIterationWithConstraintTrueBeforeStrong = nbConsecutiveIterationWithConstraintTrueBeforeStrong
    }else{
      remainingConsecutiveIterationWithConstraintTrueBeforeStrong -=1
      if(remainingConsecutiveIterationWithConstraintTrueBeforeStrong <0) remainingConsecutiveIterationWithConstraintTrueBeforeStrong = 0
    }

    if (found) {
      remainingConsecutiveFailsBeforeDivByTwo = consecutiveFailsBeforeDivByTwo
      remainingFailsBeforeStop = maxAttemptsBeforeStop
      if (weight == 1) {
        //we are working with strong constraints, that's fine.
        require(sCViolation == 0)
        1
      } else {
        if (sCViolation == 0
          && weight < startWeightForBase - minimumIterationsBeforeStrong
          && remainingConsecutiveIterationWithConstraintTrueBeforeStrong == 0) {
          //we made a good slow progression to constraints, so we can jump on strong constraints now
          1
        } else {
          //slightly increase pressure on constraints
          2L max (weight - 1)
        }
      }
    } else { //not found

      remainingConsecutiveFailsBeforeDivByTwo -= 1
      if(remainingConsecutiveFailsBeforeDivByTwo < 0) remainingConsecutiveFailsBeforeDivByTwo = 0
      remainingFailsBeforeStop -= 1
      if(remainingFailsBeforeStop < 0) remainingFailsBeforeStop = 0

      if(remainingFailsBeforeStop==0) {
        if (weight <= 2 || !tryWeight2WhenNoMoveFound) return -1 //Stop
        else return 2
      }

      if (weight == 1) {
        //were dealing with strong constraints; we stop here.
        -1
      } else {
        //we are not dealing with strong constraints
        //we increase pressure on the obj a little bit
        if(remainingConsecutiveFailsBeforeDivByTwo == 0){
          remainingConsecutiveFailsBeforeDivByTwo = consecutiveFailsBeforeDivByTwo
          2L max (weight /2)
        }else{
          2L max (weight - 1)
        }
      }
    }
  }
}


/**
 * This is a combination of a constraint with an objective function.
 * the idea is to consider the constraint as a weak constraint, and sum this violation to the objective function with weighting.
 *
 * This combinator overrides the objective function and uses CompositeObj, which is declared as follows:
 * compositeObj = obj*weightForBase + constraint*weightForAdditionalConstraint
 ** weightForAdditionalConstraint is a constant value set by the weightCorrectionStrategy
 ** weightForBase is a value that is controlled by the weightCorrectionStrategy. It must decrease throughout the search.
 *
 * Throughout the search, the relative weighing of the constraint is increased (that is, the weightForBase decreases)
 * until it gets to a strong constraint, and this process is controlled by the weightCorrectionStrategy.
 * This process has been defined to ensure that a decrease in the objective function is an improve on the overall situation,
 * and ensure that a progress in the weighting is not considering as a worsening of obj.
 *
 * weightForBase has the four categories of possible values that the weightCorrectionStrategy can use:
 ** weightForBase < 0 => NoMoveFound
 ** weightForBase = 0 => compositeObj = constraint (forget about obj)
 ** weightForBase = 1 =>  compositeObj = cascading(constraint,obj) that is the constraint is taken as a strong constraint
 ** weightForBase > 1 =>  compositeObj = weight*obj + constantWeightForConstraint*constraint
 *
 * @param a the neighborhood to consider
 * @param additionalConstraint an additional constraint, considered as a weak constraint at startup, and gradually, as a strong constraint.
 * @param weightCorrectionStrategy how the relative weight of obj and additional constraint evolve
 * @param maxAttemptsBeforeStop tolerated number of consecutive calls to weight correction without any move found.
 *                              This is merely defensive programming against the weightCorrectionStrategy.
 *                              The weightCorrectionStrategy might propoze the same parameter. In his case, parameter here can be set to something large such as 20.
 */
class GuidedLocalSearch3(a: Neighborhood,
                         additionalConstraint:Objective,
                         weightCorrectionStrategy:WeightCorrectionStrategy,
                         maxAttemptsBeforeStop:Int = 10
                        ) extends NeighborhoodCombinator(a) {

  //call this before any neigborhood exploration to define the obj etc to use for this exploration.
  def compositeObjectiveAndOriginalOBjConverter(baseObj:Objective,
                                                initValForBase:Long,
                                                changingWeightForBaseObj:Long,
                                                additionalConstraint:Objective,
                                                initValForAdditional:Long,
                                                constantWeightForAdditionalConstraint:Long):(Objective,Long,Long=>Long) = {

    val initCompositeObj = (initValForBase * changingWeightForBaseObj) + (initValForAdditional * constantWeightForAdditionalConstraint)
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

    val (fun,initObj) = changingWeightForBaseObj match{
      case 0 => //neglect the primary objective, only the additional one is considered;
        // the primary does not need to be evaluated, so we put a dedicated code here to do that.
        //the bestBaseOBj in this case is not logged
        baseOBjAtBestCompositeObj = Long.MaxValue
        (() => {
          val objValue = baseObj.value
          if (objValue == Long.MaxValue) {
            objValue
          } else {
            constantWeightForAdditionalConstraint * additionalConstraint.value
          }
        },
          constantWeightForAdditionalConstraint * initValForAdditional)
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
            (constantWeightForAdditionalConstraint * additionalConstraint.value) + (changingWeightForBaseObj * baseObjValue)
          }
          logObj(baseObjValue,toReturn)
          toReturn
        },
          if (initValForBase == Long.MaxValue) {
            Long.MaxValue
          } else {
            (constantWeightForAdditionalConstraint * initValForAdditional) + (changingWeightForBaseObj * initValForBase)
          })
    }

    (new FunctionObjective(fun),initObj, foundCompositeObjToBaseObj)
  }

  val store = additionalConstraint.model

  var weightForBase: Long = weightCorrectionStrategy.startWeightForBase
  weightCorrectionStrategy.weightForBaseReset()

  override def reset(): Unit = {
    weightForBase = weightCorrectionStrategy.startWeightForBase
    weightCorrectionStrategy.weightForBaseReset()

    super.reset()
    if(printExploredNeighborhoods) println("resetting GLS")
  }

  override def getMove(obj: Objective, initialObj: Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {

    val initValForAdditional = additionalConstraint.value
    weightForBase = weightCorrectionStrategy.getNewWeight(true,weightForBase,initValForAdditional)
    getMoveNoUpdateWeight(obj, initialObj, acceptanceCriterion, initValForAdditional, maxAttemptsBeforeStop)
  }

  def weightString(weightForBase:Long):String =  weightForBase match{
    case 0L => "forget obj, focus on additional constraints"
    case 1L => "additional are strong Constraints"
    case x if x > 1L =>  s"relativeWeight:$x/${weightCorrectionStrategy.constantWeightForAdditionalConstraint}"
    case x if x < 0L => "interrupted"
  }

  def getMoveNoUpdateWeight(obj: Objective, initialObj: Long, acceptanceCriterion: (Long, Long) => Boolean, initValForAdditional:Long, remainingAttemptsBeforeStop:Int): SearchResult = {
    if(remainingAttemptsBeforeStop == 0) {
      if(printExploredNeighborhoods){
        println("GLS stop because remainingAttemptsBeforeStop==0")
      }
      return NoMoveFound
    }

    if(weightForBase <0) {
      if(printExploredNeighborhoods){
        println("GLS stopped by weightCorrectionStrategy")
      }
      return NoMoveFound
    }

    if(printExploredNeighborhoods){
      println(s"GLS trying;${weightString(weightForBase)}")
    }

    val initValForAdditional = additionalConstraint.value

    val (compositeObj,initCompositeObj,compositeObjToBaseOBj) = compositeObjectiveAndOriginalOBjConverter(
      obj:Objective,
      initialObj,
      weightForBase,
      additionalConstraint,
      initValForAdditional,
      weightCorrectionStrategy.constantWeightForAdditionalConstraint)

    a.getMove(compositeObj,initCompositeObj, acceptanceCriterion) match {
      case NoMoveFound =>

        if(printExploredNeighborhoods){
          println("GLS got NoMoveFound")
        }

        //we try and update the weight
        val oldWeightForBase = weightForBase
        weightForBase = weightCorrectionStrategy.getNewWeight(false,weightForBase,initValForAdditional)

        if(oldWeightForBase == weightForBase) {
          if(printExploredNeighborhoods) println("GLS stop because weightForBase unchanged")
          return NoMoveFound
        }

        //policy has changed,so we try again
        getMoveNoUpdateWeight(obj, initialObj, acceptanceCriterion,initValForAdditional,remainingAttemptsBeforeStop-1)

      case m: MoveFound =>
        if(printExploredNeighborhoods) println(s"GLS got MoveFound $m")
        //a move was found, good
        val correctedObj = compositeObjToBaseOBj(m.objAfter)
        if(m.objAfter == correctedObj){
          m
        }else{
          MoveFound(new OverrideObj(m.m, correctedObj))
        }
    }
  }
}
