package oscar.cbls.lib.search.combinators

import oscar.cbls.core.objective.{FunctionObjective, Objective}
import oscar.cbls.core.search.{AcceptanceCriterion, InstrumentedMove, MoveFound, Neighborhood, NeighborhoodCombinator, NoMoveFound, OverrideObj, SearchResult}

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
  def apply(n:Neighborhood,
            randomizationNeighborhood:Neighborhood,
            maxRestartWithoutImprovement:Int,
            obj:Objective,
            restartFromBest:Boolean=false,
            minRestarts:Int = 0): Neighborhood = {
    ((if(restartFromBest) n saveBestOnExhaustAndRestoreOnExhaust obj else n) orElse ((randomizationNeighborhood
      maxMoves maxRestartWithoutImprovement withoutImprovementOver obj improvementBeingMeasuredBeforeNeighborhoodExploration) withMinimalMoves(minRestarts))
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
class Metropolis(a: Neighborhood,
                 iterationToTemperature: Long => Double = _ => 100,
                 base: Double = 2) extends NeighborhoodCombinator(a) {

  var moveCount = 0L
  var temperatureValue: Double = iterationToTemperature(moveCount)

  case object MetropolisCriterion extends AcceptanceCriterion {
    override def apply(oldValue: Long, newValue: Long): Boolean = {
      val gain = oldValue - newValue

      def applyMetropolis: Boolean = {
        // metropolis criterion
        val relativeIncrease = -gain.toFloat / oldValue.toFloat
        math.random() < math.pow(base, -relativeIncrease / temperatureValue)
      }

      (gain > 0) || applyMetropolis
    }
  }

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult =
    a.getMove(obj, initialObj, MetropolisCriterion) match {
      case NoMoveFound => NoMoveFound
      case MoveFound(m) => InstrumentedMove(m, notifyMoveTaken _)
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
 * implements the late acceptance criterion. Similarly to the simulated annealing it will accept degrading moves.
 * The acceptance is however not computed based on statistics. Instead there is a history of the "length" previous values,
 * and a pointer that iterates on these values.
 * It compares the next obj with the value fetched from the history and accepts improves over that historical value.
 * If the neighbour is accepted,the historical value is updated.
 *
 * more details in: Burke EK, Bykov Y (2016) The late acceptance hill-climbing heuristic. Eur J Oper Res 258:70–78
 * @param a the base neighbourhood
 * @param length the length of the history
 * @param maxRelativeIncreaseOnBestObj additionally, newOBj is rejected if > maxRelativeIncreaseOnBestObj*bestObj.
 *                                     This increases convergence, but decreased optimality of this approach.
 *                                     The default value is very large, so that this mechanism is inactive.
 */
class LateAcceptanceHillClimbing(a: Neighborhood,
                                 length: Int = 20,
                                 maxRelativeIncreaseOnBestObj: Double = 10000,
                                 initialObj: Option[Long] = None) extends NeighborhoodCombinator(a) {
  require(maxRelativeIncreaseOnBestObj > 1, "maybe you should not use LateAcceptanceHillClimbing if obj cannot increase anyway")

  val memory:Array[Long] = Array.fill(length)(Long.MaxValue)

  var initialized = false

  var maxToleratedObj: Long = Long.MaxValue
  var bestKnownObj: Long = Long.MaxValue

  def init(initialObj:Long): Unit = {
    for(i <- memory.indices) memory(i) = this.initialObj.getOrElse(initialObj)
    maxToleratedObj = Long.MaxValue
    bestKnownObj = Long.MaxValue
    initialized = true
  }

  var x = 0

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriterion: AcceptanceCriterion): SearchResult = {
    if (!initialized) init(initialObj)

    a.getMove(obj,initialObj,(oldOBj,newObj) => {

      //TODO these two lines should be done before a.getMove
      x = x+1
      if (x >= length) x = 0

      if (newObj < maxToleratedObj && (newObj < oldOBj || newObj < memory(x))){
        memory(x) = newObj
        if (newObj < bestKnownObj) {
          maxToleratedObj = ((newObj.toFloat * maxRelativeIncreaseOnBestObj) min Long.MaxValue).toLong
          bestKnownObj = newObj
        }
        true
      } else {
        false
      }
    })
  }

  override def reset(): Unit = {
    initialized = false
    super.reset()
  }
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

object GuidedLocalSearch {
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
                                   tryWeight2WhenNoMoveFound:Boolean):GuidedLocalSearch =
    new GuidedLocalSearch(a: Neighborhood,
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
 * @param tryWeight2WhenNoMoveFound when there is a series of NoMoveFound and the strategy is about to stop, perform a last attempt with weight set to min,
 *                                  to ensure that we are not stuck in a local minimum artificially created by the composition
 */
class Progressive(override val startWeightForBase: Long,
                  override val constantWeightForAdditionalConstraint:Long,
                  minimumIterationsBeforeStrong: Long,
                  nbConsecutiveIterationWithConstraintTrueBeforeStrong:Long,
                  consecutiveFailsBeforeDivByTwo:Long,
                  maxAttemptsBeforeStop:Int,
                  tryWeight2WhenNoMoveFound:Boolean) extends WeightCorrectionStrategy {

  var remainingConsecutiveIterationWithConstraintTrueBeforeStrong: Long = nbConsecutiveIterationWithConstraintTrueBeforeStrong
  var remainingConsecutiveFailsBeforeDivByTwo: Long = consecutiveFailsBeforeDivByTwo
  var remainingFailsBeforeStop: Int = maxAttemptsBeforeStop

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

//TODO
//class DecreaseWhenExhausted extends WeightCorrectionStrategy {


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
class GuidedLocalSearch(a: Neighborhood,
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
      }else if(compositeObjValue == bestCompositeObj && baseObjValue != baseOBjAtBestCompositeObj){
        //in this case there is potentially an ambiguity on the composite vs the base
        //we destroy the best because we cannot guarantee unicity
        baseOBjAtBestCompositeObj = Long.MaxValue
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

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriterion: AcceptanceCriterion): SearchResult = {
    val initValForAdditional = additionalConstraint.value
    weightForBase = weightCorrectionStrategy.getNewWeight(found = true, weightForBase, initValForAdditional)
    getMoveNoUpdateWeight(obj, initialObj, acceptanceCriterion, initValForAdditional, maxAttemptsBeforeStop)
  }

  def weightString(weightForBase:Long):String =  weightForBase match{
    case 0L => "forget obj, focus on additional constraints"
    case 1L => "additional are strong Constraints"
    case x if x > 1L =>  s"relativeWeight:$x/${weightCorrectionStrategy.constantWeightForAdditionalConstraint}"
    case x if x < 0L => "interrupted"
  }

  def getMoveNoUpdateWeight(obj: Objective,
                            initialObj: Long,
                            acceptanceCriterion: AcceptanceCriterion,
                            initValForAdditional: Long,
                            remainingAttemptsBeforeStop: Int): SearchResult = {
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
