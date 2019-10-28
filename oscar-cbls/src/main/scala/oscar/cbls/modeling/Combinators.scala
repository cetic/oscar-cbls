package oscar.cbls.modeling

import oscar.cbls._
import oscar.cbls.core.search.{JumpNeighborhood, Move, Neighborhood, NoMoveNeighborhood, SupportForAndThenChaining}
import oscar.cbls.lib.search.combinators._
import oscar.cbls.util.StopWatch

import scala.language.postfixOps

trait CombinatorsAPI
  extends BasicCombinators
    with CompositionCombinators
    with InstrumentNeighborhoodsCombinator
    with MetaheuristicCombinators
    with NeighborhoodSelectionCombinators
    with UtilityCombinators


trait BasicCombinators{

  /**
    * this combinator always selects the best move between the two parameters
    * notice that this combinator makes more sense
    * if the two neighborhood return their best found move,
    * and not their first found move, as usually done.
    *
    * @author renaud.delandtsheer@cetic.be
    */
  def best(a: Neighborhood, b: Neighborhood) = new Best(a, b)

  /**
    * this combinator sequentially tries all neighborhoods until one move is found
    * between calls, it will roll back to the first neighborhood
    * it tries a first, and if no move it found, tries b
    * a is reset if it did not find anything.
    *
    * @param a a neighborhood
    * @param b another neighborhood
    * @author renaud.delandtsheer@cetic.be
    */
  def orElse(a: Neighborhood, b: Neighborhood) = new OrElse(a: Neighborhood, b: Neighborhood)

  /**
    * this combinator bounds the number of moves done with this neighborhood
    * notice that the count is reset by the reset operation
    *
    * @author renaud.delandtsheer@cetic.be
    */
  def maxMoves(a: Neighborhood, maxMove: Long, cond: Option[Move => Boolean] = None) = new MaxMoves(a, maxMove, cond)

  /**
    * bounds the number of tolerated moves without improvements over the best value
    * the count is reset by the reset action.
    * @author renaud.delandtsheer@cetic.be
    * @param a a neighborhood
    * @param cond an optional condition that specifies if the move should be taken into account into this stop criterion
    * @param maxMovesWithoutImprovement the maximal number of moves (accepted by cond) that have no improvememnt over the best obj. if more moves are searched, it returns NoMoveFound
    * @param obj the objective function that is watched for improvement by this combinator
    * @param countBeforeMove true if the count should be done before the move, false otherwise.
    */
  def maxMovesWithoutImprovement(a: Neighborhood,
                                 cond: Option[Move => Boolean],
                                 maxMovesWithoutImprovement: Long,
                                 obj: () => Long,
                                 countBeforeMove:Boolean = false) =
    new MaxMovesWithoutImprovement(a,
      cond,
      maxMovesWithoutImprovement,
      obj,
      countBeforeMove)

  /**
    * this combinator is stateless, it checks the condition on every invocation. If the condition is false,
    * it does not try the Neighborhood and finds no move.
    *
    * @author renaud.delandtsheer@cetic.be
    */
  def guard(cond: () => Boolean, b: Neighborhood) = Guard(cond, b)

  /**
    * this combinator is stateful.
    * it returns the result of the first Neighborhood until it returns NoMoveFound.
    * It then switches to the other Neighborhood.
    * it does not come back to the first one after the second one is exhausted
    *
    * @author renaud.delandtsheer@cetic.be
    */
  def exhaust(a: Neighborhood, b: Neighborhood) = new Exhaust(a, b)

  /**
    * this combinator is stateful.
    * it returns the result of one Neighborhood until it returns NoMoveFound.
    * It then switches to the other Neighborhood.
    * it starts with Neighborhood a
    *
    * @author renaud.delandtsheer@cetic.be
    */
  def exhaustBack(a: Neighborhood, b: Neighborhood) = new ExhaustBack(a, b)

  /**
    * This combinator finds no move starting from the point where cond evaluates to false,
    * otherwise, it forwards the search request to "a"
    * this combinator is reset on reset
    *
    * @param a a neighborhood
    * @param cond a stop criterion
    * @author renaud.delandtsheer@cetic.be
    */
  def stopWhen(a: Neighborhood, cond: () => Boolean)  = StopWhen(a: Neighborhood, cond: () => Boolean)

  /**
    * calls the neighborhood until an improvement over obj is achieved
    * the improvement is "since the last reset"
    *
    * @param a
    * @param minMoves the min number of queries that will be forwarded to a (priority over the improvement)
    * @param maxMove the max number of queries that will be forwarded to a (priority over the improvement)
    * @param over the obj that is looked for improvement
    * @author renaud.delandtsheer@cetic.be
    */
  def untilImprovement(a: Neighborhood, over: () => Long, minMoves: Long = 0L, maxMove: Long = Long.MaxValue) =
    new UntilImprovement(a, over, minMoves, maxMove)

  /**
    * this combinator bounds the number of time the search is actually performed
    *
    * @author renaud.delandtsheer@cetic.be
    */
  def maxSearches(a: Neighborhood,  maxMove: Long) = new MaxSearches(a, maxMove)

  /**
    * once given condition has turned true,
    * retries n times the move before concluding to noMove can be found
    * resets on the first found move, or on reset
    *
    * @param a the neighborhood on which we will perform retries
    * @param cond condition that takes the number of consecutive NoMoveFound, and says if we should try again returns true if yes, false otherwise
    * @author renaud.delandtsheer@cetic.be
    * @author yoann.guyot@cetic.be
    */
  def retry(a: Neighborhood, cond: Long => Boolean = _ <= 1L) = new Retry(a, cond)


  /**
    * instantiates a new neighborhood on each exploration.
    * You can use it to perform some queries before instantiating the neighborhood.
    * You can return [[NoMoveNeighborhood]] if tehre is no actul neighborhood to explore
    * @param f a function that generated the neighborhood to explore
    */
  def dyn(f:() => Neighborhood) = new Dyn(f)
}


trait CompositionCombinators{

  /**
    * to build a composite neighborhood.
    * the first neighborhood is used only to provide a round robin exploration on its possible moves
    * you must ensure that this first neighborhood will perform a hotRestart, so that it will enumerate all its moves
    * internally, this neighborhood will be called with a fully acceptant acceptanceCriteria,
    *
    * the move combinator for every move provided by the first neighborhood, the combinator calls the second one
    * and we consider the composition of the two moves for the acceptance criteria.
    * the returned move is the composition of the two found moves
    *
    * you must also ensure that the two neighborhood evaluate the same objective function,
    * since this combinator needs to evaluate the whole composite move, and not only the last part of the composition
    *
    * A native composite neighborhood will probably be much faster than this combinator, so use this for prototyping
    * for instance, this combinator does not allow for some form of symmetry breaking, unless you are really doing it the hard way.
    *
    * this move will reset the first neighborhood on every call, since it is probably bounded by the number of moves it can provide
    *
    * @param a the first neighborhood, all moves delivered by this one will be considered
    * @param b given that the move returned by the first neighborhood is committed, we explore the globally improving moves of this one
    * @param maximalIntermediaryDegradation the maximal degradation that is admitted for the intermediary step; the higher, the more moves will be considered
    * @author renaud.delandtsheer@cetic.be
    */
  def andThen[FirstMoveType<:Move](a: Neighborhood with SupportForAndThenChaining[FirstMoveType],
                                   b: Neighborhood,
                                   maximalIntermediaryDegradation: Long = Long.MaxValue) =
    new AndThen[FirstMoveType](a,b,maximalIntermediaryDegradation)

  val mu = Mu

  /**
    * to build a composite neighborhood.
    * the first neighborhood is used only to provide a round robin exploration on its possible moves
    * you must ensure that this first neighborhood will perform a hotRestart, so that it will enumerate all its moves
    * internally, this neighborhood will be called with a fully acceptant acceptanceCriteria,
    *
    * the move combinator for every move provided by the first neighborhood, the combinator calls the second one
    * and we consider the composition of the two moves for the acceptance criteria.
    * the returned move is the composition of the two found moves
    *
    * you must also ensure that the two neighborhood evaluate the same objective function,
    * since this combinator needs to evaluate the whole composite move, and not only the last part of the composition
    *
    * A native composite neighborhood will probably be much faster than this combinator, so use this for prototyping
    * for instance, this combinator does not allow for some form of symmetry breaking, unless you are really doing it the hard way.
    *
    * this move will reset the first neighborhood on every call, since it is probably bounded by the number of moves it can provide
    *
    * @param a the first neighborhood, all moves delivered by this one will be considered
    * @param b given that the move returned by the first neighborhood is committed, we explore the globally improving moves of this one
    *          you pass a method to instantiate b, based on,the currently explored move from a
    * @param maximalIntermediaryDegradation the maximal degradation that is admitted for the intermediary step; the higher, the more moves will be considered
    * @author renaud.delandtsheer@cetic.be
    */
  def dynAndThen[FirstMoveType<:Move](a:Neighborhood with SupportForAndThenChaining[FirstMoveType],
                                      b:(FirstMoveType => Neighborhood),
                                      maximalIntermediaryDegradation: Long = Long.MaxValue) =
    new DynAndThen[FirstMoveType](a,b,maximalIntermediaryDegradation)


  def DynAndThenWithPrev[FirstMoveType<:Move](x:Neighborhood with SupportForAndThenChaining[FirstMoveType],
                                              b:((FirstMoveType,Snapshot) => Neighborhood),
                                              maximalIntermediaryDegradation:Long = Long.MaxValue,
                                              valuesToSave:Iterable[AbstractVariable]) =
    new DynAndThenWithPrev[FirstMoveType](x, b, maximalIntermediaryDegradation, valuesToSave)

  /**
    * This is an atomic combinator, it represent that the neighborhood below should be considered as a single piece.
    * When you commit a move from this neighborhood, "a" is reset, and exhausted in a single move from Atomic(a)
    * Also, Atomic is a jump neighborhood as it cannot evaluate any objective function before the move is committed.
    *
    * @param a
    */
  def atomic(a: Neighborhood, shouldStop: Int => Boolean = _ => false, stopAsSoonAsAcceptableMoves:Boolean = false) =
    Atomic(a, shouldStop,stopAsSoonAsAcceptableMoves)
}


trait InstrumentNeighborhoodsCombinator{
  /**
    * this combinator attaches a custom code to a given neighborhood.
    * the code is called whenever a move is asked to the neighborhood.
    *
    * @param a a neighborhood
    * @param proc the procedure to execute before the neighborhood is queried
    */
  def doOnQuery(a: Neighborhood, proc: () => Unit) = DoOnQuery(a, proc)

  /**
    * this combinator attaches a custom code to a given neighborhood.
    * the code is called whenever a move from this neighborhood is taken for the first time.
    * notice that this neighborhood is reset, so first time can occur several times.
    *
    * @param a a neighborhood
    * @param proc the procedure to call on one first move that is performed from this neighborhood
    */
  def doOnFirstMove(a: Neighborhood, proc: () => Unit) = new DoOnFirstMove(a: Neighborhood, proc: () => Unit)

  /**
    * this combinator attaches a custom code to a given neighborhood.
    * the code is called whenever a move from this neighborhood is taken
    * The callBack is performed before the move is actually taken.
    *
    * @param a a neighborhood
    * @param procBeforeMove the procedure to execute when the move is taken, , with the move as input parameter
    *                   use this to update a Tabu for instance
    * @param procAfterMove a procedure to execute after the move is taken, with the move as input parameter
    */
  def doOnMove(a: Neighborhood,
               procBeforeMove: Move => Unit = null,
               procAfterMove: Move => Unit = null) =
    DoOnMove(a: Neighborhood,
      procBeforeMove,
      procAfterMove)

  def doOnExhaust(a:Neighborhood, proc:(()=>Unit),onlyFirst:Boolean) =
    DoOnExhaust(a, proc,onlyFirst)
}


trait MetaheuristicCombinators{
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
  def restart(n:Neighborhood,randomizationNeighborhood:Neighborhood, maxRestartWithoutImprovement:Long, obj:Objective) =
    Restart(n,randomizationNeighborhood, maxRestartWithoutImprovement, obj)

}

trait NeighborhoodSelectionCombinators{
  /**
    * At each invocation, this combinator explores one of the neighborhoods in l (and repeat if it is exhausted)
    * neighborhoods are selected based on their slope. the slope is the total gain in objective function performed by the neighborhood, divided by the total amount of time spend exploring the neighborhood.
    * a tabu is added: in case a neighborhood is exhausted, it is not explored for a number of exploration of this combinator
    * the tabu can be overriden if all neighborhoods explored are exhausted. tabu neighborhood can be explored anyway if they are still tabu, but for less than overrideTabuOnFullExhaust invocations of this combinator
    * the refresh parameter forces the combinator to try all neighborhoods every "refresh" invocation. it is useful because some neighorhood can perform poorly at the beginning of search and much better later on, and we do not want the combinator to just "stick to its first impression"
    * @param l the neighborhoods to select from
    * @param tabuLength the number of invocation that they will not be explored when exhausted
    * @param overrideTabuOnFullExhaust the tabu can be overriden if all explored neighbors are exhausted, for each neighborhood that is tabu for les than this override
    * @param refresh a refresh of the slopee measuring must be perfored every refresh iterations
    */
  def bestSlopeFirst(l:List[Neighborhood],
                     tabuLength:Long = 10L,
                     overrideTabuOnFullExhaust:Long = 9L, refresh:Long = 100L) =
    BestSlopeFirst(l,tabuLength,overrideTabuOnFullExhaust,refresh)


  /**
    * At each invocation, this combinator explores one of the neighborhoods in l (and repeat if it is exhausted)
    * neighborhoods are selected based on their speed the fasted one to find a move is selected
    * a tabu is added: in case a neighborhood is exhausted, it is not explored for a number of exploration of this combinator
    * the tabu can be overriden if all neighborhoods explored are exhausted. tabu neighborhood can be explored anyway if they are still tabu, but for less than overrideTabuOnFullExhaust invocations of this combinator
    * the refresh parameter forces the combinator to try all neighborhoods every "refresh" invocation. it is useful because some neighorhood can perform poorly at the beginning of search and much better later on, and we do not want the combinator to just "stick to its first impression"
    * @param l the neighborhoods to select from
    * @param tabuLength the number of invocation that they will not be explored when exhausted
    * @param overrideTabuOnFullExhaust the tabu can be overriden if all explored neighbors are exhausted, for each neighborhood that is tabu for les than this override
    * @param refresh a refresh of the slopee measuring must be perfored every refresh iterations
    */
  def fastestFirst(l:List[Neighborhood],
                   tabuLength:Long = 10L,
                   overrideTabuOnFullExhaust:Long = 9L,  refresh:Long = 100L) =
    FastestFirst(l,tabuLength,overrideTabuOnFullExhaust,refresh)

  /**
    * performs a round robin on the neighborhood.
    * It proceeds to the next one after "step" invocations or if the explored one is exhausted
    *
    * @author renaud.delandtsheer@cetic.be
    */
  def roundRobin(l: List[Neighborhood], steps: Long = 1L) = new RoundRobin(l, steps)

  /**
    * this combinator randomly tries one neighborhood.
    * it tries the other if the first did not find any move
    *
    * @param a a neighborhood
    * @author renaud.delandtsheer@cetic.be
    */
  def random(a: Neighborhood*) = new RandomCombinator(a:_*)

  /**
    * this combinator randomly tries one neighborhood.
    * it tries the other if the first did not find any move
    *
    * @param a a neighborhood
    * @author renaud.delandtsheer@cetic.be
    */
  def biasedRandom(a: (Neighborhood,Double)*)(noRetryOnExhaust:Boolean = false) =
    new BiasedRandom(a:_*)(noRetryOnExhaust)

  /**
    * this combinator is stateful.
    * it returns the result of the first Neighborhood until it returns NoMoveFound.
    * It then switches to the other Neighborhood,
    * but only if a move was found by the first neighborhood
    * it does not come back to the first one after the second one is exhausted
    *
    * @author renaud.delandtsheer@cetic.be
    */
  def exhaustAndContinueIfMovesFound(a: Neighborhood, b: Neighborhood) =
    new ExhaustAndContinueIfMovesFound(a, b)
}


class NeighborhoodOps(n:Neighborhood){

  /**
    * this combinator sequentially tries all neighborhoods until one move is found
    * between calls, it will roll back to the first neighborhood
    * it tries a first, and if no move it found, tries b
    * a is reset if it did not find anything.
    *
    * @param b another neighborhood
    * @author renaud.delandtsheer@cetic.be
    */
  def orElse(b: Neighborhood): Neighborhood = new OrElse(n, b)

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
  def onExhaustRestartAfter(randomizationNeighborhood:Neighborhood, maxRestartWithoutImprovement:Long, obj:Objective, restartFromBest:Boolean = false) = {
    Restart(n,randomizationNeighborhood,maxRestartWithoutImprovement,obj,restartFromBest)
  }

  def onExhaustRestartAfterJump(randomizationProcedure: =>Unit, maxRestartWithoutImprovement:Long, obj:Objective, restartFromBest:Boolean = false, randomizationName:String = "Randomization") = {
    val jumpNeighborhood = new JumpNeighborhood(randomizationName){
      override def doIt(): Unit = randomizationProcedure
    }
    onExhaustRestartAfter(jumpNeighborhood, maxRestartWithoutImprovement, obj, restartFromBest)
  }

  /**
    * alias for (this maxMoves 1L) exhaust b
    * this wil be queried once, and then, queries will be forwarded to b.
    *
    * @param b
    * @return
    */
  def sequence(b: Neighborhood): Neighborhood = n maxMoves 1L exhaust b

  /**
    * this combinator always selects the best move between the two parameters
    * notice that this combinator makes more sense
    * if the two neighborhood return their best found move,
    * and not their first found move, as usually done.
    *
    * @author renaud.delandtsheer@cetic.be
    */
  def best(b: Neighborhood): Neighborhood = new oscar.cbls.lib.search.combinators.Best(n, b)

  /**
    * this combinator is stateful.
    * it returns the result of the first Neighborhood until it returns NoMoveFound.
    * It then switches to the other Neighborhood.
    * it does not come back to the first one after the second one is exhausted
    *
    * @author renaud.delandtsheer@cetic.be
    */
  def exhaust(b: Neighborhood): Neighborhood = new Exhaust(n, b)

  /**
    * this combinator is stateful.
    * it returns the result of one Neighborhood until it returns NoMoveFound.
    * It then switches to the other Neighborhood.
    * it starts with Neighborhood a
    *
    * @author renaud.delandtsheer@cetic.be
    */
  def exhaustBack(b: Neighborhood): Neighborhood = new ExhaustBack(n, b)

  /**
    * this combinator is stateful.
    * it returns the result of the first Neighborhood until it returns NoMoveFound.
    * It then switches to the other Neighborhood,
    * but only if a move was found by the first neighborhood
    * it does not come back to the first one after the second one is exhausted
    *
    * @author renaud.delandtsheer@cetic.be
    */
  def exhaustAndContinueIfMovesFound(b: Neighborhood) = new ExhaustAndContinueIfMovesFound(n, b)


  /**
    * this one bounds the number of time the search is actually performed
    * notice that the count is reset by the reset operation
    *
    * @author renaud.delandtsheer@cetic.be
    */
  def maxSearches(maxMove: Long) = new MaxSearches(n, maxMove)

  /**
    * this one bounds the number of moves done with this neighborhood
    * notice that the count is reset by the reset operation
    *
    * @author renaud.delandtsheer@cetic.be
    */
  def maxMoves(maxMove: Long) = new MaxMoves(n, maxMove)

  /**
    * no move if cond evaluates to false, otherwise ,it forwards the search request to a
    *
    * @param cond a stop criterion
    */
  def stopWhen(cond: () => Boolean) = StopWhen(n, cond)

  /**
    * if it is false,it returns NoMovesFound without exploring the neighborhood at all.
    *
    * @param cond
    * @return
    */
  def guard(cond:()=>Boolean) = Guard(cond,n)

  /**
    * this is an alias for maxMoves 1L
    */
  def once = new MaxMoves(n, 1L)

  def onExhaust(proc: =>Unit) = DoOnExhaust(n,() => proc,false)

  def onFirstExhaust(proc: =>Unit) = DoOnExhaust(n,() => proc,true)

  /**
    * bounds the number of tolerated moves without improvements over the best value
    * the count is reset by the reset action.
    *
    * @author renaud.delandtsheer@cetic.be
    */
  def maxMovesWithoutImprovement(maxMove: Long, obj: () => Long) = new MaxMovesWithoutImprovement(n, null, maxMove, obj)

  /**
    * makes a round robin on the neighborhood. it swaps as soon as one does not find a move
    * and swaps neighborhood after "step" invocations
    *
    * @author renaud.delandtsheer@cetic.be
    */
  def roundRobin(b: Neighborhood): RoundRobinNoParam = new RoundRobinNoParam(n, b)

  /**
    * this combinator attaches a custom code to a given neighborhood.
    * the code is called whenever a move is asked to the neighborhood.
    *
    * @param proc the procedure to execute before the neighborhood is queried
    */
  def onQuery(proc: => Unit) = DoOnQuery(n, () => proc)

  /**
    * this combinator attaches a custom code to a given neighborhood.
    * the code is called whenever a move from this neighborhood is taken
    * The callBack is performed before the move is actually taken.
    *
    * @param proc the procedure to execute when the move is taken
    */
  def beforeMove(proc: => Unit) = DoOnMove(n, procBeforeMove = (_) => proc)

  /**
    * this combinator attaches a custom code to a given neighborhood.
    * the code is called whenever a move from this neighborhood is taken
    * is gets the applied move in input.
    * The callBack is performed before the move is actually taken.
    *
    * @param procOnMove a procedure that inputs the move that is applied;
    *                   use this to update a Tabu for instance
    */
  def beforeMove(procOnMove: Move => Unit) = DoOnMove(n, procBeforeMove = procOnMove)

  /**
    * this combinator attaches a custom code to a given neighborhood.
    * the code is called whenever a move from this neighborhood is taken
    * The callBack is performed after the move is actually taken.
    *
    * @param proc the procedure to execute when the move is taken
    */
  def afterMove(proc: => Unit) = DoOnMove(n, procAfterMove = (_) => proc)

  /**
    * this combinator attaches a custom code to a given neighborhood.
    * the code is called whenever a move from this neighborhood is taken
    * is gets the applied move in input.
    * The callBack is performed after the move is actually taken.
    *
    * @param procOnMove a procedure that inputs the move that is applied;
    *                   use this to update a Tabu for instance
    */
  def afterMoveOnMove(procOnMove: Move => Unit) = DoOnMove(n, procAfterMove = procOnMove)

  /**
    * This combinator create a frame that draw the evolution curve of the objective function.
    * The drawn curve possess a scrollbar on the right that allow the user to decrease or
    * increase the number of value displayed.
    *
    * @param obj the objective function
    * @author fabian.germeau@student.vinci.be
    */
  def showObjectiveFunction(obj: Objective, title: String = "Objective function vs. time[s]") = new ShowObjectiveFunction(n,obj, title)

  /**
    * this combinator attaches a custom code to a given neighborhood.
    * the code is called whenever a move from this neighborhood is taken for the first time.
    * notice that this neighborhood is reset, so first time can occur several times.
    *
    * @param proc the procedure to call on one first move that is performed from this neighborhood
    */
  def onFirstMove(proc: => Unit) = new DoOnFirstMove(n, () => proc)

  /**
    * saves the model for the best (smallest) value of obj
    * and restores it when the neighborhood is exhausted.
    * It does not restore it.
    * You can do so either by manually calling a restoreBest on the returned object,
    * or by adding the keyword "restoreBestOnExhaust" after this one.
    * You might also consider saveBestAndRestoreOnExhaust
    * @param o the objective function
    */
  def saveBest(o: Objective) = new SaveBest(n, o)

  /**
    * saves the model for the best (smallest) value of obj
    * and restores it when the neighborhood is exhausted.
    * to restore it, it will return a move that loads the best solution.
    * @param obj
    */
  def saveBestAndRestoreOnExhaust(obj: Objective) = new SaveBest(n, obj) restoreBestOnExhaust

  /**
    * saves the model for the best (smallest) value of obj
    * and restores it when the neighborhood is exhausted.
    * to restore it, it will return a move that loads the best solution.
    * this one also saves the solution on exhaust, so it can be used within some metaheuristics
    * @param obj
    */
  def saveBestOnExhaustAndRestoreOnExhaust(obj: Objective) = new SaveBest(n, obj, alsoSaveOnExhaust = true) restoreBestOnExhaust


  /**
    * retries the move before concluding to noMove can be found
    *
    * @param cond condition that takes the number of consecutive NoMoveFound, and says if we should try again returns true if yes, false otherwise
    *             by default, we allow a single retry.
    */
  def retry(cond: Long => Boolean = (n: Long) => n <= 1L) = new Retry(n, cond)

  /**
    * to prevent resetting the internal state of this neighborhood
    *
    * @return
    */
  def noReset: Neighborhood = NoReset(n)

  /**
    * defines a name wor this (composite) neighborhood
    * this will be used as prefix for each move returned by this neighborhood (the original name will still exist)
    * use this for debug and documentation purpose only
    *
    * @param name the name that will prefix all returned moves, used for verbosities
    * @return
    */
  def name(name: String) = new Name(n, name)

  /**
    * tis combinator overrides the acceptance criterion given to the whole neighborhood
    * this can be necessary if you have a neighborhood with some phases only including simulated annealing
    *
    * @param overridingAcceptanceCriterion the acceptance criterion that is used instead of the one given to the overall sear
    */
  def withAcceptanceCriterion(overridingAcceptanceCriterion: (Long, Long) => Boolean) = new WithAcceptanceCriterion(n, overridingAcceptanceCriterion)

  /**
    * this combinator overrides accepts all moves (this is the withAcceptanceCriteria, given the fully acceptant criterion
    */
  def acceptAll() = new WithAcceptanceCriterion(n, (_: Long, _: Long) => true)

  /**
    * proposes a round-robin with that.
    * notice that you can chain steps; this will build a round-robin on the whole sequence (although this operation is not associative, so better not use parentheses)
    *
    * @param b
    * @return
    */
  def step(b: Neighborhood) = new RoundRobin(List(n, b))

  /**
    * calls the neighborhood until an improvement over obj is achieved
    * the improvement is "since the last reset"
    *
    * @param minMoves the min number of queries that will be forwarded to a (priority over the improvement)
    * @param maxMove the max number of queries that will be forwarded to a (priority over the improvement)
    * @param obj the obj that is looked for improvement
    * @author renaud.delandtsheer@cetic.be
    */
  def untilImprovement(obj: () => Long, minMoves: Long = 0L, maxMove: Long = Long.MaxValue) = new UntilImprovement(n, obj, minMoves, maxMove)

  /**
    * this combinator injects a metropolis acceptation function.
    * the criterion accepts all improving moves, and for worsening moves, it applies the metropolis criterion:
    * accept if math.random(0.0; 1.0) < base exponent (-gain / temperatureValue)
    *
    * @param temperature a function that inputs the number of moves taken, and outputs a temperature, for use in the criterion
    *                    the number of steps is reset to zero when the combinator is reset.
    *                    By default, the temperature is 100L/the number of steps
    * @param base the base for the exponent calculation. default is 2L
    */
  def metropolis(iterationToTemperature: Long => Double = (it: Long) => 5.toDouble / (it + 1), base: Double = 2) = new Metropolis(n, iterationToTemperature, base)

  //Cauchy annealing: T = T_0/k,
  def cauchyAnnealing(initialTemperature:Double, base: Double = 2) = new Metropolis(n, iterationToTemperature = (it: Long) => initialTemperature / (it + 1), base)

  //Boltzmann annealing, where T = T_0/ln k
  def boltzmannAnnealing(initialTemperature:Double, base: Double = 2) = new Metropolis(n, iterationToTemperature = (it: Long) => initialTemperature / math.log(it + 1), base)

  //TODO: Adaptive Simulated Annealing: T = T_0 exp(-c k^1/D) wth re-annealing also permits adaptation to changing sensitivities in the multi-dimensional parameter-space.





  /**
    * sets a timeout for a search procedure.
    * notice that hte timeout itself is a bit lax, because the combinator has no possibility to interrupt a neighborhood during its exploration.
    * this combinator will therefore just prevent any new exploration past the end of the timeout.
    * @param a a neighborhood
    * @param maxDuration the maximal duration, in milliseconds
    */
  def timeout(maxDuration:Long) = new Timeout(n, maxDuration:Long)

  /**
    * This combinator will interrupt the search when it becomes too flat.
    * use it to cut the tail of long, undesired searches
    * it works by time period.
    * at the end of every time period, as set by timePeriodInMilliSecond,
    * it will compute the relative improvement of obj of this latest time period over hte best so far
    * if the relative improvement is smaller than minRelativeImprovementByCut, it is considered too flat, and search is stopped
    *
    * NOTICE that if your base neighborhood has a search time that is bigger then the time period,
    * it will not be interrupted during its exploration.
    * this combinator only decides if a new neighborhood exploration is to be started
    *
    * @param timePeriodInMilliSecond defines teh time period for the cut
    * @param minRelativeImprovementByCut the relative improvement over obj
    */
  def cutTail(timePeriodInMilliSecond:Long,minRelativeImprovementByCut:Double,minTimeBeforeFirstCutInMilliSecond:Long = 0) =
    new CutTail(n, timePeriodInMilliSecond,minRelativeImprovementByCut,minTimeBeforeFirstCutInMilliSecond)
}
