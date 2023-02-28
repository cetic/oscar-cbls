package oscar.cbls.lib.search.combinators

import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.profiling.{CombinatorProfiler, Profiler, SelectionProfiler}
import oscar.cbls.core.search.{AcceptanceCriterion, DoNothingNeighborhood, InstrumentedMove, Move, MoveFound, Neighborhood, NeighborhoodCombinator, NoMoveFound, SearchResult, profiling}

import scala.annotation.tailrec

/**
 * this combinator always selects the best move between the n neighborhoods
 * notice that this combinator makes more sense
 * if the neighborhoods return their best found move,
 * and not their first found move, as usually done.
 *
 * @author renaud.delandtsheer@cetic.be
 */
class BestMove(n:Neighborhood*) extends NeighborhoodCombinator(n:_*) {
  override val profiler: SelectionProfiler = new SelectionProfiler(this, List(n:_*))
  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {
    val moves = n.flatMap(_.getProfiledMove(obj, initialObj:Long, acceptanceCriteria) match {
      case NoMoveFound => None
      case m: MoveFound => Some(m)
    })

    if (moves.isEmpty) NoMoveFound
    else {
      val move = moves.minBy(_.objAfter)
      move
    }
  }

}


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
class OrElse(a: Neighborhood, b: Neighborhood) extends NeighborhoodCombinator(a, b) {
  override val profiler: CombinatorProfiler = new CombinatorProfiler(this)
  profiler.percentageEventOccurrenceProfile("ElseCallsPerMove")

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {
    a.getProfiledMove(obj, initialObj, acceptanceCriteria) match {
      case NoMoveFound =>
        a.reset()
        profiler.percentageEventOccurrencePushEvent("ElseCallsPerMove", true)
        b.getProfiledMove(obj, initialObj:Long, acceptanceCriteria)
      case x: MoveFound =>
        profiler.percentageEventOccurrencePushEvent("ElseCallsPerMove",false)
        x
    }
  }
}


/**
 * this combinator bounds the number of moves done with this neighborhood
 * notice that the count is reset by the reset operation
 *
 * @author renaud.delandtsheer@cetic.be
 */
class MaxMoves(a: Neighborhood, val maxMove: Int, cond: Option[Move => Boolean] = None) extends NeighborhoodCombinator(a) {
  var remainingMoves: Int = maxMove
  override val profiler: CombinatorProfiler = new CombinatorProfiler(this)
  profiler.nbOccurrencePerIterationProfile("MaxMove reached",true)
  override def getMove(obj: Objective,
                       initialObj:Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {
    if (remainingMoves > 0) {
      a.getProfiledMove(obj, initialObj, acceptanceCriteria) match {
        case m: MoveFound => InstrumentedMove(m.m, () => notifyMoveTaken(m.m))
        case x => x
      }
    } else {
      if (verbose >= 1)
        println(s"MaxMoves: reached ${if (maxMove == 1L) "1 move " else s"$maxMove moves"}")
      profiler.nbOccurrencePerIterationEventOccurred("MaxMove reached")
      NoMoveFound
    }
  }

  //this resets the internal state of the move combinators
  override def reset(): Unit = {
    remainingMoves = maxMove
    profiler.nbOccurrencePerIterationNextIteration("MaxMove reached")
    super.reset()
  }

  def notifyMoveTaken(m: Move): Unit = {
    val shouldMoveBeConsidered = cond match{
      case None => true
      case Some(c) => c(m)}

    if (shouldMoveBeConsidered) remainingMoves -= 1
  }

  /**
   * this will modify the effect of the maxMoves by transforming it into a [[MaxMovesWithoutImprovement]]
   * the initial maxMoves is deleted by this method, and the integer bound is passed to [[MaxMovesWithoutImprovement]]
   */
  def withoutImprovementOver(obj: () => Long) = new MaxMovesWithoutImprovement(a, cond, maxMove, obj)

  def suchThat(cond: Move => Boolean) = new MaxMoves(a, maxMove, this.cond match{
    case None => Some(cond)
    case Some(oldCond) => Some((m: Move) => oldCond(m) && cond(m))})
}


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
class MaxMovesWithoutImprovement(a: Neighborhood,
                                 val cond: Option[Move => Boolean],
                                 val maxMovesWithoutImprovement: Int,
                                 obj: () => Long,
                                 countBeforeMove:Boolean = false,
                                 val minMoves:Int = 1)
  extends NeighborhoodCombinator(a) {

  var totalSteps = 0
  var stepsSinceLastImprovement = 0
  var bestObj = Long.MaxValue


  override val profiler: CombinatorProfiler = new CombinatorProfiler(this)
  profiler.minMeanMaxProfile("MaxMove without improvement reached")
  profiler.minMeanMaxProfile("Max moves before improvement")

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {
    if (countBeforeMove) {
      val startObj = obj()
      if (startObj < bestObj) {
        bestObj = startObj
        profiler.minMeanMaxAddValue("Max moves before improvement", stepsSinceLastImprovement)
        stepsSinceLastImprovement = 0
      } else {
        stepsSinceLastImprovement += 1
      }

      totalSteps += 1
      if (totalSteps <= minMoves || stepsSinceLastImprovement < maxMovesWithoutImprovement) {
        //We can go on
        a.getProfiledMove(obj, initialObj:Long, acceptanceCriteria) match {
          case m: MoveFound => m
          case NoMoveFound =>
            stepsSinceLastImprovement = 0
            NoMoveFound
        }
      } else {
        if (verbose >= 1L) println(s"MaxStepsWithoutImprovement: reached $maxMovesWithoutImprovement moves without improvement of $a")
        profiler.minMeanMaxAddValue("MaxMove without improvement reached", 1)
        NoMoveFound
      }
    } else{ //count after move
      if (totalSteps <= minMoves || stepsSinceLastImprovement < maxMovesWithoutImprovement) {
        //we can go on
        a.getProfiledMove(obj, initialObj,acceptanceCriteria) match {
          case m: MoveFound => InstrumentedMove(m.m, afterMove = () => notifyMoveTaken(m.m))
          case x => x
        }
      } else{
        if (verbose >= 1L) println(s"MaxStepsWithoutImprovement: reached $maxMovesWithoutImprovement moves without improvement of $a")
        profiler.minMeanMaxAddValue("MaxMove without improvement reached", 1)
        NoMoveFound
      }
    }
  }

  //this resets the internal state of the move combinators
  override def reset(): Unit = {
    stepsSinceLastImprovement = 0
    totalSteps = 0
    bestObj = Long.MaxValue
    super.reset()
  }

  def notifyMoveTaken(m: Move): Unit = {
    val shouldMoveBeConsidered = cond match{
      case None => true
      case Some(c) => c(m)}

    if (shouldMoveBeConsidered) {
      val newObj = obj()
      totalSteps += 1
      if (newObj < bestObj) {
        bestObj = newObj
        profiler.minMeanMaxAddValue("Max moves before improvement", stepsSinceLastImprovement)
        stepsSinceLastImprovement = 0
      } else {
        stepsSinceLastImprovement += 1
      }
    }
  }

  def withMinimalMoves(minMoves:Int) = new MaxMovesWithoutImprovement(a,cond, maxMovesWithoutImprovement, obj, countBeforeMove, minMoves)
  def improvementBeingMeasuredBeforeNeighborhoodExploration = new MaxMovesWithoutImprovement(a, null, maxMovesWithoutImprovement, obj, true)
}

/**
 * this combinator is stateless, it checks the condition on every invocation. If the condition is false,
 * it does not try the Neighborhood and finds no move.
 *
 * @author renaud.delandtsheer@cetic.be
 */
case class Guard(cond: () => Boolean, b: Neighborhood) extends NeighborhoodCombinator(b) {
  override val profiler: CombinatorProfiler = new CombinatorProfiler(this)
  profiler.summedValueProfile("Blocked")
  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {
    if (cond()) {
      b.getProfiledMove(obj, initialObj, acceptanceCriteria) match {
        case NoMoveFound => NoMoveFound
        case x: MoveFound => x
      }
    }
    else {
      profiler.summedValuePlus("Blocked",1)
      NoMoveFound
    }
  }
}

/**
 * This combinator will prevent exploring the neighborhood
 * in case the neighborhood did fail on previous exploration,
 * and the condition has not changed since then.
 * This combinator only evaluates the condition after a NoMoveFond, and before any exploration consecutive to a NoMoveFound.
 * nevertheless,the condition should be reasonably fast to evaluate
 *
 * @param n the basic neighborhood
 * @param condition a condition to test before exploring the neighborhood
 * @tparam T the return type of the condition; must be comparable to itself
 * @return
 */
case class GuardOnValueUpdate[T](n:Neighborhood,condition:() => T) extends NeighborhoodCombinator(n){
  var previousCondition:Option[T] = None

  override def reset(): Unit = {
    previousCondition = None
    super.reset()
  }

  override def getMove(obj: Objective, initialObj: Long, acceptanceCriterion: AcceptanceCriterion): SearchResult = {
    previousCondition match{
      case None =>
        n.getProfiledMove(obj, initialObj, acceptanceCriterion) match{
          case NoMoveFound =>
            previousCondition = Some(condition())
            NoMoveFound
          case m:MoveFound => m
        }
      case Some(cond) =>
        val newCondition = condition()
        if (cond == newCondition) NoMoveFound
        else n.getProfiledMove(obj, initialObj, acceptanceCriterion) match{
          case NoMoveFound =>
            previousCondition = Some(condition())
            NoMoveFound
          case m:MoveFound =>
            previousCondition = None
            m
        }
    }
  }
}

object ExhaustList{
  def apply(neighborhoods: Iterable[Neighborhood],
            neighborhoodName: String = "ExhaustList",
            backOnExhaust: Boolean = false): Neighborhood = {
    def recur(l: List[Neighborhood]): Neighborhood = {
      l match {
        case h :: Nil => h
        case h :: t => new Exhaust(h, recur(t))
        case Nil => DoNothingNeighborhood()
      }
    }
    if (backOnExhaust){
      recur(neighborhoods.toList).retry() name neighborhoodName
    } else recur(neighborhoods.toList) name neighborhoodName
  }
}

/**
 * this combinator is stateful.
 * it returns the result of the first Neighborhood until it returns NoMoveFound.
 * It then switches to the other Neighborhood.
 * it does not come back to the first one after the second one is exhausted
 *
 * @author renaud.delandtsheer@cetic.be
 */
class Exhaust(a: Neighborhood, b: Neighborhood) extends NeighborhoodCombinator(a, b) {
  var currentIsA = true
  var start: Long = 0L
  override val profiler: CombinatorProfiler = new CombinatorProfiler(this)
  profiler.minMeanMaxProfile("ExhaustTime (ms)")
  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {
    if(start == 0L) start = System.currentTimeMillis()
    @tailrec
    def search(): SearchResult = {
      val current = if (currentIsA) a else b
      current.getProfiledMove(obj, initialObj, acceptanceCriteria) match {
        case NoMoveFound => if (currentIsA) {
          currentIsA = false;
          profiler.minMeanMaxAddValue("ExhaustTime (ms)",System.currentTimeMillis()-start)
          search()
        } else NoMoveFound
        case x: MoveFound => x
      }
    }
    search()
  }

  //this resets the internal state of the move combinators
  override def reset(): Unit = {
    currentIsA = true
    start = 0L
    super.reset()
  }
}

/**
 * This combinator finds no move starting from the point where cond evaluates to false,
 * otherwise, it forwards the search request to "a"
 * this combinator is reset on reset
 *
 * @param a a neighborhood
 * @param cond a stop criterion
 * @author renaud.delandtsheer@cetic.be
 */
case class StopWhen(a: Neighborhood, cond: () => Boolean) extends NeighborhoodCombinator(a) {
  var isStopped: Boolean = false
  var start: Long = 0L
  override val profiler: CombinatorProfiler = new CombinatorProfiler(this)
  profiler.minMeanMaxProfile("StoppedAfter")

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {
    if(start == 0L) start = System.currentTimeMillis()
    if (isStopped || cond()) {
      if(!isStopped) profiler.minMeanMaxAddValue("StoppedAfter",System.currentTimeMillis()-start)
      isStopped = true;
      NoMoveFound
    }
    else a.getProfiledMove(obj, initialObj, acceptanceCriteria)
  }

  //this resets the internal state of the move combinators
  override def reset(): Unit = {
    isStopped = false
    start = 0L
    super.reset()
  }
}

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
class UntilImprovement(a: Neighborhood, over: () => Long, val minMoves: Long = 0L, val maxMove: Long = Long.MaxValue)
  extends NeighborhoodCombinator(a) {

  //TODO: pas sûr que cela fonctionne du premier coup; peut-être faut-il faire un reset au début de toute descente.
  var oldObjOnReset = over()
  var movesQueriedSinceReset = 0L

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {
    movesQueriedSinceReset += 1L
    if (movesQueriedSinceReset < maxMove
      && (movesQueriedSinceReset < minMoves || over() >= oldObjOnReset))
      a.getMove(obj, initialObj:Long, acceptanceCriteria)
    else NoMoveFound
  }

  //this resets the internal state of the move combinators
  override def reset(): Unit = {
    oldObjOnReset = over()
    movesQueriedSinceReset = 0L
    super.reset()
  }
}

/**
 * this combinator bounds the number of time the search is actually performed
 *
 * @author renaud.delandtsheer@cetic.be
 */
class MaxSearches(a: Neighborhood, val maxMove: Long) extends NeighborhoodCombinator(a) {
  override val profiler: CombinatorProfiler = new CombinatorProfiler(this)
  profiler.nbOccurrencePerIterationEventOccurred("MaxMovesReachedBeforeReset")
  var remainingMoves = maxMove

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {
    if (remainingMoves > 0L) {
      remainingMoves -= 1L
      a.getProfiledMove(obj, initialObj, acceptanceCriteria)
    } else {
      profiler.nbOccurrencePerIterationEventOccurred("MaxMovesReachedBeforeReset")
      NoMoveFound
    }
  }

  //this resets the internal state of the move combinators
  override def reset(): Unit = {
    profiler.nbOccurrencePerIterationNextIteration("MaxMovesReachedBeforeReset")
    remainingMoves = maxMove
    super.reset()
  }
}

/**
 * this combinator is stateful.
 * it returns the result of one Neighborhood until it returns NoMoveFound.
 * It then switches to the other Neighborhood.
 * it starts with Neighborhood a
 *
 * @author renaud.delandtsheer@cetic.be
 */
class ExhaustBack(a: Neighborhood, b: Neighborhood) extends NeighborhoodCombinator(a, b) {
  var currentIsA = true
  override val profiler: CombinatorProfiler = new CombinatorProfiler(this)
  profiler.minMeanMaxProfile("ExhaustTime (ms)")
  profiler.summedValueProfile("nbBacks")
  var start: Long = 0L

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {
    if (start == 0L) start = System.currentTimeMillis()

      val current = if (currentIsA) a else b
      current.getProfiledMove(obj, initialObj, acceptanceCriteria) match {
        case NoMoveFound =>
          if (currentIsA) {
            profiler.minMeanMaxAddValue("ExhaustTime (ms)",System.currentTimeMillis()-start)
            currentIsA = false
            b.reset()
            b.getProfiledMove(obj, initialObj:Long, acceptanceCriteria) match {
              case NoMoveFound => NoMoveFound
              case x: MoveFound => x
            }
          } else {
            currentIsA = true
            profiler.summedValuePlus("nbBacks",1)
            start = 0L
            a.reset()
            a.getProfiledMove(obj, initialObj, acceptanceCriteria) match {
              case NoMoveFound => NoMoveFound
              case x: MoveFound => x
            }
          }
        case x: MoveFound => x
      }

  }

  //this resets the internal state of the move combinators
  override def reset(): Unit = {
    currentIsA = true
    start = 0L
    super.reset()
  }
}

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
class Retry(a: Neighborhood, cond: Long => Boolean = _ <= 1L) extends NeighborhoodCombinator(a) {
  override val profiler: CombinatorProfiler = new CombinatorProfiler(this)
  profiler.nbOccurrencePerIterationNextIteration("Consecutive fail before reset")
  var consecutiveFails = 0L

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {
    a.getProfiledMove(obj, initialObj, acceptanceCriteria) match {
      case NoMoveFound =>
        profiler.nbOccurrencePerIterationEventOccurred("Consecutive fail before reset")
        consecutiveFails = consecutiveFails + 1L
        if (cond(consecutiveFails)) {
          a.reset()
          getMove(obj, initialObj, acceptanceCriteria)
        } else {
          NoMoveFound
        }
      case x =>
        consecutiveFails = 0L
        x
    }
  }

  //this resets the internal state of the move combinators
  override def reset(): Unit = {
    super.reset()
    profiler.nbOccurrencePerIterationNextIteration("Consecutive fail before reset")
    consecutiveFails = 0L
  }
}

/**
  * instantiates a new neighborhood on each exploration.
  * You can use it to perform some queries before instantiating the neighborhood.
  * You can return [[oscar.cbls.core.search.NoMoveNeighborhood]] if there is no actual neighborhood to explore
  * @param f a function that generated the neighborhood to explore
  */
class Dyn(f:() => Neighborhood,name : String = "Dyn()") extends Neighborhood(name) {
  override val profiler: Profiler = new Profiler(this)

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {
    val neighborhood = f()
    neighborhood.verbose = this.verbose
    neighborhood.getProfiledMove(obj, initialObj, acceptanceCriteria)
  }
}
