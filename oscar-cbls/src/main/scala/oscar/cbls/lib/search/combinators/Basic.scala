package oscar.cbls.lib.search.combinators

import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{InstrumentedMove, Move, MoveFound, Neighborhood, NeighborhoodCombinator, NoMoveFound, SearchResult}

import scala.annotation.tailrec

/**
 * this combinator always selects the best move between the two parameters
 * notice that this combinator makes more sense
 * if the two neighborhood return their best found move,
 * and not their first found move, as usually done.
 *
 * @author renaud.delandtsheer@cetic.be
 */
class BestMove(n:Neighborhood*) extends NeighborhoodCombinator(n:_*) {

  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {

    val moves = n.flatMap(_.getMove(obj, initialObj:Long, acceptanceCriteria) match {
      case NoMoveFound => None
      case m: MoveFound => Some(m)
    })

    if (moves.isEmpty) NoMoveFound
    else moves.minBy(_.objAfter)
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
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {
    a.getMove(obj, initialObj:Long, acceptanceCriteria) match {
      case NoMoveFound =>
        a.reset()
        b.getMove(obj, initialObj:Long, acceptanceCriteria)
      case x => x
    }
  }
}


/**
 * this combinator bounds the number of moves done with this neighborhood
 * notice that the count is reset by the reset operation
 *
 * @author renaud.delandtsheer@cetic.be
 */
class MaxMoves(a: Neighborhood, val maxMove: Long, cond: Option[Move => Boolean] = None) extends NeighborhoodCombinator(a) {
  var remainingMoves = maxMove
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {
    if (remainingMoves > 0L) {
      a.getMove(obj, initialObj:Long, acceptanceCriteria) match {
        case m: MoveFound => InstrumentedMove(m.m, () => notifyMoveTaken(m.m))
        case x => x
      }
    } else {
      if (verbose >= 1L)
        println("MaxMoves: reached " + (if (maxMove == 1L) "1 move " else maxMove + " moves"))
      NoMoveFound
    }
  }

  //this resets the internal state of the move combinators
  override def reset(): Unit = {
    remainingMoves = maxMove
    super.reset()
  }

  def notifyMoveTaken(m: Move): Unit = {
    val shouldMoveBeConsidered = cond match{
      case None => true
      case Some(c) => c(m)}

    if (shouldMoveBeConsidered) remainingMoves -= 1L
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
                                 val maxMovesWithoutImprovement: Long,
                                 obj: () => Long,
                                 countBeforeMove:Boolean = false)
  extends NeighborhoodCombinator(a) {

  var stepsSinceLastImprovement = 0L
  var bestObj = Long.MaxValue

  override def getMove(obj: Objective,initialObj:Long,  acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {
    if(countBeforeMove) {
      val startObj = obj()
      if (startObj < bestObj) {
        bestObj = startObj
        stepsSinceLastImprovement = 0L
      } else {
        stepsSinceLastImprovement += 1L
      }

      if (stepsSinceLastImprovement < maxMovesWithoutImprovement) {
        //We can go on
        a.getMove(obj, initialObj:Long, acceptanceCriteria) match {
          case m: MoveFound => m
          case NoMoveFound =>
            stepsSinceLastImprovement = 0L
            NoMoveFound
        }
      } else {
        if (verbose >= 1L) println(s"MaxStepsWithoutImprovement: reached $maxMovesWithoutImprovement moves without improvement of $a")
        NoMoveFound
      }
    } else{ //count after move
      if (stepsSinceLastImprovement < maxMovesWithoutImprovement) {
        //we can go on
        a.getMove(obj, initialObj,acceptanceCriteria) match {
          case m: MoveFound => InstrumentedMove(m.m, afterMove = () => notifyMoveTaken(m.m))
          case x => x
        }
      } else{
        if (verbose >= 1L) println(s"MaxStepsWithoutImprovement: reached $maxMovesWithoutImprovement moves without improvement of $a")
        NoMoveFound
      }
    }
  }

  //this resets the internal state of the move combinators
  override def reset(): Unit = {
    stepsSinceLastImprovement = 0L
    bestObj = Long.MaxValue
    super.reset()
  }

  def notifyMoveTaken(m: Move): Unit = {
    val shouldMoveBeConsidered = cond match{
      case None => true
      case Some(c) => c(m)}

    if (shouldMoveBeConsidered) {
      val newObj = obj()
      if (newObj < bestObj) {
        bestObj = newObj
        stepsSinceLastImprovement = 0L
      } else {
        stepsSinceLastImprovement += 1L
      }
    }
  }

  def improvementBeignMeasuredBeforeNeighborhoodExploration = new MaxMovesWithoutImprovement(a, null, maxMovesWithoutImprovement, obj, true)
}

/**
 * this combinator is stateless, it checks the condition on every invocation. If the condition is false,
 * it does not try the Neighborhood and finds no move.
 *
 * @author renaud.delandtsheer@cetic.be
 */
case class Guard(cond: () => Boolean, b: Neighborhood) extends NeighborhoodCombinator(b) {
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {
    if (cond()) b.getMove(obj, initialObj:Long, acceptanceCriteria)
    else NoMoveFound
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
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {
    @tailrec
    def search(): SearchResult = {
      val current = if (currentIsA) a else b
      current.getMove(obj, initialObj:Long, acceptanceCriteria) match {
        case NoMoveFound => if (currentIsA) { currentIsA = false; search() } else NoMoveFound
        case x: MoveFound => x
      }
    }
    search()
  }

  //this resets the internal state of the move combinators
  override def reset(): Unit = {
    currentIsA = true
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
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {
    if (isStopped || cond()) { isStopped = true; NoMoveFound }
    else a.getMove(obj, initialObj:Long, acceptanceCriterion)
  }

  //this resets the internal state of the move combinators
  override def reset(): Unit = {
    isStopped = false
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

  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {
    movesQueriedSinceReset += 1L
    if (movesQueriedSinceReset < maxMove
      && (movesQueriedSinceReset < minMoves || over() >= oldObjOnReset))
      a.getMove(obj, initialObj:Long, acceptanceCriterion)
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
  var remainingMoves = maxMove
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {
    if (remainingMoves > 0L) {
      remainingMoves -= 1L
      a.getMove(obj, initialObj:Long, acceptanceCriteria)
    } else NoMoveFound
  }

  //this resets the internal state of the move combinators
  override def reset(): Unit = {
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
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {
    def search(): SearchResult = {
      val current = if (currentIsA) a else b
      current.getMove(obj, initialObj:Long, acceptanceCriteria) match {
        case NoMoveFound =>
          if (currentIsA) {
            currentIsA = false
            b.reset()
            b.getMove(obj, initialObj:Long, acceptanceCriteria)
          } else {
            currentIsA = true
            a.reset()
            a.getMove(obj,initialObj:Long,  acceptanceCriteria)
          }
        case x: MoveFound => x
      }
    }
    search()
  }

  //this resets the internal state of the move combinators
  override def reset(): Unit = {
    currentIsA = true
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
  var consecutiveFails = 0L
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {
    a.getMove(obj, initialObj:Long, acceptanceCriteria) match {
      case NoMoveFound =>
        consecutiveFails = consecutiveFails + 1L
        if (cond(consecutiveFails)) {
          a.reset()
          getMove(obj, initialObj:Long, acceptanceCriteria)
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
  override def getMove(obj: Objective, initialObj: Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {
    val neighborhood = f()
    neighborhood.verbose = this.verbose
    neighborhood.getMove(obj, initialObj, acceptanceCriterion)
  }
}
