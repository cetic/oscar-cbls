package oscar.cbls.lib.search.combinators

import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{InstrumentedMove, Move, MoveFound, Neighborhood, NeighborhoodCombinator, NoMoveFound, SearchResult}

/**
 * this combinator attaches a custom code to a given neighborhood.
 * the code is called whenever a move is asked to the neighborhood.
 *
 * @param a a neighborhood
 * @param proc the procedure to execute before the neighborhood is queried
 */
case class DoOnQuery(a: Neighborhood, proc: () => Unit) extends NeighborhoodCombinator(a) {
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {
    proc()
    a.getMove(obj, initialObj, acceptanceCriteria)
  }
}

/**
 * this combinator attaches a custom code to a given neighborhood.
 * the code is called whenever a move from this neighborhood is taken for the first time.
 * notice that this neighborhood is reset, so first time can occur several times.
 *
 * @param a a neighborhood
 * @param proc the procedure to call on one first move that is performed from this neighborhood
 */
case class DoOnFirstMove(a: Neighborhood, proc: () => Unit) extends NeighborhoodCombinator(a) {
  var isFirstMove = true
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {
    if (isFirstMove) {
      a.getMove(obj, initialObj, acceptanceCriteria) match {
        case m: MoveFound => InstrumentedMove(m.m, notifyMoveTaken _)
        case x => x
      }
    } else {
      a.getMove(obj, initialObj, acceptanceCriteria)
    }
  }

  //this resets the internal state of the move combinators
  override def reset(): Unit ={
    isFirstMove = true
    super.reset()
  }

  private def notifyMoveTaken(): Unit ={
    proc()
    isFirstMove = false
  }
}

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
case class DoOnMove(a: Neighborhood,
                    procBeforeMove: Move => Unit = null,
                    procAfterMove: Move => Unit = null) extends NeighborhoodCombinator(a) {
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {
    a.getMove(obj, initialObj, acceptanceCriteria) match {
      case m: MoveFound =>
        InstrumentedMove(m.m, callBackBeforeMove(m.m), callBackAfterMove(m.m))
      case x => x
    }
  }

  def callBackBeforeMove(m: Move)(): Unit ={
    if (procBeforeMove != null) procBeforeMove(m)
  }

  def callBackAfterMove(m: Move)(): Unit ={
    if (procAfterMove != null) procAfterMove(m)
  }
}

case class DoOnExhaust(a:Neighborhood, proc:()=>Unit,onlyFirst:Boolean) extends NeighborhoodCombinator(a) {

  var alreadyExhaustedOnce = false
  override def getMove(obj : Objective, initialObj:Long, acceptanceCriterion : (Long, Long) => Boolean) : SearchResult =
    a.getMove(obj,initialObj,acceptanceCriterion) match{
      case NoMoveFound =>
        if(!onlyFirst || !alreadyExhaustedOnce){
          alreadyExhaustedOnce = true
          proc()
        }
        NoMoveFound
      case x:MoveFound => x
    }
}
