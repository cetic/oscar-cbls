package oscar.cbls.core.search

import oscar.cbls.core.computation.objective.Objective

sealed abstract class SearchResult {

}

object NoMoveFound extends SearchResult


object MoveFound{
  def apply(move: Move): MoveFound = {
    new MoveFound(move)
  }

  def unapply(moveFound: MoveFound): Move = {
    moveFound.move
  }

}

class MoveFound(val move: Move) extends SearchResult {
  def commit(): Unit = {
    move.commit()
  }
  def objAfter(): Long = move.objAfter()
  override def toString: String = move.toString
}
