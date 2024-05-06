package oscar.cbls.core.search

sealed abstract class SearchResult {

}

object NoMoveFound extends SearchResult


object MoveFound{
  def apply(move: Move): MoveFound = {
    new MoveFound(move)
  }

}

class MoveFound(move: Move) extends SearchResult {
  def commit(): Unit = { move.commit() }
  def objAfter: Long = move.objAfter
  override def toString: String = move.toString
}
