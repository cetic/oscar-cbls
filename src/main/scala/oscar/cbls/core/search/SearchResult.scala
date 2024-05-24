package oscar.cbls.core.search

/** The result of an exploration * */
sealed abstract class SearchResult {}

/** The result of an exploration where no valid move were found * */
object NoMoveFound extends SearchResult

/** Companion object of MoveFound */
object MoveFound {
  def apply(move: Move): MoveFound = {
    new MoveFound(move)
  }

  def unapply(moveFound: MoveFound): Move = {
    moveFound.move
  }

}

/** The result of an exploration were a valid move has been found * */
class MoveFound(val move: Move) extends SearchResult {

  /** Commits the validated move. */
  def commit(): Unit = {
    move.commit()
  }
  def objAfter(): Long          = move.objAfter()
  override def toString: String = move.toString
}
