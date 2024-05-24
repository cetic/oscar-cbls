package oscar.cbls.core.search

abstract class Move {

  /** Commits this move. */
  def commit(): Unit
  def objAfter(): Long
}
