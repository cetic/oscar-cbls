package oscar.cbls.core.search

abstract class Move {
  def commit(): Unit
  def objAfter(): Long
}
