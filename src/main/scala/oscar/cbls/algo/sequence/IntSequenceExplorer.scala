package oscar.cbls.algo.sequence

abstract class IntSequenceExplorer {
  val value: Int
  def position: Int
  def next: Option[IntSequenceExplorer]
  def prev: Option[IntSequenceExplorer]
}
