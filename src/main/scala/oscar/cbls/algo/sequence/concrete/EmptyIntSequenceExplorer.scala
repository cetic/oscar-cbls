package oscar.cbls.algo.sequence.concrete

import oscar.cbls.algo.sequence.IntSequenceExplorer

case class EmptyIntSequenceExplorer () extends IntSequenceExplorer {
  override val value: Int = Int.MinValue

  override def position: Int = -1

  override def next: Option[IntSequenceExplorer] = None

  override def prev: Option[IntSequenceExplorer] = None
}
