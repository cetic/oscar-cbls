package oscar.cbls.algo.sequence.concrete
import oscar.cbls.algo.sequence._

/** The root [[IntSequenceExplorer]].
  *
  * Meant to ease the insertion/move at position 0.
  */
class RootIntSequenceExplorer(originalSequence: IntSequence) extends IntSequenceExplorer {
  override val value: Int = Int.MinValue

  override def position: Int = -1

  override def next: Option[IntSequenceExplorer] = {
    originalSequence match {
      case _: EmptyIntSequence => None
      case _                   => originalSequence.explorerAtPosition(0)
    }
  }

  override def prev: Option[IntSequenceExplorer] = None
}
