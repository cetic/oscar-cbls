package oscar.cbls.algo.sequence.concrete
import oscar.cbls.algo.sequence._

/** The root [[IntSequenceExplorer]].
  *
  * Meant to ease the insertion/move at position 0.
  */
class RootIntSequenceExplorer(originalSequence: IntSequence) extends IntSequenceExplorer {
  override def value: Int = throw new NoSuchElementException(
    "RootIntSequenceExplorer doesn't have any value since it's not part of the sequence."
  )

  override def position: Int = -1

  override def next: Option[IntSequenceExplorer] = {
    originalSequence match {
      case _: EmptyIntSequence => None
      case _                   => originalSequence.explorerAtPosition(0)
    }
  }

  override def prev: Option[IntSequenceExplorer] = None

  override def prevUntil(f: IntSequenceExplorer => Boolean): Option[IntSequenceExplorer] = None

  override def prevUntilValue(value: Int): Option[IntSequenceExplorer] = None
}
