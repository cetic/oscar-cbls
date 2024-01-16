package oscar.cbls.algo.sequence

/** The boundary of the IntSequenceExplorer.
  *
  * Not part of the IntSequence. Meant to ease the insertion/move at position 0.
  *
  * @param intSequence
  *   The related IntSequence
  * @param beforeStart
  *   - If true we reached the RootExplorer using prev or using intSequence.explorerAtPosition(-1)
  *   - If false we reached the RootExplorer using next or using
  *     intSequence.explorerAtPosition(intSequence.size)
  */
class RootIntSequenceExplorer(intSequence: IntSequence, val beforeStart: Boolean)
    extends IntSequenceExplorer(intSequence) {
  override def value: Int = throw new NoSuchElementException(
    "RootIntSequenceExplorer doesn't have any value since it's not part of the sequence."
  )

  override def position: Int = if (beforeStart) -1 else intSequence.size

  override def next: IntSequenceExplorer = {
    if (beforeStart) {
      intSequence match {
        case _: EmptyIntSequence => this
        case _                   => intSequence.explorerAtPosition(0).get
      }
    } else {
      this
    }
  }

  override def exploreForwardUntil(f: IntSequenceExplorer => Boolean): Option[IntSequenceExplorer] = {
    if (beforeStart) {
      intSequence match {
        case _: EmptyIntSequence => None
        case _                   => intSequence.explorerAtPosition(0).get.exploreForwardUntil(f)
      }
    } else {
      None
    }
  }

  override def exploreForwardUntilValue(value: Int): Option[IntSequenceExplorer] = {
    if (beforeStart) {
      intSequence match {
        case _: EmptyIntSequence => None
        case _                   => intSequence.explorerAtPosition(0).get.exploreForwardUntilValue(value)
      }
    } else {
      None
    }
  }

  override def prev: IntSequenceExplorer = {
    if (beforeStart) {
      this
    } else {
      intSequence match {
        case _: EmptyIntSequence => this
        case _                   => intSequence.explorerAtPosition(intSequence.size - 1).get
      }
    }
  }

  override def exploreBackwardUntil(f: IntSequenceExplorer => Boolean): Option[IntSequenceExplorer] = {
    if (beforeStart) {
      None
    } else {
      intSequence match {
        case _: EmptyIntSequence => None
        case _ => intSequence.explorerAtPosition(intSequence.size - 1).get.exploreBackwardUntil(f)
      }
    }
  }

  override def exploreBackwardUntilValue(value: Int): Option[IntSequenceExplorer] = {
    if (beforeStart) {
      None
    } else {
      intSequence match {
        case _: EmptyIntSequence => None
        case _ => intSequence.explorerAtPosition(intSequence.size - 1).get.exploreBackwardUntilValue(value)
      }
    }
  }
}
