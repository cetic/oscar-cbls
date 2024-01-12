package oscar.cbls.algo.sequence

/** The boundary of the IntSequenceExplorer.
  *
  * Not part of the IntSequence. Meant to ease the insertion/move at position 0.
  *
  * @param intSequence
  *   The related IntSequence
  * @param backward
  *   - If true we reached the RootExplorer using prev or using intSequence.explorerAtPosition(-1)
  *   - If false we reached the RootExplorer using next or using
  *     intSequence.explorerAtPosition(intSequence.size)
  */
class RootIntSequenceExplorer(intSequence: IntSequence, val backward: Boolean)
    extends IntSequenceExplorer(intSequence) {
  override def value: Int = throw new NoSuchElementException(
    "RootIntSequenceExplorer doesn't have any value since it's not part of the sequence."
  )

  override def position: Int = if (backward) -1 else intSequence.size

  override def next: IntSequenceExplorer = {
    if (backward) {
      intSequence match {
        case _: EmptyIntSequence => this
        case _                   => intSequence.explorerAtPosition(0).get
      }
    } else {
      this
    }
  }

  override def nextUntil(f: IntSequenceExplorer => Boolean): Option[IntSequenceExplorer] = {
    if (backward) {
      intSequence match {
        case _: EmptyIntSequence => None
        case _                   => intSequence.explorerAtPosition(0).get.nextUntil(f)
      }
    } else {
      None
    }
  }

  override def nextUntilValue(value: Int): Option[IntSequenceExplorer] = {
    if (backward) {
      intSequence match {
        case _: EmptyIntSequence => None
        case _                   => intSequence.explorerAtPosition(0).get.nextUntilValue(value)
      }
    } else {
      None
    }
  }

  override def prev: IntSequenceExplorer = {
    if (backward) {
      this
    } else {
      intSequence match {
        case _: EmptyIntSequence => this
        case _                   => intSequence.explorerAtPosition(intSequence.size - 1).get
      }
    }
  }

  override def prevUntil(f: IntSequenceExplorer => Boolean): Option[IntSequenceExplorer] = {
    if (backward) {
      None
    } else {
      intSequence match {
        case _: EmptyIntSequence => None
        case _ => intSequence.explorerAtPosition(intSequence.size - 1).get.prevUntil(f)
      }
    }
  }

  override def prevUntilValue(value: Int): Option[IntSequenceExplorer] = {
    if (backward) {
      None
    } else {
      intSequence match {
        case _: EmptyIntSequence => None
        case _ => intSequence.explorerAtPosition(intSequence.size - 1).get.prevUntilValue(value)
      }
    }
  }

  override def foreach(f: IntSequenceExplorer => Unit): Unit = {
    if (backward) {
      next.foreach(f)
    }
  }
}
