package oscar.cbls.algo.sequence.concrete

import oscar.cbls.algo.sequence.stackedUpdate.InsertedIntSequence
import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer, Token}

class EmptyIntSequence(depth: Int) extends IntSequence(depth = depth) {
  override def size: Int = 0

  override def nbOccurrence(value: Int): Int = 0

  override def unorderedContentNoDuplicate: List[Int] = List.empty

  override def unorderedContentNoDuplicateWithNBOccurrences: List[(Int, Int)] = List.empty

  override def valueAtPosition(position: Int): Option[Int] = None

  override def positionsOfValue(value: Int): List[Int] = List.empty

  override def contains(value: Int): Boolean = false

  override def explorerAtPosition(position: Int): Option[IntSequenceExplorer] =
    if (position == -1) Some(EmptyIntSequenceExplorer())
    else None

  override def insertAfterPosition(
    value: Int,
    insertAfterPositionExpl: Option[IntSequenceExplorer],
    fast: Boolean
  ): IntSequence = {
    require(insertAfterPositionExpl.isEmpty, "InsertAfterPositionExpl must be defined.")

    IntSequence(List(value))
  }

  override def delete(removePosAsExplorer: IntSequenceExplorer, fast: Boolean): IntSequence = {
    require(requirement = false, "Can't remove a value if the sequence is empty")
    this
  }

  override def moveAfter(
    fromIncludedExpl: IntSequenceExplorer,
    toIncludedExpl: IntSequenceExplorer,
    moveAfterExpl: Option[IntSequenceExplorer],
    flip: Boolean,
    fast: Boolean
  ): IntSequence = {
    require(requirement = false, "Can't move values if the sequence is empty")
    this
  }

  override def regularizeToMaxPivot(
    maxPivotPerValuePercent: Int,
    targetToken: Token
  ): EmptyIntSequence = this

  override def regularize(targetToken: Token): EmptyIntSequence = this

  override def commitPendingMoves: IntSequence = this

  override def descriptorString: String =
    "[]_impl:empty"
}
