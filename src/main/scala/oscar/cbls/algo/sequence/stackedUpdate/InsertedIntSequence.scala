package oscar.cbls.algo.sequence.stackedUpdate

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}

class InsertedIntSequence(seq: IntSequence, val insertedValue: Int, val pos: Int, depth: Int)
    extends StackedUpdateIntSequence(depth) {
  override val size: Int = seq.size + 1

  override def nbOccurrence(value: Int): Int =
    if (value == this.insertedValue) seq.nbOccurrence(value) + 1 else seq.nbOccurrence(value)

  override def unorderedContentNoDuplicateWithNBOccurences: List[(Int, Int)] =
    unorderedContentNoDuplicate.map(value =>
      (value, if (value == insertedValue) seq.nbOccurrence(value) + 1 else seq.nbOccurrence(value))
    )

  override def descriptorString: String =
    s"${seq.descriptorString}.inserted(val:$insertedValue pos:$pos)"

  override def unorderedContentNoDuplicate: List[Int] = if (seq.nbOccurrence(insertedValue) == 0)
    insertedValue :: seq.unorderedContentNoDuplicate
  else seq.unorderedContentNoDuplicate

  override def positionsOfValueQ(value: Int): List[Int] = {
    var positionsBefore     = seq.positionsOfValueQ(value)
    var toReturn: List[Int] = null
    while (positionsBefore != null) {
      val oldPos = positionsBefore.head
      positionsBefore = positionsBefore.tail
      val newPos = oldPos2NewPos(oldPos)
      toReturn = List(newPos) ::: toReturn
    }
    if (value == insertedValue) List(pos) ::: toReturn
    else toReturn
  }

  @inline
  private def oldPos2NewPos(oldPOs: Int): Int = {
    if (oldPOs < pos) oldPOs else oldPOs + 1
  }

  override def explorerAtPosition(position: Int): Option[IntSequenceExplorer] = {
    if (position == this.pos) {
      if (position == 0) {
        Some(new InsertedIntSequenceExplorer(this, position, seq.explorerAtPosition(0), true, true))
      } else {
        Some(
          new InsertedIntSequenceExplorer(
            this,
            position,
            seq.explorerAtPosition(position - 1),
            true,
            false
          )
        )
      }
    } else if (position < this.pos) {
      seq.explorerAtPosition(position) match {
        case None    => None
        case Some(p) => Some(new InsertedIntSequenceExplorer(this, position, Some(p), false, false))
      }
    } else {
      seq.explorerAtPosition(position - 1) match {
        case None    => None
        case Some(p) => Some(new InsertedIntSequenceExplorer(this, position, Some(p), false, false))
      }
    }
  }

  override def contains(value: Int): Boolean = value == this.insertedValue || seq.contains(value)

  override def commitPendingMoves: IntSequence =
    seq.commitPendingMoves.insertAtPosition(insertedValue, pos, fast = false, autoRework = false)

  override def isEmpty: Boolean = false

  override def valueAtPosition(position: Int): Option[Int] = {
    if (position == pos) Some(insertedValue)
    else if (position < pos) seq.valueAtPosition(position)
    else seq.valueAtPosition(position - 1)
  }
}
