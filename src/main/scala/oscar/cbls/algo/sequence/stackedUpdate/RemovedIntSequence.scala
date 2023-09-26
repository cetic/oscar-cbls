package oscar.cbls.algo.sequence.stackedUpdate

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}

class RemovedIntSequence(val seq: IntSequence, val positionOfDelete: Int, depth: Int)
  extends StackedUpdateIntSequence(depth) {

  val removedValue = seq.valueAtPosition(positionOfDelete).head

  override def descriptorString: String =
    seq.descriptorString + ".removed(pos:" + positionOfDelete + " val:" + removedValue + ")"

  override def nbOccurrence(value: Int): Int =
    if (value == this.removedValue) seq.nbOccurrence(value) - 1 else seq.nbOccurrence(value)

  override def unorderedContentNoDuplicate: List[Int] =
    if (seq.nbOccurrence(removedValue) > 1) seq.unorderedContentNoDuplicate
    else seq.unorderedContentNoDuplicate.filter(_ != removedValue)

  override def unorderedContentNoDuplicateWithNBOccurences: List[(Int, Int)] =
    unorderedContentNoDuplicate.flatMap(value =>
      if (value == removedValue) {
        val occurencesBefore = seq.nbOccurrence(value)
        if (occurencesBefore == 1) None
        else Some((value, occurencesBefore - 1))
      } else Some((value, seq.nbOccurrence(value)))
    )

  override val size: Int = seq.size - 1

  override def explorerAtPosition(position: Int): Option[IntSequenceExplorer] = {
    seq.explorerAtPosition(if (position < this.positionOfDelete) position else position + 1) match {
      case None    => None
      case Some(e) => Some(new RemovedIntSequenceExplorer(this, position, e))
    }
  }

  override def positionsOfValueQ(value: Int): List[Int] = {
    var positionsBefore     = seq.positionsOfValueQ(value)
    var toReturn: List[Int] = null
    while (positionsBefore != null) {
      val oldPos = positionsBefore.head
      positionsBefore = positionsBefore.tail
      if (oldPos < this.positionOfDelete) {
        toReturn = List(oldPos) ::: toReturn
      } else if (oldPos > positionOfDelete) {
        toReturn = List(oldPos - 1) ::: toReturn
      }
    }
    toReturn
  }

  def oldPos2NewPos(oldPos: Int) = {
    if (oldPos < this.positionOfDelete) oldPos else oldPos - 1
  }

  override def contains(value: Int): Boolean = {
    if (value == removedValue) seq.nbOccurrence(value) > 1
    else seq.contains(value)
  }

  override def commitPendingMoves: IntSequence =
    seq.commitPendingMoves.delete(this.positionOfDelete, fast = false, autoRework = false)

  override def valueAtPosition(position: Int): Option[Int] = {
    if (position >= this.positionOfDelete) seq.valueAtPosition(position + 1)
    else seq.valueAtPosition(position)
  }
}

class RemovedIntSequenceExplorer(
                                  seq: RemovedIntSequence,
                                  val position: Int,
                                  explorerInOriginalSeq: IntSequenceExplorer
                                ) extends IntSequenceExplorer {
  override val value: Int = explorerInOriginalSeq.value

  override def prev: Option[IntSequenceExplorer] = {
    explorerInOriginalSeq.prev match {
      case None => None
      case Some(tentativePos) =>
        if (tentativePos.position == seq.positionOfDelete)
          tentativePos.prev match {
            case None => None
            case Some(secondTentativePos) =>
              Some(new RemovedIntSequenceExplorer(seq, position - 1, secondTentativePos))
          }
        else Some(new RemovedIntSequenceExplorer(seq, position - 1, tentativePos))
    }
  }

  override def next: Option[IntSequenceExplorer] = {
    explorerInOriginalSeq.next match {
      case None => None
      case Some(tentativePos) =>
        if (tentativePos.position == seq.positionOfDelete)
          tentativePos.next match {
            case None => None
            case Some(secondTentativePos) =>
              Some(new RemovedIntSequenceExplorer(seq, position + 1, secondTentativePos))
          }
        else Some(new RemovedIntSequenceExplorer(seq, position + 1, tentativePos))
    }
  }
}