// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.core.computation.seq

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer, RootIntSequenceExplorer}

object SeqUpdateInsert {

  /** Returns the update corresponding to the insertion of the specified value after the
    * IntSequenceExplorer.
    *
    * @param value
    *   The value we want to insert.
    * @param insertAfterPositionExplorer
    *   The IntSequenceExplorer at the position after which we want to insert the value.
    * @param prev
    *   The last update of the IntSequence.
    * @return
    *   The update corresponding to the defined insertion.
    */
  def apply(
    value: Int,
    insertAfterPositionExplorer: IntSequenceExplorer,
    prev: SeqUpdate
  ): SeqUpdate = {
    prev match {
      // here, since there is no seq given, we compare on the move itself to annihilate the moves
      case x @ SeqUpdateRemove(
            removedPositionExplorer: IntSequenceExplorer,
            prevOfDelete: SeqUpdate
          )
          if (insertAfterPositionExplorer.position + 1 == removedPositionExplorer.position
            && value == x.explorerAtRemovePosition.value) =>
        prevOfDelete
      case _ =>
        new SeqUpdateInsert(
          value,
          insertAfterPositionExplorer,
          prev,
          prev.newValue.insertAfterPosition(value, insertAfterPositionExplorer, fast = true)
        )
    }
  }

  /** Returns the update corresponding to the insertion of the specified value after the
    * IntSequenceExplorer knowing the resulting IntSequence.
    *
    * @param value
    *   The value we want to insert.
    * @param insertAfterPositionExplorer
    *   The IntSequenceExplorer at the position after which we want to insert the value.
    * @param prev
    *   The last update of the IntSequence.
    * @param seqAfter
    *   The IntSequence value after the insertion.
    * @return
    *   The update corresponding to the defined insertion.
    */
  def apply(
    value: Int,
    insertAfterPositionExplorer: IntSequenceExplorer,
    prev: SeqUpdate,
    seqAfter: IntSequence
  ): SeqUpdate = {
    prev match {
      // checks if the last two moves cancelled themselves
      case _ @SeqUpdateRemove(_: IntSequenceExplorer, prevOfDelete: SeqUpdate)
          if prevOfDelete.newValue sameIdentity seqAfter =>
        prevOfDelete
      case _ => new SeqUpdateInsert(value, insertAfterPositionExplorer, prev, seqAfter)
    }
  }

  /** Extracts the parameters of the SeqUpdateInsert.
    *
    * @param seqUpdateInsert
    *   The update we want to extracts the parameters from.
    * @return
    *   The inserted value, the explorer after which it was inserted and the update before it.
    */
  def unapply(seqUpdateInsert: SeqUpdateInsert): Option[(Int, IntSequenceExplorer, SeqUpdate)] =
    Some(seqUpdateInsert.value, seqUpdateInsert.insertAfterPositionExplorer, seqUpdateInsert.prev)
}

/** An IntSequence update, this update consists in inserting a new node after a given position.
  *
  * The position is passed as a IntSequenceExplorer to ease the update of the potential Invariant
  * depending on this SeqVariable.
  *
  * @param value
  *   The inserted value.
  * @param insertAfterPositionExplorer
  *   The IntSequenceExplorer after which the value is inserted.
  * @param prev
  *   The previous update of the IntSequence.
  * @param seqAfter
  *   The new IntSequence value.
  */
class SeqUpdateInsert(
  val value: Int,
  val insertAfterPositionExplorer: IntSequenceExplorer,
  prev: SeqUpdate,
  seqAfter: IntSequence
) extends SeqUpdateWithPrev(prev: SeqUpdate, seqAfter) {

  // The position of the new value
  private lazy val insertionPos: Int =
    insertAfterPositionExplorer match {
      case _: RootIntSequenceExplorer => 0
      case x                          => x.next.position
    }

  override protected[computation] def reverseThis(
    expectedValueAfterFullReverse: IntSequence,
    updatesAlreadyReversed: SeqUpdate
  ): SeqUpdate = {
    val explorerAtInsertionPositionInNewSeq = seqAfter.explorerAtPosition(insertionPos).get
    prev.reverseThis(
      expectedValueAfterFullReverse,
      SeqUpdateRemove(explorerAtInsertionPositionInNewSeq, updatesAlreadyReversed, prev.newValue)
    )
  }

  override protected[computation] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = {
    SeqUpdateInsert(
      value: Int,
      insertAfterPositionExplorer: IntSequenceExplorer,
      prev.appendThisTo(previousUpdates),
      seqAfter
    )
  }

  override def oldPosToNewPos(oldPos: Int): Option[Int] = {
    if (oldPos < insertionPos) Some(oldPos)
    else Some(oldPos + 1)
  }

  override def newPos2OldPos(newPos: Int): Option[Int] = {
    if (newPos == insertionPos) None
    else if (newPos <= insertAfterPositionExplorer.position) Some(newPos)
    else Some(newPos - 1)
  }

  override protected[computation] def regularize(maxPivot: Int): SeqUpdate =
    SeqUpdateInsert(
      value,
      insertAfterPositionExplorer,
      prev,
      seqAfter.regularizeToMaxPivot(maxPivot)
    )

  override def toString: String =
    s"SeqUpdateInsert(value:$value after position:${insertAfterPositionExplorer.position} prev:$prev)"
}
