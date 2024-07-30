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

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}

object SeqUpdateRemove {

  /** Returns the update corresponding to the removal of the value at the specified
    * IntSequenceExplorer
    *
    * @param removePositionExplorer
    *   The IntSequenceExplorer at the position of the value we want to remove
    * @param prev
    *   The last update of the IntSequence
    * @return
    *   The update corresponding to the defined removal
    */
  def apply(removePositionExplorer: IntSequenceExplorer, prev: SeqUpdate): SeqUpdate = {
    apply(removePositionExplorer, prev, prev.newValue.remove(removePositionExplorer, fast = true))
  }

  /** Returns the update corresponding to the removal of the value at the specified
    * IntSequenceExplorer knowing the resulting IntSequence
    *
    * @param removePositionExplorer
    *   The IntSequenceExplorer at the position of the value we want to remove
    * @param prev
    *   The last update of the IntSequence
    * @param seqAfter
    *   The IntSequence value after the removal
    * @return
    *   The update corresponding to the defined removal
    */
  def apply(
             removePositionExplorer: IntSequenceExplorer,
             prev: SeqUpdate,
             seqAfter: IntSequence
  ): SeqUpdate = {
    prev match {
      // check if the last two moves cancelled themselves
      case SeqUpdateInsert(_: Int, _: IntSequenceExplorer, insertPrev: SeqUpdate)
          if insertPrev.newValue sameIdentity seqAfter =>
        insertPrev
      case _ => new SeqUpdateRemove(removePositionExplorer, prev, seqAfter)
    }
  }

  /** Extracts the parameters of the SeqUpdateRemove
    *
    * @param seqUpdateRemove
    *   The update we want to extracts the parameters from
    * @return
    *   The explorer at the removal position and the update before it
    */
  def unapply(seqUpdateRemove: SeqUpdateRemove): Option[(IntSequenceExplorer, SeqUpdate)] =
    Some(seqUpdateRemove.explorerAtRemovePosition, seqUpdateRemove.prev)
}

/** An IntSequence update, this update consists in removing a node at a given position.
  *
  * The position is passed as a IntSequenceExplorer to ease the update of the potential Invariant
  * depending on this IntSequence.
  *
  * @param explorerAtRemovePosition
  *   The IntSequenceExplorer at the position of the value we want to remove
  * @param prev
  *   The previous update of the IntSequence
  * @param seqAfter
  *   The new IntSequence value
  */
class SeqUpdateRemove(
                       val explorerAtRemovePosition: IntSequenceExplorer,
                       prev: SeqUpdate,
                       seqAfter: IntSequence
) extends SeqUpdateWithPrev(prev, seqAfter) {

  override protected[computation] def reverseThis(
    expectedValueAfterFullReverse: IntSequence,
    updatesAlreadyReversed: SeqUpdate
  ): SeqUpdate = {
    prev.reverseThis(
      expectedValueAfterFullReverse,
      SeqUpdateInsert(
        explorerAtRemovePosition.value,
        seqAfter.explorerAtPosition(explorerAtRemovePosition.position - 1).get,
        updatesAlreadyReversed,
        prev.newValue
      )
    )
  }

  override protected[computation] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = {
    SeqUpdateRemove(explorerAtRemovePosition, prev.appendThisTo(previousUpdates), seqAfter)
  }

  override def oldPosToNewPos(oldPos: Int): Option[Int] = {
    if (oldPos == explorerAtRemovePosition.position) None
    else if (oldPos < explorerAtRemovePosition.position) Some(oldPos)
    else Some(oldPos - 1)
  }

  override def newPos2OldPos(newPos: Int): Option[Int] = {
    if (newPos < explorerAtRemovePosition.position) Some(newPos)
    else Some(newPos + 1)
  }

  override protected[computation] def regularize(maxPivot: Int): SeqUpdate =
    SeqUpdateRemove(explorerAtRemovePosition, prev, seqAfter.regularizeToMaxPivot(maxPivot))

  override def toString: String =
    s"SeqUpdateRemove(value:${explorerAtRemovePosition.value} position:${explorerAtRemovePosition.position} prev:$prev)"
}
