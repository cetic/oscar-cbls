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
    * IntSequenceExplorer
    *
    * @param removePositionExplorer
    *   The IntSequenceExplorer at the position of the value we want to remove
    * @param prev
    *   The last update of the IntSequence
    * @param seq
    *   The IntSequence value after the removal (if known)
    * @return
    *   The update corresponding to the defined removal
    */
  def apply(
    removePositionExplorer: IntSequenceExplorer,
    prev: SeqUpdate,
    seq: IntSequence
  ): SeqUpdate = {
    prev match {
      // check if the last two moves cancelled themselves
      case SeqUpdateInsert(_: Int, _: IntSequenceExplorer, insertPrev: SeqUpdate)
          if insertPrev.newValue quickEquals seq =>
        insertPrev
      case _ => new SeqUpdateRemove(removePositionExplorer, prev, seq)
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
  * @param seq
  *   The new IntSequence value
  */
class SeqUpdateRemove(
  val explorerAtRemovePosition: IntSequenceExplorer,
  prev: SeqUpdate,
  seq: IntSequence
) extends SeqUpdateWithPrev(prev, seq) {

  override protected[computation] def reverseThis(
    expectedValueAfterFullReverse: IntSequence,
    updatesAlreadyReversed: SeqUpdate
  ): SeqUpdate = {
    prev.reverseThis(
      expectedValueAfterFullReverse,
      SeqUpdateInsert(
        explorerAtRemovePosition.value,
        explorerAtRemovePosition,
        updatesAlreadyReversed,
        prev.newValue
      )
    )
  }

  override protected[computation] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = {
    SeqUpdateRemove(explorerAtRemovePosition, prev.appendThisTo(previousUpdates), seq)
  }

  override protected[computation] def explicitHowToRollBack(): SeqUpdate = {
    SeqUpdateRemove(explorerAtRemovePosition, prev.explicitHowToRollBack(), seq)
  }

  override def oldPosToNewPos(oldPos: Int): Option[Int] = {
    if (oldPos == explorerAtRemovePosition.position) None
    else if (oldPos < explorerAtRemovePosition) Some(oldPos)
    else Some(oldPos - 1)
  }

  override def newPos2OldPos(newPos: Int): Option[Int] = {
    if (newPos < explorerAtRemovePosition) Some(newPos)
    else Some(newPos + 1)
  }

  override protected[computation] def regularize(maxPivot: Int): SeqUpdate =
    SeqUpdateRemove(explorerAtRemovePosition, prev, seq.regularizeToMaxPivot(maxPivot))

  override def toString: String =
    s"SeqUpdateRemove(value:${explorerAtRemovePosition.value} position:${explorerAtRemovePosition.position} prev:$prev)"
}
