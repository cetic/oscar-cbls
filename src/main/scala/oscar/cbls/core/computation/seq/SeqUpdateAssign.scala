package oscar.cbls.core.computation.seq

import oscar.cbls.algo.sequence.IntSequence

object SeqUpdateAssign {
  def apply(sequence: IntSequence): SeqUpdateAssign = new SeqUpdateAssign(sequence)

  def unapply(seqUpdateAssign: SeqUpdateAssign): Option[IntSequence] =
    Some(seqUpdateAssign.newSequence)
}

/** A [[SeqUpdate]] that assign a new value to the [[oscar.cbls.algo.sequence.IntSequence]].
  *
  * This update is NOT INCREMENTAL.
  */
class SeqUpdateAssign(val newSequence: IntSequence) extends SeqUpdate(newSequence) {

  override protected[computation] def reverseThis(
    newValueForThisAfterFullReverse: IntSequence,
    nextOp: SeqUpdate
  ): SeqUpdate = {
    SeqUpdateAssign(newValueForThisAfterFullReverse)
  }

  override protected[computation] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = this

  override protected[computation] def explicitHowToRollBack(): SeqUpdate = this

  override protected[computation] def regularize(maxPivot: Int): SeqUpdate =
    SeqUpdateAssign(newSequence.regularizeToMaxPivot(maxPivot))

  override def depth: Int = -1
}
