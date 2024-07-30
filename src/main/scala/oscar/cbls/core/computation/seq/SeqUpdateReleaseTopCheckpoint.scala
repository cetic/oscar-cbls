package oscar.cbls.core.computation.seq

import oscar.cbls.algo.sequence.IntSequence

/** Companion object of SeqUpdateReleaseTopCheckpoint
  */
object SeqUpdateReleaseTopCheckpoint {
  def apply(prev: SeqUpdate, seq: IntSequence): SeqUpdateReleaseTopCheckpoint = {
    new SeqUpdateReleaseTopCheckpoint(prev, seq)
  }

  def unapply(
    seqUpdateReleaseTopCheckPoint: SeqUpdateReleaseTopCheckpoint
  ): Option[(SeqUpdate, IntSequence)] = {
    Some((seqUpdateReleaseTopCheckPoint.prev, seqUpdateReleaseTopCheckPoint.newValue))
  }

}

/** A SeqUpdate that release the top checkpoint of the SeqVariable.
 *
 * @param prev
 *   The previous SeqUpdate of the batch
 * @param seq
 *   The IntSequence after the release of the checkpoint
 */
class SeqUpdateReleaseTopCheckpoint(prev: SeqUpdate, seq: IntSequence)
    extends SeqUpdateWithPrev(prev, seq) {

  override def oldPosToNewPos(oldPos: Int): Option[Int] = Some(oldPos)

  override def newPos2OldPos(newPos: Int): Option[Int] = Some(newPos)

  override protected[seq] def reverseThis(
    expectedValueAfterFullReverse: IntSequence,
    updatesAlreadyReversed: SeqUpdate
  ): SeqUpdate = {
    require(requirement = false, "We should never reverse a SeqUpdateReleaseTopCheckpoint")
    null
  }

  override protected[seq] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = {
      require(requirement = false, "This should not happen")
      null
  }

  override protected[seq] def regularize(maxPivot: Int): SeqUpdate =
    SeqUpdateReleaseTopCheckpoint(prev, seq.regularizeToMaxPivot(maxPivot))

  override def toString: String =
    s"SeqUpdateReleaseTopCheckPoint(checkpoint:$seq prev:$prev)"
}
