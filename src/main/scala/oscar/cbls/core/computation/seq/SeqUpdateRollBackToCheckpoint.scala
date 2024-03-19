package oscar.cbls.core.computation.seq

import oscar.cbls.algo.sequence.IntSequence

object SeqUpdateRollBackToCheckpoint {
  def apply(
    checkpoint: IntSequence,
    howToRollBack: SeqUpdate,
    level: Int
  ): SeqUpdateRollBackToCheckpoint = {
    new SeqUpdateRollBackToCheckpoint(checkpoint, howToRollBack, level)
  }

  def unapply(
    seqUpdateRollBackToCheckpoint: SeqUpdateRollBackToCheckpoint
  ): Option[(IntSequence, Int)] =
    Some(seqUpdateRollBackToCheckpoint.checkpoint, seqUpdateRollBackToCheckpoint.level)
}

class SeqUpdateRollBackToCheckpoint(
  val checkpoint: IntSequence,
  val howToRollBack: SeqUpdate,
  val level: Int
) extends SeqUpdate(checkpoint) {

  /** Reverses the current update, used when roll-backing to a checkPoint. Since, those updates will
    * be appended to the existing updates they have to be reverse from last update to first update.
    *
    * For instance :
    *   - sinceLastCheckPoint : Insert(A, prevUpdate = Remove(B, prevUpdate = LastNotified(seq)))
    *   - reversed : Insert(B, prevUpdate = Remove(A))
    *
    * @param expectedValueAfterFullReverse
    *   The expected IntSequence value when all updates are reversed
    * @param updatesAlreadyReversed
    *   The updates already reversed
    * @return
    *   The stack of update that reverses the updates since last checkPoint
    */
  override protected[seq] def reverseThis(
    expectedValueAfterFullReverse: IntSequence,
    updatesAlreadyReversed: SeqUpdate
  ): SeqUpdate = {
    require(false, "Cannot reverse SeqUpdateRollBackToCheckpoint")
    null
  }

  /** Appends the current update after the updates passed as parameter.
    *
    * This has to be applied after the previousUpdate so we'll have. ThisUpdate(..., prev =
    * previousUpdates)
    *
    * @param previousUpdates
    *   The updates after which this is applied
    * @return
    *   A new set of updates
    */
  override protected[seq] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = this

  /** Returns the needed updates in order to roll-back to an explicit checkPoint.
    *
    * Used when we have multiple roll-back instruction within the same stack of updates without a
    * regularization
    *
    * @return
    */
  override protected[seq] def explicitHowToRollBack(): SeqUpdate = howToRollBack

  override protected[seq] def regularize(maxPivot: Int): SeqUpdate = this

  override def depth: Int = 0

  override def toString: String =
    s"SeqUpdateRollBackToCheckpoint(level:$level checkpoint:$checkpoint)"
}
