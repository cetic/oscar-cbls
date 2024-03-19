package oscar.cbls.core.computation.seq

import oscar.cbls.algo.sequence.IntSequence

/** The first update of any stack of updates.
  *
  * A new SeqUpdateLastNotified is created when :
  *   - Defining a new checkpoint
  *   - Roll-backing to a previous checkpoint
  *   - Propagating
  *   - Creating a SeqVariable
  *
  * @param value
  *   The starting IntSequence value of the stack of updates
  */
case class SeqUpdateLastNotified(value: IntSequence) extends SeqUpdate(value) {

  /** Appends the current update after the updates passed as parameter.
    *
    * This has to be applied after the previousUpdate so we'll have. ThisUpdate(..., prev =
    * previousUpdates)
    *
    * @param updatesAlreadyReversed
    *   The updates after which this is applied
    * @return
    *   A new set of updates
    */
  override protected[seq] def reverseThis(
    expectedValueAfterFullReverse: IntSequence,
    updatesAlreadyReversed: SeqUpdate
  ): SeqUpdate = {
    require(
      expectedValueAfterFullReverse quickEquals this.newValue,
      s"not proper reverse target on $this target:$expectedValueAfterFullReverse"
    )
    // In this case we
    updatesAlreadyReversed
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
  override protected[seq] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = {
    require(
      this.newValue quickEquals previousUpdates.newValue,
      "illegal append operation; values do not match"
    )
    previousUpdates
  }

  override protected[seq] def explicitHowToRollBack(): SeqUpdate = this

  override protected[seq] def regularize(maxPivot: Int): SeqUpdate = SeqUpdateLastNotified(
    value.regularizeToMaxPivot(maxPivot)
  )

  override def depth: Int = 0
}
