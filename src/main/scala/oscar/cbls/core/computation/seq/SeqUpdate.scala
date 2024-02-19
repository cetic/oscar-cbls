package oscar.cbls.core.computation.seq

import oscar.cbls.algo.sequence.IntSequence

/** Abstract structure for IntSequence updates.
  *
  * In local search we explore a lot of neighbors before selecting the next one and iterating. To
  * avoid the constant modification of the IntSequence value we use small updates of the sequence.
  * Those updates can be stacked and rolled-back in constant time.
  *
  * @param newValue
  *   The value of the IntSequence after the update.
  */
abstract class SeqUpdate(val newValue: IntSequence) {
  protected[seq] def reverseThis(newValueForThisAfterFullReverse:IntSequence, nextOp:SeqUpdate = SeqUpdateLastNotified(this.newValue)):SeqUpdate
  protected[seq] def appendThisTo(previousUpdates:SeqUpdate):SeqUpdate
  protected[seq] def explicitHowToRollBack():SeqUpdate

  protected[seq] def regularize(maxPivot:Int):SeqUpdate

  def depth:Int
}

/** A more sophisticate abstract structure for IntSequence updates
 *
 * It holds the previous update so that the invariant can easily handle a batch of updates.
 * Only extended by SeqUpdate that doesn't end the batch
 * @param prev
 * @param newValue
 *   The value of the IntSequence after the update.
 */
abstract class SeqUpdateWithPrev(val prev: SeqUpdate, newValue: IntSequence)
    extends SeqUpdate(newValue) {
  def oldPosToNewPos(oldPos:Int):Option[Int]
  def newPos2OldPos(newPos:Int):Option[Int]
  override val depth:Int = {
    val previousDepth = prev.depth
    if(previousDepth >=0) previousDepth+1 else previousDepth-1
  }
}
