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
  protected[seq] def reverseThis(
    expectedValueAfterFullReverse: IntSequence,
    updatesAlreadyReversed: SeqUpdate = SeqUpdateLastNotified(this.newValue)
  ): SeqUpdate

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
  protected[seq] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate

  /** Returns the needed updates in order to roll-back to an explicit checkPoint.
   *
   * Used when we have multiple roll-back instruction within the same stack of updates without a regularization
   *
   * @return
   */
  protected[seq] def explicitHowToRollBack(): SeqUpdate

  protected[seq] def regularize(maxPivot: Int): SeqUpdate

  // TODO : what is this actually for ???
  def depth: Int
}

/** A more sophisticate abstract structure for IntSequence updates
  *
  * It holds the previous update so that the invariant can easily handle a batch of updates. Only
  * extended by SeqUpdate that doesn't end the batch
  * @param prev
  * @param newValue
  *   The value of the IntSequence after the update.
  */
abstract class SeqUpdateWithPrev(val prev: SeqUpdate, newValue: IntSequence)
    extends SeqUpdate(newValue) {
  def oldPosToNewPos(oldPos: Int): Option[Int]
  def newPos2OldPos(newPos: Int): Option[Int]
  override val depth: Int = {
    val previousDepth = prev.depth
    if (previousDepth >= 0) previousDepth + 1 else previousDepth - 1
  }
}
