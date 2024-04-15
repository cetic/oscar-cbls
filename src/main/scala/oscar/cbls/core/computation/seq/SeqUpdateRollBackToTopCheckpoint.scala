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

object SeqUpdateRollBackToTopCheckpoint {
  def apply(
    checkpoint: IntSequence,
    howToRollBack: SeqUpdate,
    level: Int
  ): SeqUpdateRollBackToTopCheckpoint = {
    new SeqUpdateRollBackToTopCheckpoint(checkpoint, howToRollBack, level)
  }

  def unapply(
    seqUpdateRollBackToCheckpoint: SeqUpdateRollBackToTopCheckpoint
  ): Option[(IntSequence, SeqUpdate, Int)] =
    Some(
      seqUpdateRollBackToCheckpoint.checkpoint,
      seqUpdateRollBackToCheckpoint.howToRollBack,
      seqUpdateRollBackToCheckpoint.level
    )
}

class SeqUpdateRollBackToTopCheckpoint(
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
    require(false, "Cannot reverse SeqUpdateRollBackToTopCheckpoint")
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

  override protected[seq] def regularize(maxPivot: Int): SeqUpdate = this

  override def toString: String =
    s"SeqUpdateRollBackToTopCheckpoint(level:$level checkpoint:$checkpoint)"
}
