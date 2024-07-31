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

  override protected[seq] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = {
    require(requirement = false, "This should not happen")
    null
  }

  override protected[seq] def regularize(maxPivot: Int): SeqUpdate =
    SeqUpdateReleaseTopCheckpoint(prev, seq.regularizeToMaxPivot(maxPivot))

  override def toString: String =
    s"SeqUpdateReleaseTopCheckPoint(checkpoint:$seq prev:$prev)"
}
