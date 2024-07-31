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

/** Companion object of SeqUpdateRollBackToTopCheckpoint
  */
object SeqUpdateRollBackToTopCheckpoint {
  def apply(
    checkpoint: IntSequence,
    howToRollBack: SeqUpdate,
    level: Int,
    prev: SeqUpdate
  ): SeqUpdateRollBackToTopCheckpoint = {
    new SeqUpdateRollBackToTopCheckpoint(checkpoint, howToRollBack, level, prev)
  }

  def unapply(
    seqUpdateRollBackToCheckpoint: SeqUpdateRollBackToTopCheckpoint
  ): Option[(IntSequence, SeqUpdate, Int, SeqUpdate)] =
    Some(
      seqUpdateRollBackToCheckpoint.checkpoint,
      seqUpdateRollBackToCheckpoint.howToRollBack,
      seqUpdateRollBackToCheckpoint.level,
      seqUpdateRollBackToCheckpoint.prev
    )
}

/** A SeqUpdate that roll back modification up to the top checkpoint of the SeqVariable.
  *
  * @param checkpoint
  *   The SeqVariable value at the top checkpoint
  * @param howToRollBack
  *   The batch of updates that can be used to roll back modification
  * @param level
  *   The level of the top checkpoint
  * @param prev
  *   The previous update of the batch
  */
class SeqUpdateRollBackToTopCheckpoint(
  val checkpoint: IntSequence,
  val howToRollBack: SeqUpdate,
  val level: Int,
  val prev: SeqUpdate
) extends SeqUpdate(checkpoint) {

  override protected[seq] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = this

  override protected[seq] def regularize(maxPivot: Int): SeqUpdate = this

  override def toString: String =
    s"SeqUpdateRollBackToTopCheckpoint(level:$level checkpoint:$checkpoint prev:$prev)"
}
