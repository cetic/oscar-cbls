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
}
