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

/** Companion object of SeqUpdateLastNotified
  */
object SeqUpdateLastNotified {
  def apply(sequence: IntSequence): SeqUpdateLastNotified =
    new SeqUpdateLastNotified(sequence)

  def unapply(seqUpdateLastNotified: SeqUpdateLastNotified): Option[IntSequence] =
    Some(seqUpdateLastNotified.newValue)
}

/** The first update of any batch of updates.
  *
  * A new SeqUpdateLastNotified is created when:
  *   - Propagating
  *   - Creating a SeqVariable
  *
  * @param value
  *   The starting IntSequence value of the stack of updates.
  */
class SeqUpdateLastNotified(value: IntSequence) extends SeqUpdate(value) {

  override protected[seq] def reverseThis(
    expectedValueAfterFullReverse: IntSequence,
    updatesAlreadyReversed: SeqUpdate
  ): SeqUpdate = {
    require(
      expectedValueAfterFullReverse sameIdentity this.newValue,
      s"SeqUpdateLastNotified value should be equals to targeted reversed value." +
        s"\nShould be : $expectedValueAfterFullReverse got $value"
    )
    updatesAlreadyReversed
  }

  override protected[seq] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = {
    require(
      this.newValue sameIdentity previousUpdates.newValue,
      "Illegal append operation; values do not match."
    )
    previousUpdates
  }

  override protected[seq] def regularize(maxPivot: Int): SeqUpdate = SeqUpdateLastNotified(
    value.regularizeToMaxPivot(maxPivot)
  )
}
