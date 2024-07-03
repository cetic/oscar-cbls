
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

/** The first update of any stack of updates.
  *
  * A new SeqUpdateLastNotified is created when :
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
      expectedValueAfterFullReverse sameIdentity this.newValue,
      s"not proper reverse target on $this target:$expectedValueAfterFullReverse"
    )
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
      this.newValue sameIdentity previousUpdates.newValue,
      "illegal append operation; values do not match"
    )
    previousUpdates
  }

  override protected[seq] def regularize(maxPivot: Int): SeqUpdate = SeqUpdateLastNotified(
    value.regularizeToMaxPivot(maxPivot)
  )
}
