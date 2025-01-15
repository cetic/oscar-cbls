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

/** This trait must be extended by any [[oscar.cbls.core.propagation.PropagationElement]] listening
  * to a [[SeqVariable]]. Its only method will be used to notify the changes occurring to the
  * listened SeqVariable.
  */
trait SeqNotificationTarget {

  /** Notifies the listening [[oscar.cbls.core.propagation.PropagationElement]] that the listened
    * [[oscar.cbls.core.computation.seq.SeqVariable]] has changed.
    *
    * Implemented by the listening [[oscar.cbls.core.propagation.PropagationElement]]. Called by the
    * listened [[oscar.cbls.core.computation.seq.SeqVariable]].
    *
    * @param seqVariable
    *   The listened SeqVariable.
    * @param contextualVarIndex
    *   The optional index of the SeqVariable in the context of the listening Invariant. Default -1
    * @param changes
    *   A stacked list of SeqUpdate, the first one represents the latest update. Use its prev value
    *   to get the previous SeqUpdate...
    */
  def notifySeqChanges(seqVariable: SeqVariable, contextualVarIndex: Int, changes: SeqUpdate): Unit
}
