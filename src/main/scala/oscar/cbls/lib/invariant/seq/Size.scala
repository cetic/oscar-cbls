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

package oscar.cbls.lib.invariant.seq

import oscar.cbls.core.computation.{Invariant, Store}
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.seq.{SeqNotificationTarget, SeqUpdate, SeqVariable}

/** Companion object of the [[Size]] class.*/
object Size {

  /** Generates a Size Invariant based on the provided input and output variable.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   The SeqVariable whose size is maintained by this Invariant.
    * @param output
    *   The size of the SeqVariable.
    * @return
    *   A Size Invariant.
    */
  def apply(model: Store, input: SeqVariable, output: IntVariable): Size = {
    new Size(model, input, output)
  }
}

/** Invariant that maintains the size of a SeqVariable.
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   The SeqVariable whose size is maintained by this Invariant.
  * @param output
  *   The size of the SeqVariable.
  */
class Size(model: Store, input: SeqVariable, output: IntVariable)
    extends Invariant(model, Some(s"Size maintainer of ${input.name()}"))
    with SeqNotificationTarget {

  input.registerStaticallyAndDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)

  output := input.value.size

  /** Notifies the listening [[oscar.cbls.core.propagation.PropagationElement]] that the listened
    * [[oscar.cbls.core.computation.seq.SeqVariable]] has changed.
    *
    * Implemented by the listening [[oscar.cbls.core.propagation.PropagationElement]]. Called by the
    * listened [[oscar.cbls.core.computation.seq.SeqVariable]].
    *
    * @param v
    *   The listened SeqVariable.
    * @param contextualVarIndex
    *   The optional index of the SeqVariable in the context of the listening Invariant. Default -1
    * @param changes
    *   A stacked list of SeqUpdate, the first one represents the latest update. Use its prev value
    *   to get the previous SeqUpdate...
    */
  override def notifySeqChanges(v: SeqVariable, contextualVarIndex: Int, changes: SeqUpdate): Unit =
    output := changes.newValue.size

  /** Allows to check and debug propagation elements.
    *
    * This method can be called after the propagation according to the debug level of the
    * propagation structure (see [[oscar.cbls.core.propagation.PropagationStructure]]). It can be
    * used to check if the invariant worked properly by, for example, recomputing the value from
    * scratch.
    */
  override def checkInternals(): Unit = require(
    input.value.size == output.value(),
    s"Size does not correspond to reality. Should be: ${input.value.size} got ${output.value()}"
  )
}
