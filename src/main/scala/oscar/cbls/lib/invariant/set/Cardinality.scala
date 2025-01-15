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

package oscar.cbls.lib.invariant.set

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.{Invariant, Store}
import oscar.cbls.core.computation.set.{SetNotificationTarget, SetVariable}

/** Companion object of the [[Cardinality]] class. */
object Cardinality {

  /** Creates an Cardinality invariant, which maintains the cardinality of `input`, `#input`.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   The SetVariable we want to know the cardinality.
    * @param output
    *   The IntVariable evaluating to `#input`.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(
    model: Store,
    input: SetVariable,
    output: IntVariable,
    name: Option[String] = None
  ): Cardinality = new Cardinality(model, input, output, name)
}

/** Invariant which maintains the cardinality of `input`, `#input`. Update is O(1).
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   The SetVariable we want to know the cardinality.
  * @param output
  *   The IntVariable evaluating to `#input`.
  * @param name
  *   The (optional) name of the Invariant.
  */
class Cardinality(model: Store, input: SetVariable, output: IntVariable, name: Option[String])
    extends Invariant(model, name)
    with SetNotificationTarget {

  input.registerStaticallyAndDynamicallyListeningElement(this)

  output.setDefiningInvariant(this)
  output := input.value().size

  override def notifySetChanges(
    setVariable: SetVariable,
    index: Int,
    addedElems: Iterable[Int],
    removedElems: Iterable[Int],
    oldValue: Set[Int],
    newValue: Set[Int]
  ): Unit = output := newValue.size

  override def checkInternals(): Unit = {
    require(
      output.pendingValue == input.value().size,
      s"checkInternals fails in invariant ${name()}. " +
        s"output != #input. " +
        s"output: ${output.pendingValue} - #input: ${input.value().size}"
    )
  }
}
