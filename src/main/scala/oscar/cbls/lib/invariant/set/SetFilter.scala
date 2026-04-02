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

import oscar.cbls.core.computation.set.{SetNotificationTarget, SetVariable}
import oscar.cbls.core.computation.{Invariant, Store}

/** Companion object of the [[SetFilter]] class. */
object SetFilter {

  /** Creates a SetFilter invariant, which maintains `{x in input | predicate(x)}`
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   The elements we want to filter.
    * @param predicate
    *   The function that selects the values that must be included in the output.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(
    model: Store,
    input: SetVariable,
    predicate: Int => Boolean,
    name: String = "SetFilter"
  ): SetFilter = {
    val output: SetVariable = SetVariable(model, Set.empty)
    new SetFilter(model, input, output, predicate, if (name == "") None else Some(name))
  }
}

/** Invariant which maintains `{x in input | predicate(x)}`
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   The elements we want to filter.
  * @param output
  *   The SetVariable evaluating to `{x in input | predicate(x)}`
  * @param predicate
  *   The function that selects the values that must be included in the output.
  * @param name
  *   The (optional) name of the Invariant.
  */
class SetFilter(
  model: Store,
  input: SetVariable,
  output: SetVariable,
  predicate: Int => Boolean,
  name: Option[String]
) extends Invariant(model, name)
    with SetNotificationTarget {

  input.registerStaticallyAndDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)

  output := Set.empty
  for (x <- input.value()) {
    if (predicate(x)) output :+= x
  }

  /** Returns the output variable */
  def apply(): SetVariable = output

  override def notifySetChanges(
    setVariable: SetVariable,
    index: Int,
    addedElems: Iterable[Int],
    removedElems: Iterable[Int],
    oldValue: Set[Int],
    newValue: Set[Int]
  ): Unit = {

    for (added <- addedElems if predicate(added)) output :+= added

    for (removed <- removedElems if predicate(removed)) output :-= removed

  }

  override def checkInternals(): Unit = {
    val expected = input.pendingValue.filter(predicate)

    require(
      output.pendingValue == expected,
      s"""checkInternals fails in invariant ${name()}. output != {x in input | predicate(x)}.
         |output: ${output.pendingValue}
         |expected: $expected
         |""".stripMargin
    )
  }
}
