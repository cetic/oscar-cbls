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

package oscar.cbls.lib.invariant.minmax

import oscar.cbls.core.computation.{IncredibleBulk, Invariant, Store}
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.SetVariable

/** The companion object of [[Max]] class. */
object Max {

  /** Creates a Max invariant.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   The elements on which to compute the maximum.
    * @param listenedVariablesIndices
    *   A SetVariable containing the indices of the input variables to be listened to calculate the
    *   extremum.
    * @param output
    *   The output IntVariable containing Max{input(i) | i in listenedVariablesIndices}.
    * @param bulkIdentifier
    *   A [[oscar.cbls.core.computation.IncredibleBulk]] is used when several
    *   [[oscar.cbls.core.computation.Invariant]] listen to vars. Warning:
    *   [[oscar.cbls.core.computation.IncredibleBulk]] are distinguished only by their identifier.
    *   Be sure to use the same one if you're referencing the same variables.
    * @param name
    *   The name (optional) of your Invariant.
    */
  def apply(
    model: Store,
    input: Array[IntVariable],
    listenedVariablesIndices: SetVariable,
    output: IntVariable,
    bulkIdentifier: Option[String] = None,
    name: Option[String] = None
  ): Max = {
    new Max(model, input, listenedVariablesIndices, output, bulkIdentifier, name)
  }
}

/** [[oscar.cbls.core.computation.Invariant]] that maintains Max{input(i) | i in
  * listenedVariablesIndices}. Update is in O(log(n))
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   The elements on which to compute the maximum.
  * @param listenedVariablesIndices
  *   A SetVariable containing the indices of the input variables to be listened to calculate the
  *   extremum.
  * @param output
  *   The output IntVariable containing Max{input(i) | i in listenedVariablesIndices}.
  * @param bulkIdentifier
  *   A [[oscar.cbls.core.computation.IncredibleBulk]] is used when several
  *   [[oscar.cbls.core.computation.Invariant]] listen to vars. Warning:
  *   [[oscar.cbls.core.computation.IncredibleBulk]] are distinguished only by their identifier. Be
  *   sure to use the same one if you're referencing the same variables.
  * @param name
  *   The name (optional) of your Invariant.
  */
class Max(
  model: Store,
  input: Array[IntVariable],
  listenedVariablesIndices: SetVariable,
  output: IntVariable,
  bulkIdentifier: Option[String] = None,
  name: Option[String] = None
) extends Extremum(
      model,
      input,
      listenedVariablesIndices,
      output,
      Long.MinValue,
      bulkIdentifier,
      name
    ) {

  override protected def ord(v: IntVariable): Long = {
    -v.value() // The biggest value must have the smallest priority in the heap
  }
}
