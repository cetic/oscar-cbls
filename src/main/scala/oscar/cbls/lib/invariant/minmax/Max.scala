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

import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.SetVariable

/** The companion object of [[Max]] class. */
object Max {

  /** Creates a Max invariant.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   An array of variable on which to compute the maximum.
    * @param listenedVariablesIndices
    *   A SetVariable containing the indices of the input variables to be listened to calculate the
    *   maximum.
    * @param output
    *   The output IntVariable evaluating to `Max(input(i) | i in listenedVariablesIndices)`.
    * @param default
    *   The default value of the maximum.
    * @param bulkIdentifier
    *   A [[oscar.cbls.core.computation.IncredibleBulk]] is used when several Invariant listen to
    *   vars. Warning: [[oscar.cbls.core.computation.IncredibleBulk]] are distinguished only by
    *   their identifier. Be sure to use the same one if you're referencing the same variables.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(
    model: Store,
    input: Array[IntVariable],
    listenedVariablesIndices: SetVariable,
    output: IntVariable,
    default: Long = Long.MinValue + 1,
    bulkIdentifier: Option[String] = None,
    name: Option[String] = None
  ): Max = {
    new Max(model, input, listenedVariablesIndices, output, default, bulkIdentifier, name)
  }
}

/** Invariant which maintains `Max{input(i) | i in` `listenedVariablesIndices}`. Update is in
  * O(log(n)).
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   An array of variable on which to compute the maximum.
  * @param listenedVariablesIndices
  *   A SetVariable containing the indices of the input variables to be listened to calculate the
  *   maximum.
  * @param output
  *   The output IntVariable evaluating to `Max(input(i) | i in listenedVariablesIndices)`.
  * @param default
  *   The default value of the maximum.
  * @param bulkIdentifier
  *   A [[oscar.cbls.core.computation.IncredibleBulk]] is used when several Invariant listen to
  *   vars. Warning: [[oscar.cbls.core.computation.IncredibleBulk]] are distinguished only by their
  *   identifier. Be sure to use the same one if you're referencing the same variables.
  * @param name
  *   The (optional) name of the Invariant.
  */
class Max(
  model: Store,
  input: Array[IntVariable],
  listenedVariablesIndices: SetVariable,
  output: IntVariable,
  default: Long,
  bulkIdentifier: Option[String] = None,
  name: Option[String] = None
) extends Extremum(model, input, listenedVariablesIndices, output, default, bulkIdentifier, name) {

  override protected def ord(v: IntVariable): Long = {
    -v.value() // The biggest value must have the smallest priority in the heap
  }
}
