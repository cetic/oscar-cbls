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

/** Companion object of [[MaxConst]] class. */
object MaxConst {

  /** Creates a MaxConst invariant, which maintains `Max{input(i) | i in`
    * `listenedVariablesIndices}`. This invariant is lazy and maintains a todo list of postponed
    * updates. Update is in O (log(n)) in worst case. If the update does not impact the output, it
    * is postponed in O(1). Otherwise, it is performed in O(log(n)). When a removed index is
    * considered and does not impact the maximum, it goes in the backlog as well, to be removed
    * later. It is faster for neighborhood exploration with moves and backtracks.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   The constants on which to compute the maximum. (warning: Long.MinValue is forbidden in the
    *   input array)
    * @param listenedValuesIndices
    *   A SetVariable containing the indices of the input variables to be observed to calculate the
    *   maximum.
    * @param output
    *   The output IntVariable evaluating to `Max(input(i) | i in listenedVariablesIndices)`.
    * @param default
    *   The default value of the maximum.
    * @param maxBacklog
    *   The maximum number of postponed updates that doesn't affect the maximum.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(
    model: Store,
    input: Array[Long],
    listenedValuesIndices: SetVariable,
    output: IntVariable,
    default: Long = Long.MinValue + 1,
    maxBacklog: Int = Int.MaxValue,
    name: Option[String] = None
  ): MaxConst = {

    input.foreach(v => require(v != Long.MinValue, "Long.MinValue is not supported in MaxConst"))

    new MaxConst(model, input, listenedValuesIndices, output, default, maxBacklog, name)
  }
}

/** Invariant which maintains `Max{input(i) | i in` `listenedVariablesIndices}`. This invariant is
  * lazy and maintains a todo list of postponed updates. Update is in O (log(n)) in worst case. If
  * the update does not impact the output, it is postponed in O(1). Otherwise, it is performed in
  * O(log(n)). When a removed index is considered and does not impact the maximum, it goes in the
  * backlog as well, to be removed later. It is faster for neighborhood exploration with moves and
  * backtracks.
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   The constants on which to compute the maximum. (warning: Long.MinValue is forbidden in the
  *   input array)
  * @param listenedValuesIndices
  *   A SetVariable containing the indices of the input variables to be observed to calculate the
  *   maximum.
  * @param output
  *   The output IntVariable evaluating to `Max(input(i) | i in listenedVariablesIndices)`.
  * @param default
  *   The default value of the maximum.
  * @param maxBacklog
  *   The maximum number of postponed updates that doesn't affect the maximum.
  * @param name
  *   The (optional) name of the Invariant.
  */
class MaxConst(
  model: Store,
  input: Array[Long],
  listenedValuesIndices: SetVariable,
  output: IntVariable,
  default: Long,
  maxBacklog: Int = Int.MaxValue,
  name: Option[String] = None
) extends ExtremumConst(model, input, listenedValuesIndices, output, default, maxBacklog, name) {

  input.foreach(v => require(v != Long.MinValue, "Long.MinValue is not supported in MaxConst"))

  override protected def ord(v: Long): Long = -v

  override protected def notImpactingExtremum(newValue: Long): Boolean = {
    output.pendingValue >= newValue
  }
}
