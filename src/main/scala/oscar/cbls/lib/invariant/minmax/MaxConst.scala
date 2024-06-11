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
import oscar.cbls.core.computation.integer.{IntConstant, IntVariable}
import oscar.cbls.core.computation.set.SetVariable

/** Companion object of [[MaxConst]] class. */
object MaxConst {

  /** Creates a [[MaxConst]] invariant.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   An array of [[IntConstant]]
    * @param listenedValuesIndices
    *   A [[SetVariable]] containing the indices of the input variables to be observed to calculate
    *   the extremum.
    * @param output
    *   The output [[IntVariable]].
    * @param maxBacklog
    *   The maximum number of postponed updates that doesn't affect the maximum.
    * @param name
    *   The name (optional) of your Invariant
    */
  def apply(
    model: Store,
    input: Array[IntConstant],
    listenedValuesIndices: SetVariable,
    output: IntVariable,
    maxBacklog: Int = Int.MinValue,
    name: Option[String] = None
  ): MaxConst = {
    new MaxConst(model, input, listenedValuesIndices, output, maxBacklog, name)
  }
}

/** [[oscar.cbls.core.computation.Invariant]] that maintains Max(input(i) | i in
  * listenedVariablesIndices). This invariant is lazy and maintains a todo list of postponed
  * updates. Update is in O (log(n)) in worst case. If the update does not impact the output, it is
  * postponed in O(1). Otherwise, it is performed in O(log(n)). When a removed index is considered
  * and does not impact the extremum, it goes in the backlog as well, to be removed later. It is
  * faster for neighborhood exploration with moves and backtracks.
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   An array of [[IntConstant]]
  * @param listenedValuesIndices
  *   A [[SetVariable]] containing the indices of the input variables to be observed to calculate
  *   the extremum.
  * @param output
  *   The output [[IntVariable]].
  * @param maxBacklog
  *   The maximum number of postponed updates that doesn't affect the maximum.
  * @param name
  *   The name (optional) of your Invariant
  */
class MaxConst(
  model: Store,
  input: Array[IntConstant],
  listenedValuesIndices: SetVariable,
  output: IntVariable,
  maxBacklog: Int = Int.MinValue,
  name: Option[String] = None
) extends ExtremumConst(
      model,
      input,
      listenedValuesIndices,
      output,
      Long.MinValue,
      maxBacklog,
      name
    ) {

  override protected def ord(v: IntVariable): Long = -v.value()

  override protected def notImpactingExtremum(newValue: IntConstant): Boolean = {
    output.pendingValue >= newValue.value()
  }
}
