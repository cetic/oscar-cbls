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

/** Companion object of [[MinConst]] class. */
object MinConst {

  /** Creates a [[MinConst]] invariant, which maintains `Min{input(i) | i in`
    * `listenedVariablesIndices}`. This invariant is lazy and maintains a todo list of postponed
    * updates. Update is in O (log(n)) in worst case. If the update does not impact the output, it
    * is postponed in O(1). Otherwise, it is performed in O(log(n)). When a removed index is
    * considered and does not impact the minimum, it goes in the backlog as well, to be removed
    * later. It is faster for neighborhood exploration with moves and backtracks.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   The constants on which to compute the minimum.
    * @param listenedValuesIndices
    *   A SetVariable containing the indices of the input variables to be observed to calculate the
    *   minimum.
    * @param output
    *   The output IntVariable evaluating to `Min(input(i) | i in listenedVariablesIndices)`.
    * @param maxBacklog
    *   The maximum number of postponed updates that doesn't affect the minimum.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(
    model: Store,
    input: Array[Long],
    listenedValuesIndices: SetVariable,
    output: IntVariable,
    maxBacklog: Int = Int.MaxValue,
    name: Option[String] = None
  ): MinConst = {
    new MinConst(model, input, listenedValuesIndices, output, maxBacklog, name)
  }
}

/** Invariant which maintains `Min{input(i) | i in` `listenedVariablesIndices}`. This invariant is
  * lazy and maintains a todo list of postponed updates. Update is in O (log(n)) in worst case. If
  * the update does not impact the output, it is postponed in O(1). Otherwise, it is performed in
  * O(log(n)). When a removed index is considered and does not impact the minimum, it goes in the
  * backlog as well, to be removed later. It is faster for neighborhood exploration with moves and
  * backtracks.
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   The constants on which to compute the minimum.
  * @param listenedValuesIndices
  *   A SetVariable containing the indices of the input variables to be observed to calculate the
  *   minimum.
  * @param output
  *   The output IntVariable evaluating to `Min(input(i) | i in listenedVariablesIndices)`.
  * @param maxBacklog
  *   The maximum number of postponed updates that doesn't affect the minimum.
  * @param name
  *   The (optional) name of the Invariant.
  */
class MinConst(
  model: Store,
  input: Array[Long],
  listenedValuesIndices: SetVariable,
  output: IntVariable,
  maxBacklog: Int = Int.MaxValue,
  name: Option[String] = None
) extends ExtremumConst(
      model,
      input,
      listenedValuesIndices,
      output,
      Long.MaxValue,
      maxBacklog,
      name
    ) {

  override protected def ord(v: Long): Long = v

  override protected def notImpactingExtremum(newValue: Long): Boolean = {
    output.pendingValue <= newValue
  }
}
