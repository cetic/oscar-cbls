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

/** Companion object of [[MinConst]] class. */
object MinConst {

  /** Creates a [[MinConst]] invariant.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   An array of [[IntConstant]]
    * @param cond
    *   A [[SetVariable]] containing the indices of the input variables to be observed to calculate
    *   the extremum.
    * @param output
    *   The output [[IntVariable]].
    * @param maxBacklog
    *   The maximum number of postponed updates that doesn't affect the minimum.
    * @param name
    *   The name (optional) of your Invariant
    */
  def apply(
    model: Store,
    input: Array[IntConstant],
    cond: SetVariable,
    output: IntVariable,
    maxBacklog: Int = Int.MaxValue,
    name: Option[String] = None
  ): MinConst = {
    new MinConst(model, input, cond, output, maxBacklog, name)
  }
}

/** [[oscar.cbls.core.computation.Invariant]] that maintains Min(input(i) | i in cond). This
  * invariant is lazy and maintains a todo list of postponed updates. Update is in O (log(n)) in
  * worst case. If the update does not impact the output, it is postponed in O(1). Otherwise, it is
  * performed in O(log(n)). It faster for neighborhood exploration with moves and backtracks.
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   An array of [[IntConstant]]
  * @param cond
  *   A [[SetVariable]] containing the indices of the input variables to be observed to calculate
  *   the extremum.
  * @param output
  *   The output [[IntVariable]].
  * @param maxBacklog
  *   The maximum number of postponed updates that doesn't affect the minimum.
  * @param name
  *   The name (optional) of your Invariant
  */
class MinConst(
  model: Store,
  input: Array[IntConstant],
  cond: SetVariable,
  output: IntVariable,
  maxBacklog: Int = Int.MaxValue,
  name: Option[String] = None
) extends ExtremumConst(model, input, cond, output, Long.MaxValue, maxBacklog, name) {

  override protected def ord(v: IntVariable): Long = v.value()

  override protected def notImpactingExtremum(newValue: IntConstant): Boolean = {
    output.value() <= newValue.value()
  }
}
