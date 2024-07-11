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

/** The companion object of the [[MinSet]] class. */
object MinSet {

  /** Creates a MinSet invariant.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   The set of integer on which to compute the minimum.
    * @param output
    *   An IntVariable evaluating to the minimum of the input set.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(
    model: Store,
    input: SetVariable,
    output: IntVariable,
    name: Option[String] = None
  ): MinSet = {
    new MinSet(model, input, output, name)
  }
}

/** [[oscar.cbls.core.computation.Invariant]] that maintains `Min(input)`.
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   The set of integer on which to compute the minimum.
  * @param output
  *   An IntVariable evaluating to the minimum of the input set.
  * @param name
  *   The (optional) name of the Invariant.
  */
class MinSet(model: Store, input: SetVariable, output: IntVariable, name: Option[String] = None)
    extends ExtremumSet(model, input, output, Int.MaxValue, name) {

  /** Checks if a is better than b */
  override protected def better(a: Long, b: Long): Boolean = a < b

  override protected[this] def performPropagation(): Unit = {
    if (input.value().isEmpty) output := default
    else output                       := input.value().min
  }
}
