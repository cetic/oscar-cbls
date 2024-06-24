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

package oscar.cbls.lib.invariant.logic

import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.{IntConstant, IntVariable}
import oscar.cbls.lib.invariant.numeric.Int2Int

/** Companion object of the [[ElementConst]] class. */
object ElementConst {

  /** Creates an ElementConst invariant.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   The elements that can be chosen.
    * @param index
    *   An IntVariable pointing to one of the input values.
    * @param output
    *   The IntVariable which contains input(index).
    * @param name
    *   The name (optional) of your Invariant.
    */
  def apply(
    model: Store,
    input: Array[IntConstant],
    index: IntVariable,
    output: IntVariable,
    name: Option[String] = None
  ): ElementConst = {
    new ElementConst(model, input, index, output, name)
  }
}

/** [[oscar.cbls.core.computation.Invariant]] that maintains input(index) where input is an array of
  * [[IntConstant]].
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   The elements that can be chosen.
  * @param index
  *   An IntVariable pointing to one of the input values.
  * @param output
  *   The IntVariable which contains input(index).
  * @param name
  *   The name (optional) of your Invariant.
  */
class ElementConst(
  model: Store,
  input: Array[IntConstant],
  index: IntVariable,
  output: IntVariable,
  name: Option[String] = None
) extends Int2Int(model, index, output, (i: Long) => input(i.toInt).value(), false, name) {}
