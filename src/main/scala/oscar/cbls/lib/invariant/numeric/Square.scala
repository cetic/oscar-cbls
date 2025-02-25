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

package oscar.cbls.lib.invariant.numeric

import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import scalafx.scene.effect.Inputed

/** The companion object of [[Square]] class.
  */
object Square {

  /** Creates a Square invariant.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   The listened IntVariable.
    * @param output
    *   The IntVariable evaluating to `input^2`.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(
    model: Store,
    input: IntVariable,
    output: IntVariable,
    name: Option[String] = None
  ): Square = {
    new Square(model, input, output, name)
  }

  /** Static method to obtain an IntVariable that is the result of `a^2` */
  def result(a: IntVariable, name: Option[String] = None): IntVariable = {
    val output = IntVariable(a.model, a.value() * a.value())
    Square(a.model, a, output, name)
    output
  }
}

/** Invariant that maintains the square of an [[oscar.cbls.core.computation.integer.IntVariable]]
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   The listened IntVariable.
  * @param output
  *   The IntVariable evaluating to (input)^2^.
  * @param name
  *   The (optional) name of the Invariant.
  */
class Square(model: Store, input: IntVariable, output: IntVariable, name: Option[String] = None)
    extends Int2Int(model, input, output, (x: Long) => x * x, false, name) {}
