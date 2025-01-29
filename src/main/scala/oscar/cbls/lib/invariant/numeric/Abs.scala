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

/** Companion object of [[Abs]] class. */
object Abs {

  /** Creates an Abs invariant, which maintains the absolute value of an
    * [[oscar.cbls.core.computation.integer.IntVariable]].
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   The listened IntVariable.
    * @param output
    *   The IntVariable evaluating to |input|.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(
    model: Store,
    input: IntVariable,
    output: IntVariable,
    name: Option[String] = None
  ): Abs = {
    new Abs(model, input, output, name)
  }

  /** Static method to obtain an IntVariable that is the result of `|a|`. */
  def result(a: IntVariable, name: Option[String] = None): IntVariable = {
    val out = IntVariable(a.model, a.value().abs)
    Abs(a.model, a, out, name)
    out

  }
}

/** Invariant which maintains the absolute value of an
  * [[oscar.cbls.core.computation.integer.IntVariable]].
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   The listened IntVariable.
  * @param output
  *   The IntVariable evaluating to `|input|`.
  * @param name
  *   The (optional) name of the Invariant.
  */
class Abs(model: Store, input: IntVariable, output: IntVariable, name: Option[String] = None)
    extends Int2Int(model, input, output, (x: Long) => x.abs, false, name) {}
