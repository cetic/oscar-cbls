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


/** The companion object of [[Square]] class.
 */
object Square {

  /** Creates a [[Square]] invariant.
   *
   * @param model
   *  The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
   * @param input
   *  The listened [[IntVariable]].
   * @param output
   *  The [[IntVariable]] which contains input&#94;2.
   * @param name
   *   The name (optional) of your Invariant.
   */
  def apply(model: Store,
            input: IntVariable,
            output: IntVariable,
            name: Option[String] = None): Square = {
    new Square(model, input, output, name)
  }
}

/**[[oscar.cbls.core.computation.Invariant]] that maintains the square of an [[IntVariable]]
 *
 * @param model
 *  The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
 * @param input
 *  The listened [[IntVariable]].
 * @param output
 *  The [[IntVariable]] which contains input&#94;2.
 * @param name
 *   The name (optional) of your Invariant.
 */
class Square(model: Store,
             input: IntVariable,
             output: IntVariable,
             name: Option[String] = None)
extends Int2Int(model, input, output, (x: Long) => x*x, false, name){}
