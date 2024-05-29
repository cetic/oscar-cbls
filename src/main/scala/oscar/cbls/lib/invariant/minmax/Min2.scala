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
import oscar.cbls.lib.invariant.numeric.IntInt2Int

/** Companion object of [[Min2]] class. */
object Min2{

  /** Create a [[Min2]] invariant.
   *
   * @param model
   *  The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
   * @param a
   *  The first parameter of the function.
   * @param b
   *  The second parameter of the function.
   * @param output
   *  The [[IntVariable]] which contains min(a, b).
   * @param name
   *   The name (optional) of your Invariant.
   */
  def apply(model: Store,
            a: IntVariable,
            b: IntVariable,
            output: IntVariable,
            name: Option[String] = None): Min2 = {
    new Min2(model, a, b, output, name)
  }
}

/** [[oscar.cbls.core.computation.Invariant]] that maintains the minimum of two [[IntVariable]].
 *
 * @param model
 *  The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
 * @param a
 *  The first parameter of the function.
 * @param b
 *  The second parameter of the function.
 * @param output
 *  The [[IntVariable]] which contains min(a, b).
 * @param name
 *   The name (optional) of your Invariant.
 */
class Min2(model: Store,
           a: IntVariable,
           b: IntVariable,
           output: IntVariable,
           name: Option[String] = None)
extends IntInt2Int(model, a, b, output, (x: Long, y: Long) => x.min(y), name){}
