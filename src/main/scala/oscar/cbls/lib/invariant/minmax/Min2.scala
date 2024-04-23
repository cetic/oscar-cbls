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
import oscar.cbls.lib.invariant.IntIntToIntInvariant

/** The invariant that maintains the minimum of two integer
 *
 * @param model
 *    The [[oscar.cbls.core.propagation.PropagationStructure]] to which this Invariant is linked
 * @param a
 *    The first [[IntVariable]] input
 * @param b
 *    The second [[IntVariable]] input
 * @param output
 *    The [[IntVariable]] that contains min(a, b)
 * @param name
 *    The (optional) name of the invariant
 */
class Min2(model : Store,
           a: IntVariable,
           b: IntVariable,
           output: IntVariable,
           name: Option[String] = None) extends
  IntIntToIntInvariant(model, a, b, output, (x: Long, y: Long) => x.min(y), name){}

object Min2{
  def apply(model: Store, a: IntVariable, b: IntVariable, output: IntVariable): Min2 = {
    new Min2(model, a, b, output)
  }

  def apply(model: Store, a: IntVariable, b: IntVariable, output:IntVariable, name: String): Min2 = {
    new Min2(model, a, b, output, Some(name))
  }
}
