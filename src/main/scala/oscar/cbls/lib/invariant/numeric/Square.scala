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
object Square{

  /** Create a [[Square]] invariant.
   *
   * @param model
   *  The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
   * @param fromValue
   *  The listened [[IntVariable]].
   * @param toValue
   *  The [[IntVariable]] which contains fromValue².
   * @param name
   *   The name (optional) of your Invariant.
   */
  def apply(model: Store,
            fromValue: IntVariable,
            toValue: IntVariable,
            name: Option[String] = None): Square = {
    new Square(model, fromValue, toValue, name)
  }
}

/**[[oscar.cbls.core.computation.Invariant]] that maintains the square of fromValue
 *
 * @param model
 *  The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
 * @param fromValue
 *  The listened [[IntVariable]].
 * @param toValue
 *  The [[IntVariable]] which contains fromValue&#94;2.
 * @param name
 *   The name (optional) of your Invariant.
 */
class Square (model: Store,
              fromValue: IntVariable,
              toValue: IntVariable,
              name: Option[String] = None)
extends Int2Int(model, fromValue, toValue, (x: Long) => x*x, false, name){

  override def checkInternals(): Unit ={
    require(toValue.value() == -fromValue.value(),
      s"toValue != fromValue^2. fromValue: $fromValue - toValue: $toValue")
  }
}
