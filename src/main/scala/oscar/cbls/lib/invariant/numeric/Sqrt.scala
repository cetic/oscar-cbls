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

import scala.math.{sqrt, round}

/** The companion object of [[Sqrt]] */
object Sqrt{

  /**Create a [[Sqrt]] invariant.
   *
   * @param model
   *  The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
   * @param fromValue
   *  The listened [[IntVariable]].
   * @param toValue
   *  The [[IntVariable]] which contains √(fromValue).
   * @param name
   *   The name (optional) of your Invariant.
   */
  def apply(model: Store,
            fromValue: IntVariable,
            toValue: IntVariable,
            name: Option[String] = None): Sqrt = {
    new Sqrt(model, fromValue, toValue, name)
  }
}

/** [[oscar.cbls.core.computation.Invariant]] that maintain the square root of fromValue.
 * WARNING: We are only working with integers. The square root is rounded to the closest integer.
 *
 * @param model
 *  The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
 * @param fromValue
 *  The listened [[IntVariable]].
 * @param toValue
 *  The [[IntVariable]] which contains √(fromValue).
 * @param name
 *   The name (optional) of your Invariant.
 */
class Sqrt(model: Store,
           fromValue: IntVariable,
           toValue: IntVariable,
           name: Option[String] = None)
extends Int2Int(model, fromValue, toValue,(x: Long) => round(sqrt(x)), false, name)
{
  override def checkInternals(): Unit = {
    require(toValue.value() == round(sqrt(fromValue.value())),
      s"toValue != √(fromValue). fromValue: $fromValue - toValue: $toValue")
  }
}
