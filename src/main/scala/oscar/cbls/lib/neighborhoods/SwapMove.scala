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

package oscar.cbls.lib.neighborhoods

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.search.Move

/** Move that exchanges the values of two [[oscar.cbls.core.computation.integer.IntVariable]].
  *
  * @param first
  *   The first variable to swap.
  * @param second
  *   The second variable to swap.
  * @param objValueAfter
  *   The objective value of the neighbor. Used for comparison and validation.
  * @param neighborhoodName
  *   The name of the [[oscar.cbls.core.search.Neighborhood]] that generated this Move.
  */
case class SwapMove(
  first: IntVariable,
  second: IntVariable,
  override val objValueAfter: Long,
  override val
  neighborhoodName: String
) extends Move(objValueAfter, neighborhoodName) {

  override def commit(): Unit = {
    first :=: second
  }

  override def toString: String =
    s"SwapMove: $first swapped with $second. " + super.toString
}
