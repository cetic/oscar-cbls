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
import oscar.cbls.core.distributed.computation.{SearchConnector, StoreIndependentMove}
import oscar.cbls.core.search.Move

/** Move that assign a [[scala.Long]] value to an
  * [[oscar.cbls.core.computation.integer.IntVariable]].
  *
  * @param variable
  *   The variable to change.
  * @param index
  *   The index of the variable in the array
  * @param oldValue
  *   The value of the variable before the move
  * @param newValue
  *   The value to assign to `variable`.
  * @param objValueAfter
  *   The objective value of the neighbor. Used for comparison and validation.
  * @param neighborhoodName
  *   The name of the [[oscar.cbls.core.search.Neighborhood]] that generated this Move.
  */
case class AssignMove(
  variable: IntVariable,
  index: Int,
  oldValue: Long,
  newValue: Long,
  override val objValueAfter: Long,
  override val neighborhoodName: String
) extends Move(objValueAfter, neighborhoodName) {

  override def commit(): Unit = variable := newValue

  override def toString: String =
    s"AssignMove: $variable set to $newValue." + super.toString

  override def detachFromStore(searchConnector: SearchConnector) =
    StoreIndependentAssignMove(
      variable = searchConnector.detachVariable(variable),
      index = index,
      oldValue = oldValue,
      newValue = newValue,
      objValueAfter = objValueAfter,
      neighborhoodName = neighborhoodName
    )
}

case class StoreIndependentAssignMove(
  variable: Int,
  index: Int,
  oldValue: Long,
  newValue: Long,
  objValueAfter: Long,
  neighborhoodName: String
) extends StoreIndependentMove(objValueAfter) {
  override def attachMoveToStore(searchConnector: SearchConnector): Move = {
    AssignMove(
      variable = searchConnector.attachIntVarToStore(variable),
      index = index,
      oldValue = oldValue,
      newValue,
      objValueAfter,
      neighborhoodName
    )
  }
}
