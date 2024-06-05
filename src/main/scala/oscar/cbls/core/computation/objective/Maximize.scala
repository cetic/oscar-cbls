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

package oscar.cbls.core.computation.objective

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.search.{Move, MoveFound, NoMoveFound, SearchDisplay}

/** Companion object of Maximize */
object Maximize {
  def apply(
    objValue: IntVariable,
    mustBeZero: List[IntVariable] = List.empty,
    overApproximatedObjValue: Option[IntVariable] = None
  ): Maximize = {
    new Maximize(objValue, mustBeZero, overApproximatedObjValue)
  }
}

/** This Objective only accepts objValue whose value is greater than the latest accepted one.
  *
  * @param objValue
  *   The IntVariable representing the value maximized by this Objective
  * @param mustBeZero
  *   The list of strong constraints that have to be respected in order to evaluate and potentially
  *   accept the new objValue. This list can be empty.
  * @param overApproximatedObjValue
  *   An optional approximated IntVariable whose value is supposedly greater or equal to objValue.
  *   Used when the computation of the objValue is really expensive
  */
class Maximize(
  objValue: IntVariable,
  mustBeZero: List[IntVariable],
  overApproximatedObjValue: Option[IntVariable]
) extends Objective {

  override def worstValue: Long = Long.MinValue

  override def isValueNewBest(currentBest: Long, newValue: Long): Boolean =
    newValue > currentBest

  override def newExploration(searchDisplay: SearchDisplay): Exploration = new Exploration {
    private val oldObj: Long = objValue.value()

    private def checkNeighborOnApproximatedObjective(buildMove: Long => Move): Unit = {
      val newApproxObj = overApproximatedObjValue.get.value()
      toReturn match {
        case NoMoveFound if newApproxObj > oldObj =>
          checkNeighborOnRealObjective(buildMove)
        case m: MoveFound if newApproxObj > m.objAfter() =>
          checkNeighborOnRealObjective(buildMove)
        case _ if newApproxObj > Long.MinValue =>
          searchDisplay.moveExplored(() => buildMove(newApproxObj), valid = true)
        case _ =>
          searchDisplay.moveExplored(() => buildMove(newApproxObj))
      }
    }

    private def checkNeighborOnRealObjective(buildMove: Long => Move): Unit = {
      val newObj = objValue.value()
      toReturn match {
        case NoMoveFound if newObj > oldObj =>
          searchDisplay.moveExplored(
            () => buildMove(newObj),
            valid = true,
            newBest = true,
            saved = true
          )
          _toReturn = MoveFound(buildMove(newObj))
        case m: MoveFound if newObj > m.objAfter() =>
          searchDisplay.moveExplored(
            () => buildMove(newObj),
            valid = true,
            newBest = true,
            saved = true
          )
          _toReturn = MoveFound(buildMove(newObj))
        case _ if newObj > Long.MinValue =>
          searchDisplay.moveExplored(() => buildMove(newObj), valid = true)
        case _ =>
          searchDisplay.moveExplored(() => buildMove(newObj))
      }
    }

    /** Three steps :
      *   - Checks the strong constraints
      *   - Checks the overApproximatedObjValue
      *   - Checks the objValue
      *
      * @param buildMove
      *   A function linking the new objValue to the Move that leads to it (must be provided by the
      *   calling Neighborhood)
      */
    override def checkNeighbor(buildMove: Long => Move): Unit = {
      if (!mustBeZero.exists(_.value() > 0)) {
        overApproximatedObjValue match {
          case None => checkNeighborOnRealObjective(buildMove)
          case _    => checkNeighborOnApproximatedObjective(buildMove)
        }
      } else {
        searchDisplay.moveExplored(() => buildMove(objValue.value()))
      }
    }
  }

  override def toString: String = "Maximize"
}
