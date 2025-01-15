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
import oscar.cbls.core.search.profiling.NeighborhoodProfiler
import oscar.cbls.core.search.{Move, MoveFound, NoMoveFound, SimpleNeighborhood}

/** Companion object of Minimize */
object Minimize {
  def apply(
    objValue: IntVariable,
    mustBeZero: List[IntVariable] = List.empty,
    underApproximatedObjValue: Option[IntVariable] = None
  ): Minimize = {
    new Minimize(objValue, mustBeZero, underApproximatedObjValue)
  }
}

/** This Objective only accepts objValue whose value is lesser than the latest accepted one.
  *
  * @param objValue
  *   The IntVariable representing the value minimized by this Objective
  * @param mustBeZero
  *   The list of strong constraints that have to be respected in order to evaluate and potentially
  *   accept the new objValue. This list can be empty.
  * @param underApproximatedObjValue
  *   An optional approximated IntVariable whose value is supposedly lesser or equal to objValue.
  *   Used when the computation of the objValue is really expensive
  */
class Minimize(
  objValue: IntVariable,
  mustBeZero: List[IntVariable],
  underApproximatedObjValue: Option[IntVariable]
) extends Objective(objValue, mustBeZero) {

  override lazy val worstValue: Long = Long.MaxValue

  override def isValueNewBest(currentBest: Long, newValue: Long): Boolean =
    newValue < currentBest

  override def newExploration[M <: Move](
    searchProfilerOpt: Option[NeighborhoodProfiler]
  ): Exploration[M] =
    new Exploration[M](currentObjValue(), searchProfilerOpt) {

      private def checkNeighborOnApproximatedObjective(buildMove: Long => M): Unit = {
        val newApproxObj = underApproximatedObjValue.get.value()
        toReturn match {
          case NoMoveFound if newApproxObj < this.oldObj =>
            checkNeighborOnRealObjective(buildMove)
          case m: MoveFound if newApproxObj < m.objAfter() =>
            checkNeighborOnRealObjective(buildMove)
          case _ if newApproxObj < Long.MaxValue =>
            verboseMode.moveExplored(() => buildMove(newApproxObj), valid = true)
          case _ =>
            verboseMode.moveExplored(() => buildMove(newApproxObj))
        }
      }

      private def checkNeighborOnRealObjective(buildMove: Long => M): Unit = {
        val newObj = objValue.value()
        toReturn match {
          case NoMoveFound if newObj < oldObj =>
            verboseMode.moveExplored(
              () => buildMove(newObj),
              valid = true,
              newBest = true,
              saved = true
            )
            _toReturn = MoveFound(buildMove(newObj))
          case m: MoveFound if newObj < m.objAfter() =>
            verboseMode.moveExplored(
              () => buildMove(newObj),
              valid = true,
              newBest = true,
              saved = true
            )
            _toReturn = MoveFound(buildMove(newObj))
          case _ if newObj < Long.MaxValue =>
            verboseMode.moveExplored(() => buildMove(newObj), valid = true)
          case _ =>
            verboseMode.moveExplored(() => buildMove(newObj))
        }
      }

      /** Three steps :
        *   - Checks the strong constraints
        *   - Checks the underApproximatedObjValue
        *   - Checks the objValue
        *
        * @param buildMove
        *   A function linking the new objValue to the Move that leads to it (must be provided by
        *   the calling Neighborhood)
        */
      override def checkNeighbor(buildMove: Long => M): Unit = {
        if (!mustBeZero.exists(_.value() > 0)) {
          underApproximatedObjValue match {
            case None => checkNeighborOnRealObjective(buildMove)
            case _    => checkNeighborOnApproximatedObjective(buildMove)
          }
        } else {
          verboseMode.moveExplored(() => buildMove(objValue.value()))
        }
      }
    }

  override def toString: String = "Minimize"
}
