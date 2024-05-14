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
import oscar.cbls.core.search.{Move, MoveFound, NoMoveFound}

/** Companion object of Minimize */
object Minimize {
  def apply(
    solutionValue: IntVariable,
    mustBeZero: List[IntVariable] = List.empty,
    underApproximatedSolutionValue: Option[IntVariable] = None
  ): Minimize = {
    solutionValue.model.registerForPartialPropagation(solutionValue)
    mustBeZero.foreach(mbz => mbz.model.registerForPartialPropagation(mbz))
    underApproximatedSolutionValue.foreach(ao => ao.model.registerForPartialPropagation(ao))
    new Minimize(solutionValue, mustBeZero, underApproximatedSolutionValue)
  }
}

/** This Objective only accepts candidate solution whose value is lesser than the latest accepted
  * solution.
  *
  * @param solutionValue
  *   The IntVariable representing the solution value
  * @param mustBeZero
  *   The list of strong constraints that have to be respected in order to accept the solution. Can
  *   be empty.
  * @param underApproximatedSolutionValue
  *   An optional approximated IntVariable whose value is supposedly lesser or equal to
  *   solutionValue. Used when the computation of the solutionValue is really expensive
  */
class Minimize(
  solutionValue: IntVariable,
  mustBeZero: List[IntVariable],
  underApproximatedSolutionValue: Option[IntVariable]
) extends Objective {

  override def newExploration: Exploration = new Exploration {
    private val oldObj: Long = solutionValue.value()

    private def checkNeighborOnApproximatedObjective(buildMove: Long => Move): Unit = {
      val newApproxObj = underApproximatedSolutionValue.get.value()
      toReturn match {
        case NoMoveFound if newApproxObj < oldObj =>
          checkNeighborOnRealObjective(buildMove)
        case m: MoveFound if newApproxObj < m.objAfter() =>
          checkNeighborOnRealObjective(buildMove)
        case _ => ;
      }
    }

    private def checkNeighborOnRealObjective(buildMove: Long => Move): Unit = {
      val newObj = solutionValue.value()
      toReturn match {
        case NoMoveFound if newObj < oldObj        => _toReturn = MoveFound(buildMove(newObj))
        case m: MoveFound if newObj < m.objAfter() => _toReturn = MoveFound(buildMove(newObj))
        case _                                     => ;
      }
    }

    /** Three steps :
      *   - Checks the strong constraints
      *   - Checks the underApproximatedSolutionValue
      *   - Checks the solutionValue
      *
      * @param buildMove
      *   A function linking the solution value to the Move that leads to it (must be provided by
      *   the calling Neighborhood)
      */
    override def checkNeighbor(buildMove: Long => Move): Unit = {
      if (!mustBeZero.exists(_.value() > 0)) {
        if (underApproximatedSolutionValue.isDefined)
          checkNeighborOnApproximatedObjective(buildMove)
        else checkNeighborOnRealObjective(buildMove)
      }
    }
  }
}
