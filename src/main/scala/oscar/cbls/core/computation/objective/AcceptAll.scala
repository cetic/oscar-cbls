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
import oscar.cbls.core.search.profiling.{NeighborhoodProfiler, SearchProfiler}
import oscar.cbls.core.search.{Move, MoveFound, SimpleNeighborhood}

/** Companion object of AcceptAll */
object AcceptAll {

  /** This particular implementation of Objective accepts all movements no matter what (with respect
    * to strong constraint by default).
    *
    * It can be used in particular cases. For instance, in the Restart, the solution used for
    * restarting the search can worsen the objective value.
    * @param objValue
    *   The objective value.
    * @param mustBeZero
    *   The list of strong constraints that have to be respected in order to evaluate and
    *   potentially accept the new objValue. This list can be empty.
    * @param allowsConstrainViolation
    *   Should the objective accept solutions that violate some strong constraints. Set to `false`
    *   by default. <br> '''WARNING:''' if set to `true`, it can lead to chaotic behaviour of the
    *   search. For example, in a pick-up and delivery problem the search can be stuck in solution
    *   where pick-up point are positioned after the delivery points.
    */
  def apply(
    objValue: IntVariable,
    mustBeZero: List[IntVariable] = List.empty,
    allowsConstrainViolation: Boolean = false
  ): AcceptAll = {
    new AcceptAll(objValue, mustBeZero, allowsConstrainViolation)
  }
}

/** This particular implementation of Objective accepts all movements no matter what (with respect
  * to strong constraint by default).
  *
  * It can be used in particular cases. For instance, in the Restart, the solution used for
  * restarting the search can worsen the objective value.
  * @param objValue
  *   The objective value.
  * @param mustBeZero
  *   The list of strong constraints that have to be respected in order to evaluate and potentially
  *   accept the new objValue. This list can be empty.
  * @param allowsConstrainViolation
  *   Should the objective accept solutions that violate some strong constraints. <br>
  *   '''WARNING:''' if set to `true`, it can lead to chaotic behaviour of the search. For example,
  *   in a pick-up and delivery problem the search can be stuck in solution where pick-up point are
  *   positioned after the delivery points.
  */
class AcceptAll(
  objValue: IntVariable,
  mustBeZero: List[IntVariable],
  allowsConstrainViolation: Boolean
) extends Objective(objValue, mustBeZero) {

  override lazy val worstValue: Long = 0L

  override def isValueNewBest(currentBest: Long, newValue: Long): Boolean = true

  override def newExploration[M <: Move](
    searchProfilerOpt: Option[NeighborhoodProfiler]
  ): Exploration[M] =
    new Exploration[M](currentObjValue(), searchProfilerOpt) {
      override def checkNeighbor(buildMove: Long => M): Unit = {
        val newValue = objValue.value()
        if (!allowsConstrainViolation && mustBeZero.exists(_.value() != 0L)) {
          verboseMode.moveExplored(() => buildMove(newValue))
        } else {
          _toReturn = MoveFound(buildMove(newValue))
          verboseMode.moveExplored(() => buildMove(newValue), valid = true, saved = true)
        }
      }
    }

  override def toString: String =
    s"Accept all movements ${if (allowsConstrainViolation) "even those violating constraints."
      else " that does not violate strong constraints."}"
}
