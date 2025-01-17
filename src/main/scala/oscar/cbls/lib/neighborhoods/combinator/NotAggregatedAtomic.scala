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

package oscar.cbls.lib.neighborhoods.combinator

import oscar.cbls.core.computation.objective.Objective
import oscar.cbls.core.search.{MoveFound, Neighborhood, NoMoveFound, SearchResult}

/** Variant of Atomic combinator that do not aggregate the moved performed by the input
  * neighborhood. It used [[CompositeMovesWithList]] for doing all the move in one shot.
  *
  * @param n
  *   The neighborhood to squash into a single move.
  * @param shouldStop
  *   Given the number of performed moves, determines whether we should continue searching for new
  *   moves.
  * @param neighborhoodCombinatorName
  *   The name of the neighborhood combinator.
  */
class NotAggregatedAtomic(
  n: Neighborhood,
  shouldStop: Int => Boolean,
  neighborhoodCombinatorName: String
) extends Atomic(n, shouldStop, neighborhoodCombinatorName) {

  override protected[this] def exploreCombinator(objective: Objective): SearchResult = {
    val objValue = objective.objValue

    val startObjective = objValue.pendingValue
    val startSolution  = objValue.model.extractSolution()

    n.reset()
    val improvingMoves = n.getAllMoves(objective, shouldStop)

    if (improvingMoves.isEmpty)
      NoMoveFound
    else {
      val endObjValue = objValue.pendingValue

      setObjectiveVerboseMode(
        objective,
        0
      ) // The rollback do not have to be displayed in the profiling
      commitMove(
        objective,
        LoadSolutionMove(startSolution, startObjective, "Rollback the objective value")
      )
      setObjectiveVerboseMode(objective, verbosityLevel)

      MoveFound(CompositeMovesWithList(improvingMoves, endObjValue, neighborhoodCombinatorName))
    }
  }
}
