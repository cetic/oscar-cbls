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

import oscar.cbls.core.search.{Move, NoMoveFound, SearchResult}

/** An Objective defines the conditions of acceptation of a new candidate solution during the
  * search. Any optimisation has at least one Objective.
  *
  * During the exploration, the search procedure tries some modifications of the solution (called
  * Move) leading to a new candidate solution. Then checks if this solution is acceptable given the
  * Objective's conditions.
  */
abstract class Objective {

  /** Creates a new Exploration instance. Must be called when starting an exploration. */
  def newExploration: Exploration
}

/** An Exploration is used by the neighborhood to find and keep the best candidate solution during
  * the exploration phase.
  *
  * Depending of the concrete implementation of the Exploration, the behavior and thus the kept
  * solution may vary.
  */
abstract class Exploration {

  /** Keeps the best move found during this exploration. Initialized at NoMoveFound. */
  protected var _toReturn: SearchResult = NoMoveFound

  /** Returns the best move found during this exploration */
  def toReturn: SearchResult = _toReturn

  /** Checks if the candidate solution match the acceptance conditions
    *
    * @param buildMove
    *   A function linking the solution value to the Move that leads to it (must be provided by the
    *   calling Neighborhood)
    */
  def checkNeighbor(buildMove: Long => Move): Unit
}
