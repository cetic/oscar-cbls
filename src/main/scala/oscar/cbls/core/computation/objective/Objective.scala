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

/** An Objective defines the conditions of acceptation of a new objective value during the search.
  * Any optimisation has at least one Objective usually to minimize or maximize a defined value.
  * Most of the time, this value represents the value of a Solution (a defined state of the
  * problem).
  *
  * During the exploration, the search procedure tries some modifications of the solution (called
  * Move) leading to the modification of the objective value. To accept those moves the Objective
  * has to checks if the new value meets the Objective's conditions.
  */
abstract class Objective {

  /** Creates a new Exploration instance. Must be called when starting an exploration. */
  def newExploration: Exploration

  /** Returns the worst value that the objective value could have considering the Objective. */
  def worstValue: Long

  /** Returns true if newValue is a better value than currentBest.
    *
    * Depending on used Objective this information may vary
    *
    * @param currentBest
    *   The current best value (has to be given by the caller)
    * @param newValue
    *   The considered new value
    * @return
    *   True if newValue is better than currentBest
    */
  def isValueNewBest(currentBest: Long, newValue: Long): Boolean
}

/** An Exploration is used by the neighborhood to find and keep the best new objective value during
  * the exploration phase.
  *
  * Depending of the concrete implementation of the Exploration, the behavior and thus the kept
  * objective value may vary.
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
