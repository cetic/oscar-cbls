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

import oscar.cbls.core.search._
import oscar.cbls.core.search.profiling.NeighborhoodProfiler

/** An Exploration is used by the neighborhood to find and keep the best new objective value during
  * the exploration phase.
  *
  * Depending on the concrete implementation of the Exploration, the behavior and thus the kept
  * objective value may vary.
  */
abstract class Exploration[M <: Move](
  val oldObj: Long,
  searchProfilerOpt: Option[NeighborhoodProfiler]
) {

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
  def checkNeighborWP(buildMove: Long => M): Unit = {
    checkNeighbor(buildMove)
    searchProfilerOpt.foreach(x => x.neighborExplored())
  }

  protected[objective] def checkNeighbor(buildMove: Long => M): Unit
}
