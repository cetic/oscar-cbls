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

package oscar.cbls.core.search

import oscar.cbls.core.computation.objective.{Exploration, Objective}
import oscar.cbls.core.search.profiling.NeighborhoodProfiler

/** Abstract class for simple neighborhoods.
  *
  * A simple neighborhood is a [[Neighborhood]] that is not composed.
  *
  * @param neighborhoodName
  *   The name of the Neighborhood
  */
abstract class SimpleNeighborhood[M <: Move](neighborhoodName: String)
    extends Neighborhood(neighborhoodName) {

  private var _searchProfilerOpt: Option[NeighborhoodProfiler] = None

  override def searchProfiler(): Option[NeighborhoodProfiler] = _searchProfilerOpt

  override def profileSearch(): Unit = _searchProfilerOpt match {
    case None => _searchProfilerOpt = Some(new NeighborhoodProfiler(this))
    case _    => ;
  }

  override def getMove(objective: Objective): SearchResult = {
    val exploration = {
      objective.explorationStarted(this)
      objective.newExploration[M](searchProfiler())
    }
    exploreNeighborhood(exploration)
    objective.explorationEnded(this, exploration.toReturn)
    exploration.toReturn
  }

  /** Explores this SimpleNeighborhood. This is where you put the logic of your SimpleNeighborhood.
    *
    * @param exploration
    *   The Exploration instance that will validate (or not) each explored neighbor.
    */
  def exploreNeighborhood(exploration: Exploration[M]): Unit

  /** Commits the move */
  def doMove(move: M): Unit

}
