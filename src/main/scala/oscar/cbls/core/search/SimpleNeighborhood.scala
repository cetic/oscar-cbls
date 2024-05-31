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
import oscar.cbls.core.search.profiling.{NeighborhoodProfiler, SearchProfiler}

/** Abstract class for simple neighborhoods.
  *
  * A simple neighborhood is a [[Neighborhood]] that is not composed.
  *
  * @param neighborhoodName
  *   The name of the Neighborhood
  */
abstract class SimpleNeighborhood(neighborhoodName: String) extends Neighborhood(neighborhoodName) {

  override val _searchProfiler: NeighborhoodProfiler = new NeighborhoodProfiler(this)

  override def getMove(objective: Objective): SearchResult = {
    _searchDisplay.startExploration(neighborhoodName)
    val exploration = objective.newExploration
    exploreNeighborhood(exploration)
    exploration.toReturn
  }

  def exploreNeighborhood(exploration: Exploration): Unit

}
