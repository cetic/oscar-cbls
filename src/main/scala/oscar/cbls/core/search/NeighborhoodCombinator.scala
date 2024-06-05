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

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.Objective
import oscar.cbls.core.search.profiling.CombinatorProfiler

abstract class NeighborhoodCombinator(
  neighborhoodCombinatorName: String,
  val subNeighborhoods: SimpleNeighborhood*
) extends Neighborhood(neighborhoodCombinatorName) {

  override val _searchProfiler: CombinatorProfiler = new CombinatorProfiler(this)

  override def getMove(objective: Objective,objValue: IntVariable): SearchResult

  override def reset(): Unit = {
    for (n <- subNeighborhoods) n.reset()
  }

  override def searchDisplay_=(searchDisplay: SearchDisplay): Unit = {
    for (n <- subNeighborhoods) n.searchDisplay_=(searchDisplay)
    super.searchDisplay_=(searchDisplay)
  }

  override def toString: String = this.getClass.getSimpleName

}
