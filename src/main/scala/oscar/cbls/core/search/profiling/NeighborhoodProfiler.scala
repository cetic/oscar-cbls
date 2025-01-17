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

package oscar.cbls.core.search.profiling

import oscar.cbls.core.search.Neighborhood
import oscar.cbls.core.search.profiling.profilingData.NeighborhoodProfilingData

/** Base profiler for [[oscar.cbls.core.search.SimpleNeighborhood]]. It also tracks the exploration
  * timing, the number of exploration, the selection duration...
  *
  * @param neighborhood
  *   The profiled neighborhood
  */
class NeighborhoodProfiler(override val neighborhood: Neighborhood)
    extends SearchProfiler(neighborhood) {

  override lazy val commonProfilingData: NeighborhoodProfilingData = NeighborhoodProfilingData()

  private var lastNeighborExploredAt: Long    = 0L
  private var firstNeighborSelection: Boolean = true

  // Updates neighbor selection data.
  def neighborSelected(): Unit = {
    val neighborSelectionAt = System.nanoTime()
    if (firstNeighborSelection) {
      val selectionDuration = neighborSelectionAt - startExplorationAt
      commonProfilingData.firstNeighborSelectionCounterInc()
      commonProfilingData.firstNeighborSelectionDurationPlus(selectionDuration)
      firstNeighborSelection = false
    } else {
      val selectionDuration = neighborSelectionAt - lastNeighborExploredAt
      commonProfilingData.notFirstNeighborSelectionCounterInc()
      commonProfilingData.notFirstNeighborSelectionDurationPlus(selectionDuration)
    }
  }

  override def avgTimeFirstNeighborSelection: String =
    commonProfilingData.avgTimeFirstNeighborSelectionMillis()
  override def avgTimeNotFirstNeighborSelection: String =
    commonProfilingData.avgTimeNotFirstNeighborSelectionMillis()
  override def nbExplored: String = commonProfilingData.nbExplored.toString
  override def avgTimeExplore: String =
    s"${(commonProfilingData.timeSpentMillis.toDouble / commonProfilingData.nbExplored * 1000).round / 1000.0}"

  override def explorationStarted(startValue: Long): Unit = {
    firstNeighborSelection = true
    super.explorationStarted(startValue)
  }

  // Updates neighbor exploration data.
  def neighborExplored(): Unit = {
    commonProfilingData.exploreInc()
    lastNeighborExploredAt = System.nanoTime()
  }

}
