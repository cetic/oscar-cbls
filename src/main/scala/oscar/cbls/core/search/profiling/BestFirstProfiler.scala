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

import oscar.cbls.core.search.{Neighborhood, NeighborhoodCombinator}

/** A selection profiler specialized for BestFirstProfiler
  *
  * @param combinator
  *   The profiled combinator
  * @param neighborhoods
  *   The list of supervised neighborhood
  */
case class BestFirstProfiler(
  override val combinator: NeighborhoodCombinator,
  override val neighborhoods: List[Neighborhood]
) extends SelectionProfiler(combinator, neighborhoods) {

  nbOccurrencePerIterationProfile("NbFirstFailedPerReset")

  def slopeForCombinators(neighborhoodId: Int, defaultIfNoCall: Long = Long.MaxValue): Long =
    if (totalTimeSpentSubN(neighborhoodId) == 0L) defaultIfNoCall
    else ((1000L * totalGainSubN(neighborhoodId)) / totalTimeSpentSubN(neighborhoodId)).toInt

  def nbFoundSubN(i: Int): Long           = profilers(i).commonProfilingData.nbFoundForSelection
  private def totalGainSubN(i: Int): Long = profilers(i).commonProfilingData.gainForSelection
  def totalTimeSpentMoveFoundSubN(i: Int): Long = profilers(
    i
  ).commonProfilingData.timeSpentMoveFoundMillisForSelection

  def firstFailed(): Unit = nbOccurrencePerIterationEventOccurred("NbFirstFailedPerReset")
  def resetSelectionNeighborhoodStatistics(): Unit = {
    nbOccurrencePerIterationNextIteration("NbFirstFailedPerReset")
    profilers.foreach(p => p.resetThisStatistics())
  }
}
