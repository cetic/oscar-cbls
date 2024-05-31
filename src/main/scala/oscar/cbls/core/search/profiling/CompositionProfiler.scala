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

/** A SearchProfiler specialized for Composition type combinator. It add the possibility to handle
  * dynamically generated neighborhood by merging their data (if compatible)
  *
  * @param combinator
  *   The profiled combinator
  * @param left
  *   [optional] the first Neighborhood (not dynamically generated)
  */
case class CompositionProfiler(
  override val combinator: NeighborhoodCombinator,
  left: Option[Neighborhood] = None
) extends CombinatorProfiler(combinator) {

  private var currentRightProfiler: SearchProfiler          = _
  private var dynNeighborhoodProfiler: List[SearchProfiler] = List.empty

  override def subProfilers: List[SearchProfiler] =
    (if (left.isDefined) List(left.get._searchProfiler) else List.empty) ++ dynNeighborhoodProfiler

  override def explorationPaused(): Unit = {
    explorationPausedAt = System.nanoTime()
    currentExplorationTimeSpent += explorationPausedAt - Math.max(
      startExplorationAt,
      explorationResumedAt
    )
    currentRightProfiler.explorationPaused()
  }

  override def explorationResumed(): Unit = {
    explorationResumedAt = System.nanoTime()
    currentRightProfiler.explorationResumed()
  }

  def setCurrentRight(profiler: SearchProfiler): Unit = currentRightProfiler = profiler

  /*
    1° merge common profiling data
    2° merge combinator specific statistics
    3° merge left neighborhood and dynamically generated one
   */
  override def merge(profiler: SearchProfiler): Unit = {
    val compositionProfiler = profiler.asInstanceOf[CompositionProfiler]
    commonProfilingData.merge(profiler.commonProfilingData)
    mergeSpecificStatistics(compositionProfiler)
    if (left.isDefined) left.get._searchProfiler.merge(compositionProfiler.left.get._searchProfiler)
    compositionProfiler.dynNeighborhoodProfiler.foreach(mergeDynProfiler)
  }

  /*
   * Within Composition combinator, neighborhood are generated during the search each time we use it,
   * meaning that a new profiler is created each time with initial value.
   * To ensure that we have proper value, we must merge the old profiler with the new one.
   * That's the purpose of this method.
   *
   * We first determine if we already encountered this type of neighborhood.
   * To do that we use the detailedRecursiveName of the neighborhood (basically getting all the structure of the neighborhood)
   *
   * We DON'T WANT TO MERGE when :
   *  - The B neighborhood isn't generic (when using andThen for instance) hence the eq check
   */
  def mergeDynProfiler(profiler: SearchProfiler = currentRightProfiler): Unit = {
    if (!dynNeighborhoodProfiler.exists(x => x eq profiler)) {
      val matchingProfilerOpt =
        dynNeighborhoodProfiler.find(x => x.detailedRecursiveName == profiler.detailedRecursiveName)
      if (matchingProfilerOpt.isDefined) matchingProfilerOpt.get.merge(profiler)
      else dynNeighborhoodProfiler = dynNeighborhoodProfiler :+ profiler
    }
  }
}
