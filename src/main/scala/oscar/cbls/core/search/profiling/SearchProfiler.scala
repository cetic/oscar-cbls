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

import oscar.cbls.core.search.profiling.profilingData._
import oscar.cbls.core.search.Neighborhood

object SearchProfiler {
  def selectedStatisticInfo(profilers: Iterable[SearchProfiler]): String = {
    // TODO
    // Properties.justifyRightArray(profilers.toList.flatMap(p => List(p.collectThisProfileHeader,p.collectThisProfileData))).mkString("\n")
    ""
  }
}

/** This class purpose is to profile a Neighborhood during the search. By using the
  * explorationStarted and explorationEnded method you'll get various information as :
  *   - The total gain
  *   - Total call
  *   - The time spent _ ...
  *
  * Be aware that the explorationStarted and explorationEnded methods are called within the
  * Neighborhood.profiledGetMove() method so you don't have to call them manually if you use this
  * method instead of getMove()
  *
  * @param neighborhood
  *   : the profiled Neighborhood
  */
class SearchProfiler(val neighborhood: Neighborhood) {

  protected var startExplorationAt          = 0L
  protected var currentExplorationTimeSpent = 0L
  protected var explorationPausedAt         = 0L
  protected var explorationResumedAt        = 0L

  def subProfilers: List[SearchProfiler] = List.empty

  def explorationStarted(): Unit = {
    commonProfilingData.callInc()
    startExplorationAt = System.nanoTime()
    explorationPausedAt = 0L
    explorationResumedAt = 0L
    currentExplorationTimeSpent = 0L
  }

  // Pause the exploration (mandatory to have a proper exploration duration within the dynAndThen combinator)
  def explorationPaused(): Unit = {
    explorationPausedAt = System.nanoTime()
    currentExplorationTimeSpent += explorationPausedAt - Math.max(
      startExplorationAt,
      explorationResumedAt
    )
  }
  // Resume the exploration (mandatory to have a proper exploration duration within the dynAndThen combinator)
  def explorationResumed(): Unit = explorationResumedAt = System.nanoTime()

  def explorationEnded(gain: Option[Long]): Unit = {
    val timeSpent = currentExplorationTimeSpent + System.nanoTime() - Math.max(
      startExplorationAt,
      explorationResumedAt
    )
    if (gain.nonEmpty) {
      commonProfilingData.foundInc()
      commonProfilingData.gainPlus(gain.get)
      commonProfilingData.timeSpentMoveFoundPlus(timeSpent)
    } else {
      commonProfilingData.timeSpentNoMoveFoundPlus(timeSpent)
    }
  }

  // The object that holds the common profiling data (meaning nbCalls,AvgTimeMove,nbFound...)
  lazy val commonProfilingData: CommonProfilingData = new CommonProfilingData()

  private def gainPerCall: String = if (commonProfilingData.nbCalls == 0L) "NA"
  else s"${commonProfilingData.gain / commonProfilingData.nbCalls}"
  private def callDuration: String = if (commonProfilingData.nbCalls == 0L) "NA"
  else s"${commonProfilingData.timeSpentMillis / commonProfilingData.nbCalls}"
  private def slope: String = if (commonProfilingData.timeSpentMillis == 0L) "NA"
  else
    s"${1000 * (commonProfilingData.gain.toDouble / commonProfilingData.timeSpentMillis.toDouble).toLong}"
  private def avgTimeSpendNoMove: String = if (
    commonProfilingData.nbCalls - commonProfilingData.nbFound == 0L
  ) "NA"
  else
    s"${commonProfilingData.timeSpentNoMoveFoundMillis / (commonProfilingData.nbCalls - commonProfilingData.nbFound)}"
  private def avgTimeSpendMove: String = if (commonProfilingData.nbFound == 0L) "NA"
  else s"${commonProfilingData.timeSpentMoveFoundMillis / commonProfilingData.nbFound}"
  protected def nbExplored: String                       = "NA"
  protected def avgTimeExplore: String                   = "NA"
  protected def avgTimeFirstNeighborSelection: String    = "NA"
  protected def avgTimeNotFirstNeighborSelection: String = "NA"

  final def collectThisProfileStatistics: List[Array[String]] =
    List(collectThisProfileHeader, collectThisProfileData)
  final def collectThisProfileHeader: Array[String] = Array(
    "Neighborhood",
    "calls",
    "found",
    "explored",
    "sumGain",
    "sumTime(ms)",
    "avgGain",
    "avgTime(ms)",
    "slope(-/s)",
    "avgTimeNoMove",
    "avgTimeMove",
    "wastedTime",
    "avgTimeExplored(ms)",
    "avgFirstSelectionTime(ms)",
    "avgOtherSelectionTime(ms)"
  )
  final def collectThisProfileData: Array[String] = {
    Array[String](
      s"$neighborhood",
      s"${commonProfilingData.nbCalls}",
      s"${commonProfilingData.nbFound}",
      nbExplored,
      s"${commonProfilingData.gain}",
      s"${commonProfilingData.timeSpentMillis}",
      s"$gainPerCall",
      s"$callDuration",
      s"$slope",
      s"$avgTimeSpendNoMove",
      s"$avgTimeSpendMove",
      s"${commonProfilingData.timeSpentNoMoveFoundMillis}",
      avgTimeExplore,
      avgTimeFirstNeighborSelection,
      avgTimeNotFirstNeighborSelection
    )
  }

  def goodValueIndicator(): Array[Option[String]] = {
    // Neighborhood, calls, founds, explored, sumGain, sumTime, avgGain
    Array(
      None,
      None,
      Some("Max"),
      None,
      Some("Max"),
      Some("Min"),
      Some("Max"),
      // avgTime, slope, avgTimeNoMove, avgTimeMove, wastedTime, avtTimeExplored
      Some("Min"),
      Some("Max"),
      Some("Min"),
      Some("Min"),
      Some("Min"),
      Some("Min"),
      // avgFirstNeighborSelectionTime, avgNotFirstNeighborSelectionTime
      Some("Min"),
      Some("Min")
    )
  }

  // Use this method if you need to reset some statistics (but keeping the total statistics)
  // For instance when using BestSlopeFirst
  def resetThisStatistics(): Unit = commonProfilingData.resetStatisticsForSelection()
  // This method is used when you need to merge to IDENTICAL provider.
  // For instance with the DynAndThen generating dynamic neighborhood
  def merge(profiler: SearchProfiler): Unit = {
    commonProfilingData.merge(profiler.commonProfilingData)
  }

  override def toString: String = s"Profile(${neighborhood.toString})\n$commonProfilingData"
  // Get the detailedRecursiveName, the idea is to be able to distinguish two generated neighborhood no matter the depth
  def detailedRecursiveName: String = s"${neighborhood.toString}"
}
