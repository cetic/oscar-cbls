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
import oscar.cbls.core.search.{MoveFound, Neighborhood, NoMoveFound, SearchResult}

/** Profiles a Neighborhood during the search.
  *
  * Its usage is based on several methods, two of them are already set at the right place :
  *   - explorationStarted : Right before exploring [[oscar.cbls.core.search.SimpleNeighborhood]] or
  *     [[oscar.cbls.core.search.NeighborhoodCombinator]]
  *   - explorationEnded : Right after exploring [[oscar.cbls.core.search.SimpleNeighborhood]] or
  *     [[oscar.cbls.core.search.NeighborhoodCombinator]]
  *
  * Some less common manipulation are the following : explorationPaused/Resumed. Those are presents
  * within [[oscar.cbls.core.computation.objective.composite.CompositeLeftStub]]. They are used when
  * you need to pause the exploration. For instance in the DynAndThen, the exploration of the left
  * Neighborhood is not finished, but we need to explore the right Neighborhood.
  *
  * @param neighborhood
  *   The profiled Neighborhood
  */
class SearchProfiler(val neighborhood: Neighborhood) {

  protected var startExplorationAt          = 0L
  protected var currentExplorationTimeSpent = 0L
  protected var explorationPausedAt         = 0L
  protected var explorationResumedAt        = 0L

  private var startValue: Long = 0L

  // The object that holds the common profiling data (meaning nbCalls,AvgTimeMove,nbFound...)
  lazy val commonProfilingData: CommonProfilingData = new CommonProfilingData()

  /** Returns the profilers of the sub-neighborhoods. Empty if neighborhood is a SimpleNeighborhood
    */
  def subProfilers: List[SearchProfiler] = List.empty

  /** Initializes and updates some variables to profile the exploration */
  def explorationStarted(startValue: Long): Unit = {
    this.startValue = startValue
    commonProfilingData.callInc()
    startExplorationAt = System.nanoTime()
    explorationPausedAt = 0L
    explorationResumedAt = 0L
    currentExplorationTimeSpent = 0L
  }

  /** Pauses the exploration timer (needed for some combinator ex: dynAndThen) */
  def explorationPaused(): Unit = {
    explorationPausedAt = System.nanoTime()
    currentExplorationTimeSpent += explorationPausedAt - Math.max(
      startExplorationAt,
      explorationResumedAt
    )
  }

  /** Resumes the exploration timer (needed for some combinator ex: dynAndThen) */
  def explorationResumed(): Unit = explorationResumedAt = System.nanoTime()

  /** Saves exploration data */
  def explorationEnded(explorationResult: SearchResult): Unit = {
    val timeSpent = currentExplorationTimeSpent + System.nanoTime() - Math.max(
      startExplorationAt,
      explorationResumedAt
    )
    explorationResult match {
      case NoMoveFound =>
        commonProfilingData.timeSpentNoMoveFoundPlus(timeSpent)
      case mf: MoveFound =>
        commonProfilingData.foundInc()
        commonProfilingData.gainPlus(Math.abs(startValue - mf.objAfter()))
        commonProfilingData.timeSpentMoveFoundPlus(timeSpent)
    }
  }

  private def gainPerCall: String = {
    if (commonProfilingData.nbCalls == 0L) "NA"
    else s"${commonProfilingData.gain / commonProfilingData.nbCalls}"
  }
  private def callDuration: String = {
    if (commonProfilingData.nbCalls == 0L) "NA"
    else s"${commonProfilingData.timeSpentMillis / commonProfilingData.nbCalls}"
  }
  private def slope: String = {
    if (commonProfilingData.timeSpentMillis == 0L) "NA"
    else
      s"${(1000.0 * commonProfilingData.gain / commonProfilingData.timeSpentMillis.toDouble).toLong}"
  }
  private def avgTimeSpendNoMove: String = {
    if (commonProfilingData.nbCalls - commonProfilingData.nbFound == 0L) "NA"
    else
      s"${commonProfilingData.timeSpentNoMoveFoundMillis / (commonProfilingData.nbCalls - commonProfilingData.nbFound)}"
  }
  private def avgTimeSpendMove: String = {
    if (commonProfilingData.nbFound == 0L) "NA"
    else s"${commonProfilingData.timeSpentMoveFoundMillis / commonProfilingData.nbFound}"
  }
  protected def nbExplored: String                       = "NA"
  protected def avgTimeExplore: String                   = "NA"
  protected def avgTimeFirstNeighborSelection: String    = "NA"
  protected def avgTimeNotFirstNeighborSelection: String = "NA"

  // Returns headers (for display purpose).
  final def collectThisProfileHeader: List[String] = List(
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
  // Returns collected data (mainly for display purpose).
  final def collectThisProfileData(hideGainRelatedData: Boolean = false): List[String] = {
    List[String](
      s"$neighborhood",
      s"${commonProfilingData.nbCalls}",
      s"${commonProfilingData.nbFound}",
      nbExplored,
      if (hideGainRelatedData) "NA" else s"${commonProfilingData.gain}",
      s"${commonProfilingData.timeSpentMillis}",
      if (hideGainRelatedData) "NA" else s"$gainPerCall",
      s"$callDuration",
      if (hideGainRelatedData) "NA" else s"$slope",
      s"$avgTimeSpendNoMove",
      s"$avgTimeSpendMove",
      s"${commonProfilingData.timeSpentNoMoveFoundMillis}",
      avgTimeExplore,
      avgTimeFirstNeighborSelection,
      avgTimeNotFirstNeighborSelection
    )
  }

  /** Defines the quality of a profiled value.
    *
    * For instance, it's best to have a lot of found moves and a little time spend :
    *   - Max => the more, the better
    *   - Min => the less, the better
    *   - None => We do not care
    *
    * Useful for graphical display
    */
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

  /** Merges this provider CommonProfilingData with another one.
    *
    * Useful when two [[Neighborhood]] have to be considered as one. For instance the DynAndThen
    * combinator needs to merge the dynamically generated Neighborhood to keep their profiling data.
    * NOTE : For composition combinator there is a [[CompositionProfiler]] doing all that.
    */
  private[profiling] def merge(profiler: SearchProfiler): Unit = {
    commonProfilingData.merge(profiler.commonProfilingData)
  }

  override def toString: String = s"Profile(${neighborhood.toString})\n$commonProfilingData"

  /** Get the detailedRecursiveName, the idea is to be able to distinguish two generated
    * neighborhood no matter the depth
    */
  private[profiling] def detailedRecursiveName: String = s"${neighborhood.toString}"
}
