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

package oscar.cbls.core.search.profiling.profilingData

/** Extends CommonProfilingData by adding some specifics profiling data related to
  * [[oscar.cbls.core.search.SimpleNeighborhood]].
  *
  * Added data :
  *   - nbExplored ==> The number of explored neighbor
  *   - first and not first movement selection duration ==> To spot wasted time during
  *     initialisation of SimpleNeighborhood's exploration.
  */
private[profiling] case class NeighborhoodProfilingData() extends CommonProfilingData {

  private var _nbExplored: Long                        = 0L
  private var _firstNeighborSelectionCounter: Long     = 0L
  private var _firstNeighborSelectionDuration: Long    = 0L
  private var _notFirstNeighborSelectionCounter: Long  = 0L
  private var _notFirstNeighborSelectionDuration: Long = 0L

  private var _lastCallExploration: Long = 0L

  override def merge(other: ProfilingData): Unit = {
    super.merge(other)
    other match {
      case npd: NeighborhoodProfilingData =>
        _nbExplored += npd._nbExplored
      case _ =>
        require(
          requirement = false,
          s"Unable to merge different types of profiling data (${this.getClass} vs ${other.getClass}"
        )
    }
  }

  override def callInc(): Unit = {
    _lastCallExploration = 0
    super.callInc()
  }

  // Increments the explored neighbor counter.
  def exploreInc(): Unit = {
    _lastCallExploration += 1
    _nbExplored += 1
  }
  // Returns the number of explored neighbor.
  def nbExplored: Long = _nbExplored

  // Increments the counter of first neighbor selection.
  def firstNeighborSelectionCounterInc(): Unit = _firstNeighborSelectionCounter += 1
  // Adds the duration to the first neighbor selection time tracker.
  def firstNeighborSelectionDurationPlus(duration: Long): Unit =
    _firstNeighborSelectionDuration += duration
  // Returns average first neighbor selection duration of the profiled SimpleNeighborhood.
  def avgTimeFirstNeighborSelectionMillis(): String =
    if (_firstNeighborSelectionCounter == 0) "NA"
    else s"${(_firstNeighborSelectionDuration.toDouble / _firstNeighborSelectionCounter) / 1000000}"
  // Increments the counter of NOT first neighbor selection.
  def notFirstNeighborSelectionCounterInc(): Unit = _notFirstNeighborSelectionCounter += 1
  // Adds the duration to the NOT first neighbor selection time tracker.
  def notFirstNeighborSelectionDurationPlus(duration: Long): Unit =
    _notFirstNeighborSelectionDuration += duration
  // Returns average NOT first neighbor selection duration of the profiled SimpleNeighborhood.
  def avgTimeNotFirstNeighborSelectionMillis(): String =
    if (_notFirstNeighborSelectionCounter == 0) "NA"
    else
      s"${(_notFirstNeighborSelectionDuration.toDouble / _notFirstNeighborSelectionCounter) / 1000000}"

  override def toString: String = super.toString + s" | Explored :${_nbExplored}"
}
