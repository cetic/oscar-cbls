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

/** Adds some final Neighborhood specifics profiling data to CommonProfilingData.
  *
  * Added data :
  *   - nbExplored
  *   - first and not first movement selection duration
  */
case class NeighborhoodProfilingData() extends CommonProfilingData {

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

  def exploreInc(): Unit = {
    _lastCallExploration += 1
    _nbExplored += 1
  }
  def nbExplored: Long = _nbExplored

  def firstNeighborSelectionCounterInc(): Unit = _firstNeighborSelectionCounter += 1
  def firstNeighborSelectionDurationPlus(duration: Long): Unit =
    _firstNeighborSelectionDuration += duration
  def avgTimeFirstNeighborSelectionMillis(): String =
    if (_firstNeighborSelectionCounter == 0) "NA"
    else s"${(_firstNeighborSelectionDuration.toDouble / _firstNeighborSelectionCounter) / 1000000}"
  def notFirstNeighborSelectionCounterInc(): Unit = _notFirstNeighborSelectionCounter += 1
  def notFirstNeighborSelectionDurationPlus(duration: Long): Unit =
    _notFirstNeighborSelectionDuration += duration
  def avgTimeNotFirstNeighborSelectionMillis(): String =
    if (_notFirstNeighborSelectionCounter == 0) "NA"
    else
      s"${(_notFirstNeighborSelectionDuration.toDouble / _notFirstNeighborSelectionCounter) / 1000000}"

  override def toString: String = super.toString + s" | Explored :${_nbExplored}"
}
