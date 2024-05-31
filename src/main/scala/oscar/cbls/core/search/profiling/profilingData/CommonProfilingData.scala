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

/** This class contains all profiling data common to Neighborhood and Combinator.
  *
  * You can see duplicated data like _nbCalls and _nbCallsForSelection. The second one is meant to
  * be reset upon SelectionCombinator reset action. The first one is never reset and contains the
  * data of the whole search.
  */
class CommonProfilingData() extends ProfilingData() {

  var _lastCallFound: Boolean     = false
  var _lastCallGain: Long         = 0L
  var _lastCallDurationNano: Long = 0L

  var _nbCalls: Long                  = 0L
  var _nbFound: Long                  = 0L
  var _gain: Long                     = 0L
  var _timeSpentMoveFoundNano: Long   = 0L
  var _timeSpentNoMoveFoundNano: Long = 0L

  var _nbCallsForSelection: Long                  = 0L
  var _nbFoundsForSelection: Long                 = 0L
  var _gainForSelection: Long                     = 0L
  var _timeSpentMoveFoundNanoForSelection: Long   = 0L
  var _timeSpentNoMoveFoundNanoForSelection: Long = 0L

  override def merge(other: ProfilingData): Unit = {
    other match {
      case bpd: CommonProfilingData =>
        _lastCallFound = bpd._lastCallFound
        _lastCallGain = bpd._lastCallGain
        _lastCallDurationNano = bpd._lastCallDurationNano
        _nbCalls += bpd._nbCalls
        _nbFound += bpd._nbFound
        _gain += bpd._gain
        _timeSpentMoveFoundNano += bpd._timeSpentMoveFoundNano
        _timeSpentNoMoveFoundNano += bpd._timeSpentNoMoveFoundNano
      case _ =>
        require(
          requirement = false,
          s"Unable to merge different types of profiling data (${this.getClass} vs ${other.getClass}"
        )
    }

  }

  def gainPlus(gain: Long): Unit = {
    this._lastCallGain = gain
    this._gain += gain
    this._gainForSelection += gain
  }
  def callInc(): Unit = {
    _nbCalls += 1
    _nbCallsForSelection += 1
  }
  def foundInc(): Unit = {
    _nbFound += 1
    _nbFoundsForSelection += 1
  }
  def timeSpentMoveFoundPlus(timeNano: Long): Unit = {
    this._lastCallFound = true
    this._lastCallDurationNano = timeNano
    this._timeSpentMoveFoundNano += timeNano
    this._timeSpentMoveFoundNanoForSelection += timeNano
  }
  def timeSpentNoMoveFoundPlus(timeNano: Long): Unit = {
    this._lastCallFound = false
    this._lastCallDurationNano = timeNano
    this._timeSpentNoMoveFoundNano += timeNano
    this._timeSpentNoMoveFoundNanoForSelection += timeNano
  }

  def nbCalls: Long                    = _nbCalls
  def nbFound: Long                    = _nbFound
  def gain: Long                       = _gain
  def timeSpentMoveFoundMillis: Long   = _timeSpentMoveFoundNano / 1000000
  def timeSpentNoMoveFoundMillis: Long = _timeSpentNoMoveFoundNano / 1000000
  def timeSpentMillis: Long = (_timeSpentMoveFoundNano + _timeSpentNoMoveFoundNano) / 1000000

  def nbCallsForSelection: Long                    = _nbCallsForSelection
  def nbFoundForSelection: Long                    = _nbFoundsForSelection
  def gainForSelection: Long                       = _gainForSelection
  def timeSpentMoveFoundMillisForSelection: Long   = _timeSpentMoveFoundNanoForSelection / 1000000
  def timeSpentNoMoveFoundMillisForSelection: Long = _timeSpentNoMoveFoundNanoForSelection / 1000000
  def timeSpentMillisForSelection: Long =
    (_timeSpentMoveFoundNanoForSelection + _timeSpentNoMoveFoundNanoForSelection) / 1000000

  def resetStatisticsForSelection(): Unit = {
    _nbCallsForSelection = 0L; _nbFoundsForSelection = 0L; _gainForSelection = 0L
    _timeSpentMoveFoundNanoForSelection = 0L; _timeSpentNoMoveFoundNanoForSelection = 0L
  }

  override def toString: String = {
    s"Calls :${_nbCalls} | Founds :${_nbFound} | gain :${_gain} | " +
      s"total time move (nano):${_timeSpentMoveFoundNano} | total time no move (nano):${_timeSpentNoMoveFoundNano}" +
      s"Last call: found ${_lastCallFound} | gain ${_lastCallGain} | duration ${_lastCallDurationNano} nano"
  }
}
