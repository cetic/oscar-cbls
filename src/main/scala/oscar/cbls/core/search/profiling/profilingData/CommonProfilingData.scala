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
  * It is linked to a single instance of Neighborhood.
  */
private[profiling] class CommonProfilingData extends ProfilingData {

  private var _lastCallFound: Boolean     = false
  private var _lastCallGain: Long         = 0L
  private var _lastCallDurationNano: Long = 0L

  private var _nbCalls: Long                  = 0L
  private var _nbFound: Long                  = 0L
  private var _gain: Long                     = 0L
  private var _timeSpentMoveFoundNano: Long   = 0L
  private var _timeSpentNoMoveFoundNano: Long = 0L

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
          s"Unable to merge different types of profiling data (${this.getClass} vs ${other.getClass})"
        )
    }

  }

  // Adds the gain of the last call.
  def gainPlus(gain: Long): Unit = {
    this._lastCallGain = gain
    this._gain += gain
  }

  // Increments call counter.
  def callInc(): Unit = {
    _nbCalls += 1
  }

  // Increments found counter.
  def foundInc(): Unit = {
    _nbFound += 1
  }

  // Adds last call duration to the time tracker for successful exploration.
  def timeSpentMoveFoundPlus(timeNano: Long): Unit = {
    this._lastCallFound = true
    this._lastCallDurationNano = timeNano
    this._timeSpentMoveFoundNano += timeNano
  }

  // Adds last call duration to the time tracker for unsuccessful exploration.
  def timeSpentNoMoveFoundPlus(timeNano: Long): Unit = {
    this._lastCallFound = false
    this._lastCallDurationNano = timeNano
    this._timeSpentNoMoveFoundNano += timeNano
  }

  // Returns the number of explorations made by the profiled Neighborhood.
  def nbCalls: Long = _nbCalls
  // Returns the number of successful explorations made by the profiled Neighborhood.
  def nbFound: Long = _nbFound
  // Returns the total gain acquired using the profiled Neighborhood.
  def gain: Long = _gain
  // Returns the total duration of successful explorations made by the profiled Neighborhood.
  def timeSpentMoveFoundMillis: Long = _timeSpentMoveFoundNano / 1000000
  // Returns the total duration of unsuccessful explorations made by the profiled Neighborhood.
  def timeSpentNoMoveFoundMillis: Long = _timeSpentNoMoveFoundNano / 1000000
  // Returns the total duration of explorations made by the profiled Neighborhood.
  def timeSpentMillis: Long = (_timeSpentMoveFoundNano + _timeSpentNoMoveFoundNano) / 1000000

  override def toString: String = {
    s"Calls :${_nbCalls} | Founds :${_nbFound} | gain :${_gain} | " +
      s"total time move (nano):${_timeSpentMoveFoundNano} | total time no move (nano):${_timeSpentNoMoveFoundNano}" +
      s"Last call: found ${_lastCallFound} | gain ${_lastCallGain} | duration ${_lastCallDurationNano} nano"
  }
}
