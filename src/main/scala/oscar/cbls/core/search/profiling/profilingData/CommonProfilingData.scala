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
  * It is linked to a single instance of Neighborhood. You can see duplicated data like _nbCalls and
  * _nbCallsIntermediary. The second one is meant to be reset when a combinator uses the
  * resetStatistic method. The first one is never reset and contains the data of the whole search.
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

  private var _nbCallsIntermediary: Long                  = 0L
  private var _nbFoundsIntermediary: Long                 = 0L
  private var _gainIntermediary: Long                     = 0L
  private var _timeSpentMoveFoundNanoIntermediary: Long   = 0L
  private var _timeSpentNoMoveFoundNanoIntermediary: Long = 0L

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

  def gainPlus(gain: Long): Unit = {
    this._lastCallGain = gain
    this._gain += gain
    this._gainIntermediary += gain
  }
  def cancelLastGain(): Unit = {
    _gain -= _lastCallGain
    _gainIntermediary -= _lastCallGain
  }
  def callInc(): Unit = {
    _nbCalls += 1
    _nbCallsIntermediary += 1
  }
  def foundInc(): Unit = {
    _nbFound += 1
    _nbFoundsIntermediary += 1
  }
  def foundDec(): Unit = {
    _nbFound -= 1
    _nbFoundsIntermediary -= 1
  }
  def timeSpentMoveFoundPlus(timeNano: Long): Unit = {
    this._lastCallFound = true
    this._lastCallDurationNano = timeNano
    this._timeSpentMoveFoundNano += timeNano
    this._timeSpentMoveFoundNanoIntermediary += timeNano
  }
  def timeSpentNoMoveFoundPlus(timeNano: Long): Unit = {
    this._lastCallFound = false
    this._lastCallDurationNano = timeNano
    this._timeSpentNoMoveFoundNano += timeNano
    this._timeSpentNoMoveFoundNanoIntermediary += timeNano
  }

  def nbCalls: Long                    = _nbCalls
  def nbFound: Long                    = _nbFound
  def gain: Long                       = _gain
  def timeSpentMoveFoundMillis: Long   = _timeSpentMoveFoundNano / 1000000
  def timeSpentNoMoveFoundMillis: Long = _timeSpentNoMoveFoundNano / 1000000
  def timeSpentMillis: Long = (_timeSpentMoveFoundNano + _timeSpentNoMoveFoundNano) / 1000000

  def nbFoundForSelection: Long                  = _nbFoundsIntermediary
  def gainForSelection: Long                     = _gainIntermediary
  def timeSpentMoveFoundMillisForSelection: Long = _timeSpentMoveFoundNanoIntermediary / 1000000
  def timeSpentMillisForSelection: Long =
    (_timeSpentMoveFoundNanoIntermediary + _timeSpentNoMoveFoundNanoIntermediary) / 1000000

  def resetIntermediaryStatistics(): Unit = {
    _nbCallsIntermediary = 0L; _nbFoundsIntermediary = 0L; _gainIntermediary = 0L
    _timeSpentMoveFoundNanoIntermediary = 0L; _timeSpentNoMoveFoundNanoIntermediary = 0L
  }

  override def toString: String = {
    s"Calls :${_nbCalls} | Founds :${_nbFound} | gain :${_gain} | " +
      s"total time move (nano):${_timeSpentMoveFoundNano} | total time no move (nano):${_timeSpentNoMoveFoundNano}" +
      s"Last call: found ${_lastCallFound} | gain ${_lastCallGain} | duration ${_lastCallDurationNano} nano"
  }
}
