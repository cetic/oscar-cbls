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

/** Profiles and computes the number of occurrences of an event over iterations.
  *
  * (min/mean/max occurrences of an event during an iteration) ex: Max movement reach before reset
  */
case class NbOccurrencesPerIteration(initFirstIteration: Boolean) extends CombinatorProfilingData {
  var _occurrences: Int       = 0
  var _minOccurrences: Int    = Int.MaxValue
  var _maxOccurrences: Int    = Int.MinValue
  var _summedOccurrences: Int = 0
  var _iterations: Int        = if (initFirstIteration) 1 else 0

  def occurrences: Int       = _occurrences
  def minOccurrences: Int    = _minOccurrences
  def maxOccurrences: Int    = _maxOccurrences
  def summedOccurrences: Int = _summedOccurrences
  def iterations: Int        = _iterations

  def nextIteration(): Unit = {
    _minOccurrences = Math.min(_minOccurrences, _occurrences)
    _maxOccurrences = Math.max(_maxOccurrences, _occurrences)
    _summedOccurrences = _summedOccurrences + _occurrences
    _iterations = _iterations + 1
    _occurrences = 0
  }

  def eventOccurred(): Unit = _occurrences += 1

  def merge(other: CombinatorProfilingData): Unit = {
    other match {
      case o: NbOccurrencesPerIteration =>
        o.nextIteration() // We have to "commit" the last iteration
        _minOccurrences = Math.min(_minOccurrences, o.minOccurrences)
        _maxOccurrences = Math.max(_maxOccurrences, o.maxOccurrences)
        _summedOccurrences += o.summedOccurrences
        _iterations += o.iterations
    }
  }

  override def collectStatisticsHeaders(): Array[String] = {
    nextIteration()
    Array(
      "MinOccurrencePerIteration",
      "MeanOccurrencePerIteration",
      "MaxOccurrencePerIteration",
      "NbIterations",
      "TotalOccurrences"
    )
  }
  override def collectStatisticsData(): Array[String] = {
    Array(
      _minOccurrences.toString,
      ((_summedOccurrences * 1000 / _iterations) / 1000.0).toString,
      _maxOccurrences.toString,
      _iterations.toString,
      _summedOccurrences.toString
    )
  }
}
