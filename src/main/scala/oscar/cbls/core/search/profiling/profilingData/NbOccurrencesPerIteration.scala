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
  * (min/mean/max occurrences of an event during an iteration) ex: Max movement reached before reset
  *
  * @param startIncIteration
  *   Whether or not the instance must start with an occurrence
  * @param startIncOccurrence
  *   Whether or not the instance must start at the second iteration
  */
case class NbOccurrencesPerIteration(
  startIncOccurrence: Boolean = false,
  startIncIteration: Boolean = false
) extends CombinatorProfilingData {
  private var _occurrences: Int       = 0
  private var _minOccurrences: Int    = Int.MaxValue
  private var _maxOccurrences: Int    = Int.MinValue
  private var _summedOccurrences: Int = 0
  private var _iterations: Int        = 1

  private def minOccurrences: Int    = _minOccurrences
  private def maxOccurrences: Int    = _maxOccurrences
  private def summedOccurrences: Int = _summedOccurrences
  private def iterations: Int        = _iterations

  /** Starts a new iteration.
    *
    * Updates min/max/summed occurrence of the event considering all iterations. Increments
    * iterations number and resets occurrences (for the new iteration)
    */
  def nextIteration(): Unit = {
    _minOccurrences = Math.min(_minOccurrences, _occurrences)
    _maxOccurrences = Math.max(_maxOccurrences, _occurrences)
    _summedOccurrences = _summedOccurrences + _occurrences
    _iterations = _iterations + 1
    _occurrences = 0
  }

  /** Increments the number of occurrences of the event */
  def eventOccurred(): Unit = _occurrences += 1

  override def merge(other: CombinatorProfilingData): Unit = {
    other match {
      case o: NbOccurrencesPerIteration =>
        o.nextIteration() // We have to "commit" the last iteration
        _minOccurrences = Math.min(_minOccurrences, o.minOccurrences)
        _maxOccurrences = Math.max(_maxOccurrences, o.maxOccurrences)
        _summedOccurrences += o.summedOccurrences
        _iterations += o.iterations
    }
  }

  override def collectStatisticsHeaders(): List[String] = {
    nextIteration()
    List(
      "MinOccurrencePerIteration",
      "MeanOccurrencePerIteration",
      "MaxOccurrencePerIteration",
      "NbIterations",
      "TotalOccurrences"
    )
  }

  override def collectStatisticsData(): List[String] = {
    List(
      _minOccurrences.toString,
      ((_summedOccurrences * 1000 / _iterations) / 1000.0).toString,
      _maxOccurrences.toString,
      _iterations.toString,
      _summedOccurrences.toString
    )
  }
}
