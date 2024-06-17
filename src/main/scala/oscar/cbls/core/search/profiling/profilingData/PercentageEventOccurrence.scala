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

/** Profiles and computes the percentage of occurrence of a specific event over iteration.
  *
  * Ex: The percentage of else choice with OrElse
  */
case class PercentageEventOccurrence(firstOneOccurred: Boolean = false) extends CombinatorProfilingData {
  private var _occurrences: Int = if(firstOneOccurred) 1 else 0
  private var _iterations: Int  = 0

  private def occurrences: Int = _occurrences
  private def iterations: Int  = _iterations

  /** Informs the profiler the occurrence of an event in this iteration.
    *
    * @param occurred
    *   Whether or not the particular event occurred
    */
  def pushEvent(occurred: Boolean): Unit = {
    if (occurred) _occurrences += 1
    _iterations += 1
  }

  override def merge(other: CombinatorProfilingData): Unit = {
    other match {
      case o: PercentageEventOccurrence =>
        _occurrences += o.occurrences
        _iterations += o.iterations
      case _ =>
    }
  }

  override def collectStatisticsHeaders(): List[String] = List("Occurrences", "percentage")
  override def collectStatisticsData(): List[String] =
    List(_occurrences.toString, (_occurrences / _iterations).toString)
}
