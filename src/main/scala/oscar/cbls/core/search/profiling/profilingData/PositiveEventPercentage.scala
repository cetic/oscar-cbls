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

/** Profiles and computes the percentage of occurrence of a specific event.
  *
  * It's a yes or no kind of event. For instance for the OrElse combinator, the event is positive
  * when the first Neighborhood didn't find a solution, and we need to explore the second
  * Neighborhood.
  */
private[profiling] case class PositiveEventPercentage(firstOneIsPositive: Boolean = false)
    extends CombinatorProfilingData {
  private var _positiveEvents: Int = if (firstOneIsPositive) 1 else 0
  private var _events: Int         = 1

  private def positiveEvents: Int = _positiveEvents
  private def events: Int         = _events

  /** Informs the profiler the occurrence of an event.
    *
    * @param isPositive
    *   Whether the particular event is positive.
    */
  def pushEvent(isPositive: Boolean): Unit = {
    if (isPositive) _positiveEvents += 1
    _events += 1
  }

  override def merge(other: CombinatorProfilingData): Unit = {
    other match {
      case o: PositiveEventPercentage =>
        _positiveEvents += o.positiveEvents
        _events += o.events
      case _ =>
    }
  }

  override def collectStatisticsHeaders(): List[String] = List("Positive events", "Percentage")
  override def collectStatisticsData(): List[String] =
    List(_positiveEvents.toString, (_positiveEvents / _events).toString)
}
