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

package oscar.cbls.lib.invariant.routing.timeWindows

/** Class to model a time window
  *
  * @param earliestArrival
  *   The earliest time the vehicle can access to the associated node
  * @param latestArrival
  *   The latest time the vehicle can access to the associated node
  * @param taskDuration
  *   The time needed to perform the task on the associated node
  */
case class TimeWindow(earliestArrival: Long, latestArrival: Long, taskDuration: Long) {

  /** The earliest time the vehicle can leave the associated node */
  val earliestLeaving: Long = earliestArrival + taskDuration

  /** The latest time the vehicle can leave the associated node */
  val latestLeaving: Long = latestArrival + taskDuration

  require(
    earliestArrival >= 0 && latestArrival >= 0 && taskDuration >= 0,
    s"Value defining a time windows must be >=0. Got: $this"
  )

  require(
    latestArrival >= earliestArrival,
    s"Latest arrival ($latestArrival) must be bigger than earliest arrival ($earliestArrival)"
  )
}
