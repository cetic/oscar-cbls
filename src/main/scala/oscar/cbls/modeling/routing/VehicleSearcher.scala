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

package oscar.cbls.modeling.routing

import oscar.cbls.algo.sequence.IntSequence

/** Abstract class to define vehicle searchers.
  *
  * @param v
  *   The number of vehicle in the routes.
  */
abstract class VehicleSearcher(v: Int) {

  /** Returns a new vehicle searcher based on the given checkpoint. */
  def defineCheckpoint(newCheckpoint: Option[IntSequence]): VehicleSearcher

  /** Returns the searcher at the top checkpoint. */
  def rollbackToTopCheckpoint(): VehicleSearcher

  /** Returns the searcher at the previous checkpoint. The top one is deleted */
  def releaseTopCheckpoint(): VehicleSearcher

  /** Helper function that finds which vehicle in a route reaches a given position.
    */
  protected def vehicleBinarySearch(position: Int, startPosOfVehicle: Int => Int): Int = {
    var upperVehicle: Int  = v - 1
    var upperPosition: Int = startPosOfVehicle(upperVehicle)
    var lowerVehicle: Int  = 0
    var lowerPosition: Int = 0

    if (position >= upperPosition) return upperVehicle

    while (lowerVehicle + 1 < upperVehicle) {

      val midVehicle: Int  = (lowerVehicle + upperVehicle) / 2
      val midPosition: Int = startPosOfVehicle(midVehicle)
      if (midPosition == position) {
        return midVehicle
      } else if (midPosition < position) {
        lowerVehicle = midVehicle
        lowerPosition = midPosition
      } else {
        upperVehicle = midVehicle
        upperPosition = midPosition
      }
    }
    lowerVehicle
  }

}
