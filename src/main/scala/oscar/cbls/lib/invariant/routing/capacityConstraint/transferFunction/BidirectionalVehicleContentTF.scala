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

package oscar.cbls.lib.invariant.routing.capacityConstraint.transferFunction

object BidirectionalVehicleContentTF {

  /** Creates the bidirectional content transfer function over a segment. Handling two
    * UnidirectionalContentTF one for non-flipped segment and another one for flipped segment.
    *
    * @param nonFlippedFunction
    *   The transfer function if the segment is NOT flipped.
    * @param flippedFunction
    *   The transfer function if the segment is flipped.
    */
  def apply(
    nonFlippedFunction: VehicleContentTransferFunction,
    flippedFunction: VehicleContentTransferFunction
  ): BidirectionalVehicleContentTF = {
    new BidirectionalVehicleContentTF(nonFlippedFunction, flippedFunction)
  }
}

/** Defines the bidirectional transfer function over a segment. Handling two UnidirectionalContentTF
  * one for non-flipped segment and another one for flipped segment.
  *
  * @param nonFlippedFunction
  *   The transfer function if the segment is NOT flipped.
  * @param flippedFunction
  *   The transfer function if the segment is flipped.
  */
class BidirectionalVehicleContentTF(
  val nonFlippedFunction: VehicleContentTransferFunction,
  val flippedFunction: VehicleContentTransferFunction
) {

  /** Returns the start node of the segment considering if the segment is flipped or not. */
  def from(flipped: Boolean): Int =
    if (flipped) flippedFunction.from
    else nonFlippedFunction.from

  /** Returns the end node of the segment considering if the segment is flipped or not. */
  def to(flipped: Boolean): Int =
    if (flipped) flippedFunction.to
    else nonFlippedFunction.to

  /** Returns the content at the end of the segment considering if the segment is flipped or not. */
  def contentAtEndIfStartAt0(flipped: Boolean): Long =
    if (flipped) flippedFunction.contentAtEndIfStartAt0
    else nonFlippedFunction.contentAtEndIfStartAt0

  /** Returns the minimum encountered content value considering if the segment is flipped or not. */
  def minIfStartAtZero(flipped: Boolean): Long =
    if (flipped) flippedFunction.minContentIfStartAt0
    else nonFlippedFunction.minContentIfStartAt0

  /** Returns the maximum encountered content value considering if the segment is flipped or not. */
  def maxIfStartAtZero(flipped: Boolean): Long =
    if (flipped) flippedFunction.maxContentIfStartAt0
    else nonFlippedFunction.maxContentIfStartAt0

  /** Returns true if there is a capacity constraint violation.
    *
    * Either the max capacity of the vehicle is exceeded or we have a negative content at some
    * point.
    *
    * @param startContent
    *   The input content of this segment.
    * @param maxVehicleContent
    *   The capacity of the vehicle.
    * @param flipped
    *   Whether this segment is flipped or not.
    * @return
    *   True if there is a capacity constraint violation, false otherwise.
    */
  def apply(startContent: Long, maxVehicleContent: Long, flipped: Boolean): Boolean = {
    val vehicleContentFunction = if (flipped) flippedFunction else nonFlippedFunction
    vehicleContentFunction.max(startContent) > maxVehicleContent || vehicleContentFunction.min(
      startContent
    ) < 0
  }

  /** Returns true if the transfer function is empty, considering if the segment is flipped or not.
    */
  def isEmpty(flipped: Boolean): Boolean =
    if (flipped) flippedFunction.isEmpty
    else nonFlippedFunction.isEmpty

  override def toString: String = {
    s"""Two ways vehicle content function :
       |Non-flipped : $nonFlippedFunction
       |Flipped : $flippedFunction
       |""".stripMargin
  }
}
