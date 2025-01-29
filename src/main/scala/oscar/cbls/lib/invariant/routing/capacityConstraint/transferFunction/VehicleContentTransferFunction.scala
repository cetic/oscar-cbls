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

/** Defines a transfer function for capacity constraints.
  *
  * It's meant to provide information about vehicle content evolution over a
  * [[oscar.cbls.core.computation.genericConstraint.segment.Segment]].
  *
  * It is based on three concepts :
  *   - Max content : The maximum content of the vehicle within the segment (starting at zero or
  *     more).
  *   - Min content : The minimum content of the vehicle within the segment (starting at zero or
  *     more).
  *   - Content at end : The content at the end of the segment.
  *
  * Based on those value we are able to define :
  *   - If there are some forbidden situation within a segment : capacity exceeded (max > max
  *     capacity) or negative content (min < 0).
  *   - The output of the segment, being also the input of the next segment (content at end).
  */
abstract class VehicleContentTransferFunction {

  /** Returns the start node of this transfer function's segment. */
  def from: Int

  /** Returns the end node of this transfer function's segment. */
  def to: Int

  /** If true, it means that the related segment leads to a forbidden situation. */
  def isEmpty: Boolean

  /** Returns the maximum content value of the segment considering a defined input value.
    *
    * @param startContent
    *   The content of the vehicle when starting this segment.
    * @return
    *   The maximum content encountered during this segment considering the input value.
    */
  def max(startContent: Long): Long

  /** Returns the minimum content value of the segment considering a defined input value.
    *
    * @param startContent
    *   The content of the vehicle when starting this segment.
    * @return
    *   The minimum content encountered during this segment considering the input value.
    */
  def min(startContent: Long): Long

  /** Returns the maximum content encountered during this segment, starting with an empty vehicle.
    */
  def maxContentIfStartAt0: Long

  /** Returns the minimum content encountered during this segment, starting with an empty vehicle.
    */
  def minContentIfStartAt0: Long

  /** Returns the content at the end of this segment, starting with an empty vehicle. */
  def contentAtEndIfStartAt0: Long
}

/** Defines the transfer function over a segment containing forbidden situation.
  */
case object EmptyContentTF extends VehicleContentTransferFunction {

  override def from: Int = -1

  override def to: Int = -1

  override def isEmpty: Boolean = true

  override def max(startContent: Long): Long = Long.MaxValue

  override def min(startContent: Long): Long = Long.MinValue

  override def maxContentIfStartAt0: Long = -1

  override def minContentIfStartAt0: Long = -1

  override def contentAtEndIfStartAt0: Long = -1

  override def toString: String = "Empty vehicle content"
}
