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

package oscar.cbls.lib.invariant.routing.abstractGenericConstraint.transferFunction

/** A TransferFunction is a function that'll be attached to each precomputed sub-segment or
  * [[oscar.cbls.lib.invariant.routing.abstractGenericConstraint.logReducedSegment.LogReducedSegment]]
  * of the route. Its purpose is to give information such that we are able to know if this segment
  * respects the constraint or just having the output value of this segment.
  *
  * It's composed of two UnidirectionalTransferFunction. The forward is used for the non-flipped
  * segment and the backward for the flipped segment.
  *
  * For instance :
  *   - For a TimeWindow constraint, the function's apply() method would return the leaving time
  *     from the last node of the segment. If the result is None it would mean that the segment does
  *     not respect the constraint (typically, exceeding deadline at some node).
  *   - For a Capacity constraint, the function's apply() method would return the capacity left at
  *     the end of the segment. If the result is None it would mean that the segment does not
  *     respect the constraint (typically having negative content for some node or exceeding the
  *     capacity).
  *
  * @param from
  *   The node from which this transfer function is valid.
  * @param to
  *   The node to which this transfer function is valid.
  * @param forward
  *   The forward transfer function gives the value considering that the segment `from` -> `to` is
  *   non-flipped.
  * @param backward
  *   The forward transfer function gives the value considering that the segment `from` -> `to` is
  *   flipped.
  */
abstract class TransferFunction[U](
  val from: Int,
  val to: Int,
  val forward: UnidirectionalTransferFunction,
  val backward: UnidirectionalTransferFunction
) {

  /** Returns the value of this transfer function. It's supposedly used on segment starting at
    * depot.
    */
  def apply(): U

  /** Returns the start node of the segment considering if the segment is flipped or not. */
  def from(forward: Boolean): Int =
    if (forward) from
    else to

  /** Returns the end node of the segment considering if the segment is flipped or not. */
  def to(forward: Boolean): Int =
    if (forward) to
    else from

  /** Composes this transfer function with another into a bigger one.
    *
    * @param otherTF
    *   The other transfer function.
    * @param otherFlipped
    *   Whether the other transfer function is flipped.
    * @return
    *   The composed transfer function.
    */
  def compose(otherTF: TransferFunction[U], otherFlipped: Boolean): TransferFunction[U]
}
