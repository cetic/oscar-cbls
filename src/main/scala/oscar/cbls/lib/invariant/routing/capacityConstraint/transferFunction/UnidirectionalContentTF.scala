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

/** Companion object of [[UnidirectionalContentTF]]. */
object UnidirectionalContentTF {

  /** Creates the content transfer function over a segment starting at `from` and ending at `to`.
    *
    * @param from
    *   The node starting the related segment.
    * @param to
    *   The node ending the related segment.
    * @param maxContentIfStartAt0
    *   The maximum content encountered during this segment, starting with an empty vehicle.
    * @param minContentIfStartAt0
    *   The minimum content encountered during this segment, starting with an empty vehicle.
    * @param contentAtEndIfStartAt0
    *   The output content of this segment, starting with an empty vehicle.
    */
  def apply(
    from: Int,
    to: Int,
    maxContentIfStartAt0: Long,
    minContentIfStartAt0: Long,
    contentAtEndIfStartAt0: Long
  ): UnidirectionalContentTF = {
    new UnidirectionalContentTF(
      from,
      to,
      maxContentIfStartAt0,
      minContentIfStartAt0,
      contentAtEndIfStartAt0
    )
  }

  /** Creates the content transfer function over a node.
    *
    * @param node
    *   The node.
    * @param contentDelta
    *   The content variation at that node.
    */
  def apply(node: Int, contentDelta: Long): UnidirectionalContentTF = {
    new UnidirectionalContentTF(node, node, contentDelta, contentDelta, contentDelta)
  }
}

/** Defines the content transfer function over a segment starting at `_from` and ending at `_to`.
  *
  * @param _from
  *   The node starting the related segment.
  * @param _to
  *   The node ending the related segment.
  * @param _maxContentIfStartAt0
  *   The maximum content encountered during this segment, starting with an empty vehicle.
  * @param _minContentIfStartAt0
  *   The minimum content encountered during this segment, starting with an empty vehicle.
  * @param _contentAtEndIfStartAt0
  *   The output content of this segment, starting with an empty vehicle.
  */
class UnidirectionalContentTF(
  _from: Int,
  _to: Int,
  _maxContentIfStartAt0: Long,
  _minContentIfStartAt0: Long,
  _contentAtEndIfStartAt0: Long
) extends VehicleContentTransferFunction {
  override def from: Int = _from

  override def to: Int = _to

  override def isEmpty: Boolean = false

  override def max(startContent: Long): Long = _maxContentIfStartAt0 + startContent

  override def min(startContent: Long): Long = _minContentIfStartAt0 + startContent

  override def maxContentIfStartAt0: Long = _maxContentIfStartAt0

  override def minContentIfStartAt0: Long = _minContentIfStartAt0

  override def contentAtEndIfStartAt0: Long = _contentAtEndIfStartAt0

  override def toString: String =
    s"""From : $from, To = $to
       |Vehicle content at end : $contentAtEndIfStartAt0
       |Max if start content is zero : $maxContentIfStartAt0
       |Min if start content is zero : $minContentIfStartAt0""".stripMargin
}
