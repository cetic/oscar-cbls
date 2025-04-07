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

import oscar.cbls.lib.invariant.routing.abstractGenericConstraint.transferFunction.UnidirectionalTransferFunction

/** Companion object of [[UnidirectionalContentTF]]. */
object UnidirectionalContentTF {

  /** Defines a unidirectional content transfer function over a segment.
    *
    * It's either non-flipped (forward == true) or flipped (forward == false).
    *
    * @param forward
    *   Whether this is the non-flipped UnidirectionalTF of the TransferFunction.
    * @param maxContentIfStartAt0
    *   The maximum content encountered during this segment, starting with an empty vehicle.
    * @param minContentIfStartAt0
    *   The minimum content encountered during this segment, starting with an empty vehicle.
    * @param contentAtEndIfStartAt0
    *   The output content of this segment, starting with an empty vehicle.
    */
  def apply(
    forward: Boolean,
    maxContentIfStartAt0: Long,
    minContentIfStartAt0: Long,
    contentAtEndIfStartAt0: Long
  ): UnidirectionalContentTF = {
    new UnidirectionalContentTF(
      forward,
      maxContentIfStartAt0: Long,
      minContentIfStartAt0: Long,
      contentAtEndIfStartAt0: Long
    )
  }
}

/** Defines a unidirectional content transfer function over a segment.
  *
  * It's either non-flipped (forward == true) or flipped (forward == false).
  *
  * @param forward
  *   Whether this is the non-flipped UnidirectionalTF of the TransferFunction.
  * @param _maxContentIfStartAt0
  *   The maximum content encountered during this segment, starting with an empty vehicle.
  * @param _minContentIfStartAt0
  *   The minimum content encountered during this segment, starting with an empty vehicle.
  * @param _contentAtEndIfStartAt0
  *   The output content of this segment, starting with an empty vehicle.
  */
class UnidirectionalContentTF(
  forward: Boolean,
  _maxContentIfStartAt0: Long,
  _minContentIfStartAt0: Long,
  _contentAtEndIfStartAt0: Long
) extends UnidirectionalTransferFunction {

  def max(startContent: Long): Long = _maxContentIfStartAt0 + startContent

  def min(startContent: Long): Long = _minContentIfStartAt0 + startContent

  def maxContentIfStartAt0: Long = _maxContentIfStartAt0

  def minContentIfStartAt0: Long = _minContentIfStartAt0

  def contentAtEndIfStartAt0: Long = _contentAtEndIfStartAt0

  override def toString: String =
    s"""Vehicle content at end : $contentAtEndIfStartAt0
       |Max if start content is zero : $maxContentIfStartAt0
       |Min if start content is zero : $minContentIfStartAt0""".stripMargin

  override def compose(otherUTF: UnidirectionalTransferFunction): UnidirectionalTransferFunction = {
    otherUTF match {
      case other: UnidirectionalContentTF =>
        val max =
          Math.max(maxContentIfStartAt0, contentAtEndIfStartAt0 + other.maxContentIfStartAt0)
        val min =
          Math.min(minContentIfStartAt0, contentAtEndIfStartAt0 + other.minContentIfStartAt0)
        val end = contentAtEndIfStartAt0 + other.contentAtEndIfStartAt0
        UnidirectionalContentTF(forward, max, min, end)
      case _ =>
        throwComposeError(this, otherUTF)
        null
    }
  }

}
