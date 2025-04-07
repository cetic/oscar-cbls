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

import oscar.cbls.lib.invariant.routing.abstractGenericConstraint.transferFunction.TransferFunction

object VehicleContentTF {
  def apply(
    from: Int,
    to: Int,
    forwardTF: UnidirectionalContentTF,
    backwardTF: UnidirectionalContentTF,
    vehicleCapacity: Option[Long] = None
  ): VehicleContentTF = {
    new VehicleContentTF(from, to, forwardTF, backwardTF, vehicleCapacity)
  }
}

/** A vehicle content transfer function.
  *
  * @param from
  *   The node from which this transfer function is valid.
  * @param to
  *   The node to which this transfer function is valid.
  * @param forwardTF
  *   The unidirectional TF starting at from and ending at to.
  * @param backwardTF
  *   The unidirectional TF starting at to and ending at from.
  * @param vehicleCapacity
  *   The capacity of the vehicle used for this TF. None if this TF does not contain a vehicle.
  */
class VehicleContentTF(
  from: Int,
  to: Int,
  forwardTF: UnidirectionalContentTF,
  backwardTF: UnidirectionalContentTF,
  vehicleCapacity: Option[Long]
) extends TransferFunction[Boolean](from, to, forwardTF, backwardTF) {

  override def apply(): Boolean = {
    require(
      vehicleCapacity.nonEmpty,
      "This TF does not start from a vehicle's depot. Cannot compute value."
    )
    forwardTF.max(0) > vehicleCapacity.get || forwardTF.min(0) < 0
  }

  override def compose(
    otherTF: TransferFunction[Boolean],
    otherFlipped: Boolean
  ): TransferFunction[Boolean] = {
    val (newForward, newBackward) =
      if (otherFlipped)
        (
          forwardTF.compose(otherTF.backward).asInstanceOf[UnidirectionalContentTF],
          otherTF.forward.compose(backwardTF).asInstanceOf[UnidirectionalContentTF]
        )
      else
        (
          forwardTF.compose(otherTF.forward).asInstanceOf[UnidirectionalContentTF],
          otherTF.backward.compose(backwardTF).asInstanceOf[UnidirectionalContentTF]
        )
    new VehicleContentTF(from, otherTF.to, newForward, newBackward, vehicleCapacity)
  }

  /** Returns the content at the end of the segment considering if the segment is flipped or not. */
  def contentAtEndIfStartAt0(forward: Boolean): Long =
    if (forward) forwardTF.contentAtEndIfStartAt0
    else backwardTF.contentAtEndIfStartAt0

  override def toString: String = {
    s"""Vehicle content function :
       |From : $from - To : $to
       |Forward : $forwardTF
       |Backward : $backwardTF
       |""".stripMargin
  }
}
