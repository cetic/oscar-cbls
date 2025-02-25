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

package oscar.cbls.core.computation.genericConstraint

import oscar.cbls.algo.sequence.IntSequenceExplorer
import oscar.cbls.core.computation.genericConstraint.segment.{
  FlippedPreComputedSubSequence,
  PrecomputedSubSequence,
  Segment,
  VehicleSegments
}
import oscar.cbls.modeling.routing.VRP

/** Abstract class for [[NaiveRoutingConstraint]] where the computations along the sequence are
  * performed forward.
  *
  * @param vrp
  *   The object that represents the Vehicle Routing Problem.
  * @param defaultValueForUnroutedNodes
  *   The default value associated to unrouted nodes.
  * @param initValuePerVehicle
  *   The values used to start the computation along each vehicle routes.
  * @param fun
  *   A function `(from, to, valueFrom) => valueTo` that takes the node `from`, the node `to` and
  *   the value `valueFrom` associated to the node `from` and computes the value associated to the
  *   node `to`.
  * @param name
  *   The (optional) name of the Invariant.
  * @tparam U
  *   Parametrized type that represents the output type of the constraint, for example, `Long` for
  *   `RouteLength` (the total distance).
  */
abstract class ForwardNaiveRoutingConstraint[U: Manifest](
  vrp: VRP,
  defaultValueForUnroutedNodes: U,
  initValuePerVehicle: Array[U],
  fun: (Int, Int, U) => U,
  name: Option[String]
) extends NaiveRoutingConstraint[U](
      vrp,
      defaultValueForUnroutedNodes,
      initValuePerVehicle,
      fun,
      name
    ) {

  override protected def getNextPointOfSegmentToUpdate(
    exp: IntSequenceExplorer,
    seg: Segment
  ): Option[IntSequenceExplorer] = {
    seg match {
      case PrecomputedSubSequence(_, endNode, _) if exp.value != endNode        => Some(exp.next)
      case FlippedPreComputedSubSequence(_, endNode, _) if exp.value != endNode => Some(exp.next)
      case _                                                                    => None
    }
  }

  override protected def firstPointToUpdateOnSegment(seg: Segment): Int = seg.startNode()

  override protected def lastPointToUpdateOnSegment(seg: Segment): Int = seg.endNode()

  override protected def orderSegments(vehicleSegments: VehicleSegments): List[Segment] =
    vehicleSegments.segments
}
