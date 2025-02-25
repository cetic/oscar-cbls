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

package oscar.cbls.lib.invariant.routing

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.genericConstraint.logReducedSegment._
import oscar.cbls.core.computation.genericConstraint.LogReducedGlobalConstraint

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.modeling.routing.VRP

import scala.annotation.tailrec

/** Companion object of the [[NbNodesLogReduced]] class. */
object NbNodesLogReduced {

  /** Creates a NbNodes invariant, which maintains the number of nodes visited by each vehicle of
    * the routes.
    *
    * @param vrp
    *   The object that represents the Vehicle Routing Problem.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(vrp: VRP, name: String = "Nb Nodes"): NbNodesLogReduced = {
    new NbNodesLogReduced(vrp, Array.fill(vrp.v)(IntVariable(vrp.model, 0L)), Some(name))
  }
}

/** Invariant which maintains the number of nodes visited by each vehicle of the routes.<br>
  *
  * When precomputations are performed, we compute for each node the number of node reached from the
  * current vehicle start position.
  *
  * @param vrp
  *   The object that represents the Vehicle Routing Problem.
  * @param output
  *   Array telling how many nodes are reached by each vehicle.
  * @param name
  *   The (optional) name of the Invariant.
  */
class NbNodesLogReduced(vrp: VRP, output: Array[IntVariable], name: Option[String])
    extends LogReducedGlobalConstraint[Long, Long](vrp, name) {

  output.foreach(_.setDefiningInvariant(this))

  /** Returns the output variables of the invariant. */
  def apply(): Array[IntVariable] = output

  /** Returns the output variable associated to the given vehicle. */
  def apply(vehicle: Int): IntVariable = output(vehicle)

  override def nodeValue(node: Int): Long = 1

  override def endNodeValue(vehicle: Int): Long = 0

  override def composeSteps(firstStep: Long, secondStep: Long): Long = firstStep + secondStep

  override def computeVehicleValueComposed(
    vehicle: Int,
    segments: List[LogReducedSegment[Long]]
  ): Long = {

    @tailrec
    def composeLogReduceSegments(
      logReducedSegments: List[LogReducedSegment[Long]],
      currentNb: Long = 0L
    ): Long = {
      logReducedSegments match {
        case Nil => currentNb
        case head :: tail =>
          val newNb: Long = head match {
            case s: LogReducedPreComputedSegment[Long] =>
              s.steps.sum[Long];

            case s: LogReducedFlippedPreComputedSegment[Long] => s.steps.reverse.sum[Long]

            case _: LogReducedNewNode[Long] => 1L
          }
          composeLogReduceSegments(tail, currentNb + newNb)

      }
    }
    composeLogReduceSegments(segments)
  }

  override protected def assignVehicleValue(vehicle: Int, value: Long): Unit =
    output(vehicle) := value

  override protected def computeVehicleValueFromScratch(vehicle: Int, routes: IntSequence): Long = {
    require(
      vehicle < vrp.v,
      s"The value $vehicle is not a vehicle in the given sequence (must be < ${vrp.v})"
    )

    var nbNodes: Long            = 1L
    var exp: IntSequenceExplorer = routes.explorerAtAnyOccurrence(vehicle).get.next
    while (exp.position < routes.size && exp.value >= vrp.v) {
      nbNodes += 1L
      exp = exp.next
    }
    nbNodes
  }

}
