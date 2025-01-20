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
import oscar.cbls.core.computation.globalConstraint.GlobalConstraintCore
import oscar.cbls.core.computation.globalConstraint.segment.Segment
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.modeling.routing.VRP

/** Companion object of the [[NbNodes]] class. */
object NbNodes {

  /** Creates a NbNodes invariant, which maintains the number of nodes visited by each vehicle of
    * the routes.
    *
    * @param vrp
    *   The object that represents the Vehicle Routing Problem.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(vrp: VRP, name: String = "Nb Nodes"): NbNodes = {
    new NbNodes(vrp, Array.fill(vrp.v)(IntVariable(vrp.model, 0L)), Some(name))
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
class NbNodes(vrp: VRP, output: Array[IntVariable], name: Option[String])
    extends GlobalConstraintCore[Long](vrp, name) {

  output.foreach(_.setDefiningInvariant(this))

  /** Array that contains precomputed value for each node of the sequence. */
  val precomputedValues: Array[Long] = new Array[Long](vrp.n)

  /** Returns the output variables of the invariant. */
  def apply(): Array[IntVariable] = output

  /** Returns the output variable associated to the given vehicle. */
  def apply(vehicle: Int): IntVariable = output(vehicle)

  override protected def performPrecomputation(vehicle: Int, routes: IntSequence): Unit = {
    require(
      vehicle < vrp.v,
      s"The value $vehicle is not a vehicle in the given sequence (must be < ${vrp.v})"
    )

    var nbNode: Long = 1L
    precomputedValues(vehicle) = nbNode
    var exp: IntSequenceExplorer = routes.explorerAtAnyOccurrence(vehicle).get.next
    while (exp.position < routes.size && exp.value >= vrp.v) {
      nbNode += 1L
      precomputedValues(exp.value) = nbNode
      exp = exp.next
    }

  }

  override protected def computeVehicleValue(
    vehicle: Int,
    segments: List[Segment],
    routes: IntSequence
  ): Long = {
    segments.foldLeft(0L)((numNodes, seg) => {
      val start = seg.startNode()
      val end   = seg.endNode()
      // If the segment is flipped, precomputedValues(end) <= precomputedValues(start).
      // For this case we need the absolute value.
      numNodes + (precomputedValues(end) - precomputedValues(start)).abs + 1
    })
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