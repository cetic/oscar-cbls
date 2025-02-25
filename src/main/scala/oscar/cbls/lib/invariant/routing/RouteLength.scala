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
import oscar.cbls.core.computation.genericConstraint.GlobalConstraintCore
import oscar.cbls.core.computation.genericConstraint.segment._
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.lib.invariant.routing.RouteLength.PrecomputedDistance
import oscar.cbls.modeling.routing.VRP

/** Companion object of the [[RouteLength]] class. */
object RouteLength {

  /** Creates a RouteLength invariant, which maintains the length of the routes for each vehicle of
    * a VRP.
    *
    * @param vrp
    *   The object that represents the Vehicle Routing Problem.
    * @param distanceFunction
    *   A function that, given two nodes, returns the distance between the two nodes.
    */
  def apply(vrp: VRP, distanceFunction: Int => Int => Long): RouteLength = {
    val output: Array[IntVariable] = Array.fill(vrp.n)(IntVariable(vrp.model, 0L))
    new RouteLength(vrp, output, distanceFunction, Some("RouteLength"))
  }

  /** Creates a RouteLength invariant, which maintains the length of the routes for each vehicle of
    * a VRP.<br>
    *
    * This method instantiates the invariant with a distance matrix. When the matrix is symmetric,
    * we can make memory economies by giving only the values above the main diagonal. In that case,
    * set the `matrixIsTriangular` flag to `true`.
    *
    * @param vrp
    *   The object that represents the Vehicle Routing Problem.
    * @param distanceMatrix
    *   The distance matrix between the points of the problem.
    * @param matrixIsTriangular
    *   Flag expressing whether we are using a sparse matrix to represent a symmetric one.
    */
  def apply(
    vrp: VRP,
    distanceMatrix: Array[Array[Long]],
    matrixIsTriangular: Boolean = false
  ): RouteLength = {
    val output: Array[IntVariable] = Array.fill(vrp.n)(IntVariable(vrp.model, 0L))
    val distFct: Int => Int => Long = (i: Int) =>
      (j: Int) => {
        val toReturn = {
          if (!matrixIsTriangular) distanceMatrix(i)(j)
          else if (i <= j) distanceMatrix(i)(j - i)
          else distanceMatrix(j)(i - j)
        }
        toReturn
      }

    new RouteLength(vrp, output, distFct, Some("RouteLength"))
  }

  /** Case class used to save, for each node, the distance from the vehicle to the node and the
    * distance from the node to the vehicle. It is used for the precomputations.
    */
  case class PrecomputedDistance(distanceFromStart: Long, distanceToStart: Long)
}

/** An invariant which maintains the length of the routes for each vehicle of a VRP.<br>
  *
  * When precomputation are performed, we compute for each node the distance from the vehicle to the
  * node and the distance from the node to the vehicle.
  *
  * @param vrp
  *   The object that represents the Vehicle Routing Problem.
  * @param output
  *   Array maintaining the length of each vehicle.
  * @param distanceFunction
  *   A function that, given two nodes, returns the distance between the two nodes.
  * @param name
  *   The (optional) name of the Invariant.
  */
class RouteLength(
  vrp: VRP,
  output: Array[IntVariable],
  distanceFunction: Int => Int => Long,
  name: Option[String]
) extends GlobalConstraintCore[Long](vrp, name) {

  output.foreach(_.setDefiningInvariant(this))

  val precomputedValues: Array[PrecomputedDistance] = Array.fill(vrp.n)(PrecomputedDistance(0L, 0L))

  /** Returns the output variables of the invariant. */
  def apply(): Array[IntVariable] = output

  /** Returns the output variable associated to the given vehicle. */
  def apply(vehicle: Int): IntVariable = output(vehicle)

  override protected def performPrecomputation(vehicle: Int, routes: IntSequence): Unit = {
    var prevNode: Int                           = vehicle
    var prevPrecomputedVal: PrecomputedDistance = PrecomputedDistance(0L, 0L)
    precomputedValues(vehicle) = prevPrecomputedVal

    var exp: IntSequenceExplorer = routes.explorerAtAnyOccurrence(vehicle).get.next
    while (exp.position < routes.size && exp.value >= vrp.v) {
      prevPrecomputedVal = PrecomputedDistance(
        prevPrecomputedVal.distanceFromStart + distanceFunction(prevNode)(exp.value),
        prevPrecomputedVal.distanceToStart + distanceFunction(exp.value)(prevNode)
      )

      prevNode = exp.value
      precomputedValues(prevNode) = prevPrecomputedVal

      exp = exp.next
    }
  }

  override protected def computeVehicleValue(
    vehicle: Int,
    segments: List[Segment],
    routes: IntSequence
  ): Long = {

    def digestListOfSegments(segments: List[Segment], prevNode: Int): Long = {
      segments match {
        case Nil => distanceFunction(prevNode)(vehicle)
        case seg :: tail =>
          seg match {
            case PrecomputedSubSequence(startNode, endNode, _) =>
              val distanceToEnter =
                if (prevNode == -1) 0L else distanceFunction(prevNode)(startNode)
              val length =
                precomputedValues(endNode).distanceFromStart -
                  precomputedValues(startNode).distanceFromStart

              require(length >= 0, s"Got a negative segment length ($length).")

              distanceToEnter + length + digestListOfSegments(tail, endNode)

            case FlippedPreComputedSubSequence(startNode, endNode, _) =>
              val distanceToEnter =
                if (prevNode == -1) 0L else distanceFunction(prevNode)(startNode)
              val length = precomputedValues(startNode).distanceToStart -
                precomputedValues(endNode).distanceToStart

              require(length >= 0, s"Got a negative segment length ($length).")

              distanceToEnter + length + digestListOfSegments(tail, endNode)

            case NewNode(node) =>
              val distanceToEnter =
                if (prevNode == -1) 0L else distanceFunction(prevNode)(node)

              distanceToEnter + digestListOfSegments(tail, node)
          }
      }
    }

    digestListOfSegments(segments, -1)
  }

  override protected def assignVehicleValue(vehicle: Int, value: Long): Unit = {
    output(vehicle) := value
  }

  override protected def computeVehicleValueFromScratch(vehicle: Int, routes: IntSequence): Long = {
    var prevNode = vehicle
    var toReturn = 0L

    var exp: IntSequenceExplorer = routes.explorerAtAnyOccurrence(vehicle).get.next

    while (exp.position < routes.size && exp.value >= vrp.v) {
      toReturn += distanceFunction(prevNode)(exp.value)
      prevNode = exp.value
      exp = exp.next
    }

    toReturn + distanceFunction(prevNode)(vehicle)
  }

}
