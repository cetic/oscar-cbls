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

import oscar.cbls.VRP
import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer, RootIntSequenceExplorer}
import oscar.cbls.core.computation.genericConstraint.logReducedSegment._
import oscar.cbls.core.computation.genericConstraint.segment._

/** The log-reduced global constraint is a secondary approach to pre-computation and constraint
  * evaluation.
  *
  * In the original global constraint, the pre-computation computes the value over each
  * sub-sequences of node. This can be really expensive (O(n²)) but the constraint evaluation during
  * the neighborhood exploration is in O(s) where s in the number of segments. The secondary
  * approach aims to reduce the complexity of the pre-computation to the detriment of the constraint
  * evaluation's complexity. Leading to a complexity of O(n) for the precomputation phase and a
  * complexity of O(log n) for the evaluation of a solution.
  *
  * __How does it work ?__
  *
  * Classical global constraint :
  *   - For each pair of nodes in a vehicle's route, precompute some data ([[T]]) to speed up the
  *     constraint evaluation phase.
  *   - Cut the [[oscar.cbls.algo.sequence.IntSequence]] into [[Segment]] based on
  *     insertion/moving/removing coordinates.
  *   - Use [[Segment]] precomputed data to compute the output value as fast as possible.
  *
  * Log reduced global constraint :
  *   - Decompose each route in steps of size (1,2,4,8,16...) (2^0^, 2^1^, 2^2^...) (see
  *     [[decomposeToBitNumbersMSBFirst]] for more details)
  *   - For each step, precompute some data ([[T]]) to speed up the constraint evaluation phase.
  *   - Cut the [[oscar.cbls.algo.sequence.IntSequence]] into [[Segment]] based on
  *     insertion/moving/removing coordinates.
  *   - Decorate the [[Segment]] to form [[LogReducedSegment]] (see [[decorateSegments]] for more
  *     details).
  *   - Use [[LogReducedSegment]] precomputed data to compute the output value as fast as possible.
  *
  * @param vrp
  *   The object that represents the Vehicle Routing Problem.
  * @param name
  *   The (optional) name of the Invariant.
  * @tparam T
  *   Parametrized type that represents the value associated to each precomputed step. For example,
  *   type `Long` could be associated to `RouteLength`; in that case, each precomputed step would
  *   contain the length of this step.
  * @tparam U
  *   Parametrized type that represents the output type of the constraint, for example, `Long` for
  *   `RouteLength` (the total distance).
  */
abstract class LogReducedGlobalConstraint[T: Manifest, U: Manifest](vrp: VRP, name: Option[String])
    extends GlobalConstraintCore[U](vrp, name) {

  /** Returns the precomputed value of type T associated with the node.
    */
  def nodeValue(node: Int): T

  /** Returns the end node precomputed value of type T associated with the vehicle.
    */
  def endNodeValue(vehicle: Int): T

  /** Composes the precomputed value of two steps.
    *
    * @param firstStep
    *   The value (T) associated to the first step.
    * @param secondStep
    *   The value (T) associated to the second step.
    * @return
    *   The value (T) associated to the combination of the first step followed by the second step.
    */
  def composeSteps(firstStep: T, secondStep: T): T

  /** Computes the value of the constraint associated to a specific vehicle.
    *
    * @param vehicle
    *   The vehicle whose associated constraint value is needed.
    * @param segments
    *   The list of LogReducedSegment which, when combined, formed the route of the vehicle. Each
    *   LRS contains the list of step that formed it.
    * @return
    *   The constraint value associated with the vehicle. This value should only be computed based
    *   on the provided segments.
    */
  def computeVehicleValueComposed(vehicle: Int, segments: List[LogReducedSegment[T]]): U

  /** A node and all its precomputed steps.
    *
    * @param node
    *   The node.
    * @param steps
    *   Its steps.
    */
  private class NodeAndSteps(val node: Int, var steps: Array[T] = null) {
    override def toString: String = {
      s"NodeAndSteps(node:$node precomputes:${if (steps == null) "null"
        else steps.mkString(",")})"
    }
  }

  /** A node associated with its vehicle and position in its vehicle's route.
    *
    * @param vehicle
    *   The vehicle that reaches the node.
    * @param positionInVehicleRoute
    *   The position of the node in the vehicle's route.
    * @param node
    *   The node.
    */
  private case class VehicleAndPosition(vehicle: Int, positionInVehicleRoute: Int, node: Int)

  /** For each vehicle, contains the route as an array of NodeAndSteps. */
  private val vehicleToNodeAndSteps: Array[Array[NodeAndSteps]] = Array.fill(vrp.v)(null)

  /** An array containing the [[VehicleAndPosition]] of each node. */
  private val preComputedVals: Array[VehicleAndPosition] = Array.fill(vrp.n)(null)

  override def performPrecomputation(vehicle: Int, routes: IntSequence): Unit = {
    // identifies all nodes.
    identifyNodesAndAllocate(routes.explorerAtAnyOccurrence(vehicle).get, vehicle, 0)

    if (vehicleToNodeAndSteps(vehicle).length > 1) {
      // Decomposes and defines the different level of steps for this route.
      var sequenceOfLevels = decomposeToBitNumbersMSBFirst(vehicleToNodeAndSteps(vehicle).length)

      var positionInRoute = 0
      while (sequenceOfLevels.nonEmpty) {
        val currentLevel = sequenceOfLevels.head
        sequenceOfLevels = sequenceOfLevels.tail

        // Using the defined level, creates all the steps of this route.
        decorateAndAllocate(vehicle, positionInRoute, currentLevel, allocateFirst = true)
        positionInRoute += 1 << currentLevel
      }
    }
  }

  /** Goes through the vehicle's route. Identifies nodes, allocates enough space for future steps
    * and initiate some values.
    *
    * @param explorer
    *   The IntSequenceExplorer exploring the route of vehicle (starting at vehicle's position)
    * @param vehicle
    *   The vehicle whose related values are precomputed.
    * @param positionInVehicleRoute
    *   The current position in vehicle's route (starting at 0).
    */
  private def identifyNodesAndAllocate(
    explorer: IntSequenceExplorer,
    vehicle: Int,
    positionInVehicleRoute: Int
  ): Unit = {
    explorer match {
      case _: RootIntSequenceExplorer =>
        // Allocate the right array to contain all NodeAndSteps of the route.
        vehicleToNodeAndSteps(vehicle) = Array.fill(positionInVehicleRoute + 1)(null)
        vehicleToNodeAndSteps(vehicle)(positionInVehicleRoute) = new NodeAndSteps(vehicle)

      case x if x.value < vrp.v && x.value != vehicle =>
        // Allocate the right array to contain all NodeAndSteps of the route.
        vehicleToNodeAndSteps(vehicle) = Array.fill(positionInVehicleRoute + 1)(null)
        vehicleToNodeAndSteps(vehicle)(positionInVehicleRoute) = new NodeAndSteps(vehicle)

      case x =>
        // Identify the node as part of the route of vehicle
        preComputedVals(x.value) = VehicleAndPosition(vehicle, positionInVehicleRoute, x.value)

        identifyNodesAndAllocate(x.next, vehicle, positionInVehicleRoute + 1)

        // Initiate the NodeAndSteps of this node.
        vehicleToNodeAndSteps(vehicle)(positionInVehicleRoute) = new NodeAndSteps(x.value)
    }
  }

  /** Considering the binary value of x, returns the position of the bit of value 1.
    *
    * Ex :
    *   - 1 => 1 => List(0)
    *   - 4 => 100 => List(2)
    *   - 7 => 111 => List(2,1,0)
    *
    * It will then be used to determine the level of step and the steps' length. Ex :
    *   - 5 => 101 => List(2,0)
    *     - First group, 2+1 levels :
    *       - first level length = 2^0^ (1) => (0),(1),(2),(3)
    *       - second level length = 2^1^ (2) => (0,1),(2,3)
    *       - Third level length = 2^2^ (4) => (0,1,2,3)
    *     - Second group, 0+1 level :
    *       - first level length = 2^0^ (1) => (4)
    *
    * @param x
    *   The length of the route we want to decompose.
    * @return
    *   The list of position of bit of value 1.
    */
  private def decomposeToBitNumbersMSBFirst(x: Int): List[Int] = {
    require(x >= 0)

    var remaining = x
    var offset    = 0
    var toReturn  = List.empty[Int]

    while (remaining != 0) {
      if ((remaining & 1) != 0) {
        toReturn = offset :: toReturn
        remaining = remaining ^ 1
      }
      remaining = remaining >> 1
      offset = offset + 1
    }
    toReturn
  }

  /** Based on decomposed values (see [[decomposeToBitNumbersMSBFirst]]), creates steps and allocate
    * their value T.
    *
    * Based on the level :
    *   - If level == 0 => Setting the value T for the step of size 1 at positionInRoute.
    *   - else => creating recursively the two steps at level-1 and then compose them.
    *
    * @param vehicle
    *   The current vehicle.
    * @param positionInRoute
    *   The current position in vehicle's route.
    * @param level
    *   The current level.
    * @param allocateFirst
    *   If we need to allocate the array for the current position steps.
    */
  private def decorateAndAllocate(
    vehicle: Int,
    positionInRoute: Int,
    level: Int,
    allocateFirst: Boolean
  ): Unit = {

    if (allocateFirst)
      vehicleToNodeAndSteps(vehicle)(positionInRoute).steps =
        Array.fill(level + 1)(null.asInstanceOf[T])

    if (level == 0) {
      val precompute = vehicleToNodeAndSteps(vehicle)(positionInRoute)
      val node       = precompute.node

      if (node == vehicle && positionInRoute != 0)
        precompute.steps(0) = endNodeValue(node)
      else
        precompute.steps(0) = nodeValue(node)

    } else {

      val stepSize = 1 << (level - 1)

      // Decorates and allocates the two lower level steps.
      decorateAndAllocate(vehicle, positionInRoute, level - 1, allocateFirst = false)
      decorateAndAllocate(vehicle, positionInRoute + stepSize, level - 1, allocateFirst = true)

      // Composes them.
      vehicleToNodeAndSteps(vehicle)(positionInRoute).steps(level) = composeSteps(
        vehicleToNodeAndSteps(vehicle)(positionInRoute).steps(level - 1),
        vehicleToNodeAndSteps(vehicle)(positionInRoute + stepSize).steps(level - 1)
      )
    }
  }

  override def computeVehicleValue(
    vehicle: Int,
    segments: List[Segment],
    routes: IntSequence
  ): U = {
    // Decorates segments with steps and then uses the method adapted to LogReducedSegment.
    computeVehicleValueComposed(vehicle, decorateSegments(vehicle, segments))
  }

  /** For each Segment, selects the proper steps to form it.
    *
    * @param vehicle
    *   The vehicle for which the constraint value is asked.
    * @param segments
    *   The list of segment representing the move applied to the vehicle's route.
    * @return
    *   The corresponding list of LogReducedSegment.
    */
  private def decorateSegments(
    vehicle: Int,
    segments: List[Segment]
  ): List[LogReducedSegment[T]] = {

    segments match {
      case Nil =>
        // The last Segment was not originally part of the vehicle's route. We must add the "return to depot" node.
        List(
          LogReducedPreComputedSegment[T](vehicle: Int, vehicle: Int, List(endNodeValue(vehicle)))
        )

      case head :: tail =>
        head match {
          case PrecomputedSubSequence(startNode: Int, endNode: Int, _) =>
            val startNodeValue = preComputedVals(startNode)
            val endNodeValue   = preComputedVals(endNode)
            if (
              tail.isEmpty && startNodeValue.vehicle == vehicle
              && endNodeValue.positionInVehicleRoute == vehicleToNodeAndSteps(vehicle).length - 2
            ) {
              // Last SubSequence of this vehicle, untouched.
              List(
                LogReducedPreComputedSegment[T](
                  startNode,
                  vehicle, // we set vehicle as the real end
                  extractSequenceOfT(
                    startNodeValue.vehicle,
                    startNodeValue.positionInVehicleRoute,
                    vehicleToNodeAndSteps(vehicle).length - 1,
                    flipped = false
                  )
                )
              )

            } else {
              // Not the last one or originally part of another vehicle's route.
              List(
                LogReducedPreComputedSegment[T](
                  startNode,
                  endNode,
                  extractSequenceOfT(
                    startNodeValue.vehicle,
                    startNodeValue.positionInVehicleRoute,
                    endNodeValue.positionInVehicleRoute,
                    flipped = false
                  )
                )
              ) ::: decorateSegments(vehicle, tail)
            }
          case FlippedPreComputedSubSequence(startNode: Int, endNode: Int, _) =>
            // Flipped so either there are other Segment after it or we need to add a return to depot node after it.
            val startNodeValue = preComputedVals(startNode)
            val endNodeValue   = preComputedVals(endNode)
            List(
              LogReducedFlippedPreComputedSegment[T](
                startNode: Int,
                endNode: Int,
                extractSequenceOfT(
                  startNodeValue.vehicle,
                  startNodeValue.positionInVehicleRoute,
                  endNodeValue.positionInVehicleRoute,
                  flipped = true
                )
              )
            ) ::: decorateSegments(vehicle, tail)

          case NewNode(node: Int) =>
            List(LogReducedNewNode[T](node: Int, value = nodeValue(node))) :::
              decorateSegments(vehicle, tail)
        }
    }
  }

  /** Extracts the precomputed values corresponding to the SubSequence defined by vehicle, start and
    * end position.
    *
    * Flips the start and end position if needed.
    *
    * @param vehicle
    *   The vehicle corresponding to the SubSequence.
    * @param startPositionInRoute
    *   The start position of the SubSequence.
    * @param endPositionInRoute
    *   The end position of the SubSequence.
    * @param flipped
    *   If the SubSequence is flipped.
    * @return
    *   An ordered list of precomputed values corresponding to the SubSequence.
    */
  private def extractSequenceOfT(
    vehicle: Int,
    startPositionInRoute: Int,
    endPositionInRoute: Int,
    flipped: Boolean
  ): List[T] = {

    if (flipped) {
      extractSequenceOfTUnFlippedGoingUp(
        vehicleToNodeAndSteps(vehicle),
        startPositionInRoute = endPositionInRoute,
        endPositionInRoute = startPositionInRoute
      )
    } else {
      extractSequenceOfTUnFlippedGoingUp(
        vehicleToNodeAndSteps(vehicle),
        startPositionInRoute = startPositionInRoute,
        endPositionInRoute = endPositionInRoute
      )
    }
  }

  /** Extracts the sequence of pre-computed values corresponding to the SubSequence defined by start
    * and end positions.
    *
    * Summary : Recursively gets the highest segment starting at startNode and fitting in the
    * SubSequence length. After selecting a segment, increases startPositionInRoute by the segment's
    * length. Going up or down means increasing or decreasing (respectively) the segment's size.
    *
    * @param vehiclePreComputes
    *   An array containing the pre-computed value of the current vehicle.
    * @param startPositionInRoute
    *   The starting position of the next segment (starting at current SubSequence's start).
    * @param endPositionInRoute
    *   The ending position of the current SubSequence.
    * @return
    *   An ordered list of the pre-computed values corresponding to the SubSequence.
    */
  private def extractSequenceOfTUnFlippedGoingUp(
    vehiclePreComputes: Array[NodeAndSteps],
    startPositionInRoute: Int,
    endPositionInRoute: Int
  ): List[T] = {

    if (startPositionInRoute == endPositionInRoute + 1) return Nil

    // Max Segment's size starting at startPositionInRoute = 2^maxLevel
    val maxLevel    = vehiclePreComputes(startPositionInRoute).steps.length - 1
    val segmentSize = 1 << maxLevel

    if (startPositionInRoute + segmentSize > endPositionInRoute + 1) {
      // The level (nb of nodes in the same segment is too high considering the subsequence length).
      // Note : Once we reach this point, all segments have size lower than the actual segmentSize.
      extractSequenceOfTUnFlippedGoingDown(
        vehiclePreComputes,
        startPositionInRoute,
        endPositionInRoute,
        maxLevel - 1
      )
    } else {
      // Segment's max size fits in the SubSequence length, maybe next one too.
      List(vehiclePreComputes(startPositionInRoute).steps(maxLevel)) :::
        extractSequenceOfTUnFlippedGoingUp(
          vehiclePreComputes,
          startPositionInRoute + segmentSize,
          endPositionInRoute
        )
    }
  }

  /** Extracts the sequence of pre-computed values corresponding to the SubSequence defined by start
    * and end positions.
    *
    * Keeps the same size of segment or decreases it.
    *
    * @param vehiclePreComputes
    *   An array containing the pre-computed value of the current vehicle.
    * @param startPositionInRoute
    *   The starting position of the next segment.
    * @param endPositionInRoute
    *   The ending position of the current SubSequence.
    * @param maxLevel
    *   The current max level where segment's size = 2^maxLevel^
    * @return
    *   An ordered list of the pre-computed values corresponding to the SubSequence.
    */
  private def extractSequenceOfTUnFlippedGoingDown(
    vehiclePreComputes: Array[NodeAndSteps],
    startPositionInRoute: Int,
    endPositionInRoute: Int,
    maxLevel: Int
  ): List[T] = {

    if (startPositionInRoute == endPositionInRoute + 1) return Nil

    val segmentSize = 1 << maxLevel

    if (startPositionInRoute + segmentSize > endPositionInRoute + 1) {
      // The segment's size does not fit in the remaining of the SubSequence. Decrease it.
      extractSequenceOfTUnFlippedGoingDown(
        vehiclePreComputes,
        startPositionInRoute,
        endPositionInRoute,
        maxLevel - 1
      )
    } else {
      // The segment's size fit, take that segment and proceed with next one.
      List(vehiclePreComputes(startPositionInRoute).steps(maxLevel)) :::
        extractSequenceOfTUnFlippedGoingDown(
          vehiclePreComputes,
          startPositionInRoute + segmentSize,
          endPositionInRoute,
          maxLevel - 1
        )
    }
  }
}
