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

package oscar.cbls.lib.invariant.routing.abstractGenericConstraint

import oscar.cbls.VRS
import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer, RootIntSequenceExplorer}
import oscar.cbls.lib.invariant.routing.abstractGenericConstraint.logReducedSegment._
import oscar.cbls.lib.invariant.routing.abstractGenericConstraint.segment._
import oscar.cbls.lib.invariant.routing.abstractGenericConstraint.transferFunction.TransferFunction

import scala.annotation.tailrec

/** Defines a global constraint based on transfer functions.
  *
  * In routing the global constraint is applied over the whole route. For instance a global
  * constraint maintaining whether there is a time window/capacity violation or not.
  *
  * A transfer function is a small input/output function where the output reveal some information
  * about the constraint. For instance in a time-window constraint, the input would be the arrival
  * time at the node and the output the leaving time (or a dedicated value for violation).
  *
  * @param vrs
  *   The object that represents the vehicle routing structure.
  * @param withLogReduction
  *   If true the log reduction algorithm will be activated.
  * @param withExtremesPC
  *   If true classical pre-computation will be applied for each pair of node starting at vehicle's
  *   depot and ending in the vehicle's route. And also for each pair of node starting at the end of
  *   the route and ending in the vehicle's route. (Useless without using log reduction as well)
  * @param name
  *   The (optional) name of the Invariant.
  * @tparam U
  *   Parametrized type that represents the output type of the constraint, for example, `Long` for
  *   `RouteLength` (the total distance).
  */
abstract class TransferFunctionBasedGlobalConstraint[U: Manifest](
  vrs: VRS,
  withLogReduction: Boolean = false,
  withExtremesPC: Boolean = false,
  name: Option[String]
) extends GlobalConstraintCore[U](vrs, name) {

  ///////////////////////////////////////////
  /* Non Log Reduction specifics variables */

  private lazy val fullPrecomputedTF: Array[Array[TransferFunction[U]]] =
    Array.tabulate(vrs.n, vrs.n)((from, to) => if (from == to) nodeValue(from) else null)

  ///////////////////////////////////////
  /* Log Reduction specifics variables */

  /** For each vehicle, contains the route as an array of NodeAndSteps. */
  private lazy val vehicleToNodeAndSteps: Array[Array[NodeAndSteps]] = Array.fill(vrs.v)(null)

  /** An array containing the [[VehicleAndPosition]] of each node. */
  private lazy val preComputedVals: Array[VehicleAndPosition] = Array.fill(vrs.n)(null)

  private lazy val vehicleToExtremePrecomputes: Array[Array[NodeAndExtremePreComputes]] =
    Array.fill(vrs.v)(null)

  /////////////////////////
  /* Methods to override */

  /** Returns the precomputed value of type TransferFunction associated with the node.
    */
  def nodeValue(node: Int): TransferFunction[U]

  /** Returns the end node precomputed value of type TransferFunction associated with the vehicle.
    */
  def endNodeValue(vehicle: Int): TransferFunction[U]

  /** Checks if the given transferFunction, starting at depot, leads to a constraint violation.
    *
    * If it leads to a constraint violation, the implementation should provide the value of type U
    * that'll be interpreted properly when assigning the value to the output variable.
    *
    * If constraint violation does not apply, simply return false.
    *
    * @param vehicle
    *   The vehicle starting at depot.
    * @param transferFunction
    *   The transferFunction that is tested.
    * @return
    *   True if a constraint violation is detected. False otherwise.
    */
  def checkViolationStartingAtDepot(vehicle: Int, transferFunction: TransferFunction[U]): Boolean

  /** Returns the value of the global constraint based on the given transfer function which starts
    * at depot.
    *
    * @param vehicle
    *   The vehicle starting at depot.
    * @param transferFunction
    *   A transfer function starting at depot.
    * @return
    *   The value of the global constraint.
    */
  private def valueStartingAtDepot(vehicle: Int, transferFunction: TransferFunction[U]): U =
    transferFunction.apply()

  ////////////////////////////////////
  /* Global constraint core methods */

  override protected def performPrecomputation(vehicle: Int, routes: IntSequence): Unit = {
    if (withLogReduction) {
      logReducedPrecomputation(vehicle, routes)
    } else {
      val route =
        routes.explorerAtAnyOccurrence(vehicle).get.forward.valuesUntilValue((vehicle + 1) % vrs.v)
      recursivePreComputationColumn(route)
    }
  }

  override protected def computeVehicleValue(
    vehicle: Int,
    segments: List[Segment],
    routes: IntSequence
  ): U = {
    if (withLogReduction) {
      computeValuesBasedOnLRSegment(vehicle, decorateSegments(vehicle, segments))
    } else {
      computeValueBasedOnSegments(vehicle, segments)
    }
  }

  ////////////////////////////////
  /* NON LOG-REDUCED algorithms */

  /** Recursively composes all provided segments' TF and eventually returns the value of the global
    * constraint of the vehicle.
    *
    * @note
    *   At each step it checks if there is a violation, if so it just returns the violation value.
    *
    * @param segments
    *   The provided segments.
    * @param currentTF
    *   The current composed transfer function.
    * @return
    *   The value of the global constraint.
    */
  @tailrec
  private def computeValueBasedOnSegments(
    vehicle: Int,
    segments: List[Segment],
    currentTF: Option[TransferFunction[U]] = None
  ): U = {
    if (currentTF.nonEmpty) {
      val violationIsDetected = checkViolationStartingAtDepot(vehicle, currentTF.get)
      if (violationIsDetected) return valueStartingAtDepot(vehicle, currentTF.get)
    }

    require(segments.nonEmpty || currentTF.nonEmpty, "No segment were provided.")
    segments match {
      case Nil =>
        valueStartingAtDepot(
          vehicle,
          currentTF.get.compose(endNodeValue(vehicle), otherFlipped = false)
        )
      case segment :: tail =>
        val mFrom = if (segment.isFlipped) segment.endNode() else segment.startNode()
        val mTo   = if (segment.isFlipped) segment.startNode() else segment.endNode()
        computeValueBasedOnSegments(
          vehicle,
          tail,
          Some(
            if (currentTF.isEmpty) fullPrecomputedTF(mFrom)(mTo)
            else
              currentTF.get.compose(fullPrecomputedTF(mFrom)(mTo), segment.isFlipped)
          )
        )
    }
  }

  /** Recursively selects the next element (to) and precompute from -> to.
    *
    * Starting with the TF of node from as prevTF, we recursively compose prevTF with the TF of node
    * to.
    *
    * @param from
    *   The unique starting node of each TF that will be precomputed this round.
    * @param tos
    *   The ending nodes of each TF that will be precomputed this round.
    * @param prevTF
    *   The previous precomputed TF.
    */
  @tailrec
  private def recursivePreComputationRow(
    from: Int,
    tos: List[Int],
    prevTF: TransferFunction[U]
  ): Unit = {
    tos match {
      case to :: tail =>
        fullPrecomputedTF(from)(to) =
          prevTF.compose(fullPrecomputedTF(to)(to), otherFlipped = false)
        recursivePreComputationRow(from, tail, fullPrecomputedTF(from)(to))
      case _ =>
    }
  }

  /** Recursively selects the next element and calls the "row" precomputation.
    *
    * Since each TF holds its forward and backward values, we only need to cover the upper diagonal
    * of the matrix.
    *
    * @param route
    *   The route of the vehicle for which we want to precompute TFs.
    */
  @tailrec
  private def recursivePreComputationColumn(route: List[Int]): Unit = {
    route match {
      case from :: tail =>
        recursivePreComputationRow(from, tail, fullPrecomputedTF(from)(from))
        recursivePreComputationColumn(tail)
      case _ =>
    }
  }

  ////////////////////////////
  /* LOG-REDUCED algorithms */

  @tailrec
  private def extremesPrecomputationForward(
    vehicle: Int,
    posInVehicleRoute: Int,
    lastTF: Option[TransferFunction[U]] = None
  ): Unit = {
    val currentExtreme = vehicleToExtremePrecomputes(vehicle)(posInVehicleRoute)
    if (posInVehicleRoute == 0)
      currentExtreme.fromStart = nodeValue(vehicle)
    else if (currentExtreme.node == vehicle)
      currentExtreme.fromStart = lastTF.get.compose(endNodeValue(vehicle), otherFlipped = false)
    else
      currentExtreme.fromStart =
        lastTF.get.compose(nodeValue(currentExtreme.node), otherFlipped = false)

    if (currentExtreme.node != vehicle || posInVehicleRoute == 0)
      extremesPrecomputationForward(vehicle, posInVehicleRoute + 1, Some(currentExtreme.fromStart))
  }

  @tailrec
  private def extremesPrecomputationBackward(
    vehicle: Int,
    posInVehicleRoute: Int,
    lastTF: Option[TransferFunction[U]] = None
  ): Unit = {
    val currentExtreme = vehicleToExtremePrecomputes(vehicle)(posInVehicleRoute)
    if (currentExtreme.node == vehicle && posInVehicleRoute != 0)
      currentExtreme.toEnd = endNodeValue(vehicle)
    else
      currentExtreme.toEnd =
        nodeValue(currentExtreme.node).compose(lastTF.get, otherFlipped = false)
    if (posInVehicleRoute != 0)
      extremesPrecomputationBackward(vehicle, posInVehicleRoute - 1, Some(currentExtreme.toEnd))
  }

  /** Precomputes [[LogReducedSegment]] and optionally extremes based on the new value of vehicle's
    * route.
    *
    * @param vehicle
    *   The concerned vehicle.
    * @param routes
    *   The new route of the vehicle.
    */
  private def logReducedPrecomputation(vehicle: Int, routes: IntSequence): Unit = {
    // identifies all nodes.
    identifyNodesAndAllocate(routes.explorerAtAnyOccurrence(vehicle).get, vehicle, 0)

    if (withExtremesPC) {
      extremesPrecomputationForward(vehicle, 0)
      extremesPrecomputationBackward(vehicle, vehicleToExtremePrecomputes(vehicle).length - 1)
    }

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

  /** Given the list of [[TransferFunction]], computes the resulting [[TransferFunction]] and its
    * value.
    *
    * Iteratively :
    *   - Selects the next LRS.
    *   - Composes its list of TransferFunction with the currentTF.
    *   - If a violation is detected, return the currentTF value, else iterate.
    *
    * @param vehicle
    *   The considered vehicle.
    * @param steps
    *   The list of TransferFunction.
    * @param flipped
    *   Whether these TransferFunction are considered flipped.
    * @param currentTF
    *   The current TransferFunction, that will be composed with the next TransferFunction.
    * @return
    *   The value of the resulting TransferFunction.
    */
  @tailrec
  private def composeStepsList(
    vehicle: Int,
    steps: List[TransferFunction[U]],
    flipped: Boolean,
    currentTF: Option[TransferFunction[U]] = None
  ): TransferFunction[U] = {
    require(steps.nonEmpty || currentTF.nonEmpty, "No transfer function were provided.")
    if (currentTF.isDefined) {
      val violationIsDetected = checkViolationStartingAtDepot(vehicle, currentTF.get)
      if (violationIsDetected) return currentTF.get
    }
    steps match {
      case Nil => currentTF.get
      case tf :: tail =>
        if (currentTF.isDefined)
          composeStepsList(
            vehicle,
            tail,
            flipped,
            Some(currentTF.get.compose(tf, otherFlipped = flipped))
          )
        else {
          // First tf can not be flipped (starting at vehicle).
          composeStepsList(vehicle, tail, flipped, Some(tf))
        }
    }
  }

  /** Given the list of [[LogReducedSegment]], computes the resulting [[TransferFunction]] and its
    * value.
    *
    * Iteratively :
    *   - Selects the next LRS.
    *   - Composes its list of TransferFunction with the currentTF.
    *   - If a violation is detected, return the currentTF value, else iterate.
    *
    * @param vehicle
    *   The considered vehicle.
    * @param logReducedSegments
    *   The list of LogReducedSegment.
    * @param currentTF
    *   The current TransferFunction, that will be composed with the next TransferFunction.
    * @return
    *   The value of the resulting TransferFunction.
    */
  @tailrec
  private def computeValuesBasedOnLRSegment(
    vehicle: Int,
    logReducedSegments: List[LogReducedSegment[TransferFunction[U]]],
    currentTF: Option[TransferFunction[U]] = None
  ): U = {
    require(
      logReducedSegments.nonEmpty || currentTF.nonEmpty,
      "No log reduced segment were provided."
    )
    logReducedSegments match {
      case Nil =>
        valueStartingAtDepot(vehicle, currentTF.get)

      case lrs :: tail =>
        val newCurrentTF: TransferFunction[U] = lrs match {
          case LogReducedPreComputedSegment(_, _, steps) =>
            composeStepsList(vehicle, steps, flipped = false, currentTF)
          case LogReducedFlippedPreComputedSegment(_, _, steps) =>
            composeStepsList(vehicle, steps.reverse, flipped = true, currentTF)
          case LogReducedNewNode(_, value) =>
            composeStepsList(vehicle, List(value), flipped = false, currentTF)
          case x =>
            throw new Error(s"Unhandled match with $x")
        }
        val violationIsDetected = checkViolationStartingAtDepot(vehicle, newCurrentTF)
        if (violationIsDetected) return valueStartingAtDepot(vehicle, newCurrentTF)
        computeValuesBasedOnLRSegment(vehicle, tail, Some(newCurrentTF))
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
      case x
          if x.isInstanceOf[RootIntSequenceExplorer] || (x.value < vrs.v && x.value != vehicle) =>
        // Allocate the right array to contain all NodeAndSteps of the route.
        vehicleToNodeAndSteps(vehicle) = Array.fill(positionInVehicleRoute + 1)(null)
        vehicleToNodeAndSteps(vehicle)(positionInVehicleRoute) = new NodeAndSteps(vehicle)
        if (withExtremesPC) {
          vehicleToExtremePrecomputes(vehicle) = Array.fill(positionInVehicleRoute + 1)(null)
          vehicleToExtremePrecomputes(vehicle)(positionInVehicleRoute) =
            new NodeAndExtremePreComputes(vehicle)
        }

      case x =>
        // Identify the node as part of the route of vehicle
        preComputedVals(x.value) = VehicleAndPosition(vehicle, positionInVehicleRoute, x.value)

        identifyNodesAndAllocate(x.next, vehicle, positionInVehicleRoute + 1)

        // Initiate the NodeAndSteps of this node.
        vehicleToNodeAndSteps(vehicle)(positionInVehicleRoute) = new NodeAndSteps(x.value)
        if (withExtremesPC)
          vehicleToExtremePrecomputes(vehicle)(positionInVehicleRoute) =
            new NodeAndExtremePreComputes(x.value)
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
    * their value TransferFunction.
    *
    * Based on the level :
    *   - If level == 0 => Setting the value TransferFunction for the step of size 1 at
    *     positionInRoute.
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
        Array.fill(level + 1)(null.asInstanceOf[TransferFunction[U]])

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
      vehicleToNodeAndSteps(vehicle)(positionInRoute).steps(level) =
        vehicleToNodeAndSteps(vehicle)(positionInRoute)
          .steps(level - 1)
          .compose(
            vehicleToNodeAndSteps(vehicle)(positionInRoute + stepSize).steps(level - 1),
            otherFlipped = false
          )
    }
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
  ): List[LogReducedSegment[TransferFunction[U]]] = {

    segments match {
      case Nil =>
        // The last Segment was not originally part of the vehicle's route. We must add the "return to depot" node.
        List(
          LogReducedPreComputedSegment[TransferFunction[U]](
            vehicle: Int,
            vehicle: Int,
            List(endNodeValue(vehicle))
          )
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
                LogReducedPreComputedSegment[TransferFunction[U]](
                  startNode,
                  vehicle, // we set vehicle as the real end
                  if (withExtremesPC)
                    List(
                      vehicleToExtremePrecomputes(vehicle)(
                        startNodeValue.positionInVehicleRoute
                      ).toEnd
                    )
                  else
                    extractSequenceOfT(
                      startNodeValue.vehicle,
                      startNodeValue.positionInVehicleRoute,
                      vehicleToNodeAndSteps(vehicle).length - 1,
                      flipped = false
                    )
                )
              )

            } else if (startNodeValue.node == vehicle && withExtremesPC) {
              // Starting segment and with extremes
              List(
                LogReducedPreComputedSegment[TransferFunction[U]](
                  startNode,
                  endNode,
                  List(
                    vehicleToExtremePrecomputes(vehicle)(
                      endNodeValue.positionInVehicleRoute
                    ).fromStart
                  )
                )
              ) :::
                decorateSegments(vehicle, tail)

            } else {
              // Not the last one or originally part of another vehicle's route.
              List(
                LogReducedPreComputedSegment[TransferFunction[U]](
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
              LogReducedFlippedPreComputedSegment[TransferFunction[U]](
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
            List(LogReducedNewNode[TransferFunction[U]](node: Int, value = nodeValue(node))) :::
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
  ): List[TransferFunction[U]] = {
    extractSequenceOfTUnFlipped(
      vehicleToNodeAndSteps(vehicle),
      if (flipped) endPositionInRoute else startPositionInRoute,
      if (flipped) startPositionInRoute else endPositionInRoute,
      if (flipped) vehicleToNodeAndSteps(vehicle)(endPositionInRoute).steps.length - 1
      else vehicleToNodeAndSteps(vehicle)(startPositionInRoute).steps.length - 1
    )
  }

  /** Extracts the sequence of pre-computed values corresponding to the SubSequence defined by start
    * and end positions.
    *
    * Summary : Recursively gets the highest segment starting at startNode and fitting in the
    * SubSequence length. After selecting a segment, increases startPositionInRoute by the segment's
    * length. Going down means decreasing the segment's size if the actual size does not fit.
    *
    * @param vehiclePreComputes
    *   An array containing the pre-computed value of the current vehicle.
    * @param startPositionInRoute
    *   The starting position of the next segment (starting at current SubSequence's start).
    * @param endPositionInRoute
    *   The ending position of the current SubSequence.
    * @param maxLevel
    *   The current max level where segment's size = 2^maxLevel^
    * @param goingDown
    *   Whether we are in a decreasing-size phase
    * @return
    *   An ordered list of the pre-computed values corresponding to the SubSequence.
    */
  private def extractSequenceOfTUnFlipped(
    vehiclePreComputes: Array[NodeAndSteps],
    startPositionInRoute: Int,
    endPositionInRoute: Int,
    maxLevel: Int,
    goingDown: Boolean = false
  ): List[TransferFunction[U]] = {

    if (startPositionInRoute == endPositionInRoute + 1) return Nil

    // Max Segment's size starting at startPositionInRoute = 2^maxLevel
    val segmentSize = 1 << maxLevel

    if (startPositionInRoute + segmentSize > endPositionInRoute + 1) {
      // The segment's size does not fit in the remaining of the SubSequence. Decrease it.
      extractSequenceOfTUnFlipped(
        vehiclePreComputes,
        startPositionInRoute,
        endPositionInRoute,
        maxLevel - 1,
        goingDown = true
      )
    } else if (goingDown) {
      // The segment's size fit, take that segment and proceed with next one.
      List(vehiclePreComputes(startPositionInRoute).steps(maxLevel)) :::
        extractSequenceOfTUnFlipped(
          vehiclePreComputes,
          startPositionInRoute + segmentSize,
          endPositionInRoute,
          maxLevel - 1,
          goingDown = true
        )
    } else {
      // Segment's max size fits in the SubSequence length, maybe next one too.
      List(vehiclePreComputes(startPositionInRoute).steps(maxLevel)) :::
        extractSequenceOfTUnFlipped(
          vehiclePreComputes,
          startPositionInRoute + segmentSize,
          endPositionInRoute,
          maxLevel
        )
    }
  }

  /////////////////////
  /* Utility classes */

  /** A node and all its precomputed steps.
    *
    * @param node
    *   The node.
    * @param steps
    *   Its steps.
    */
  private class NodeAndSteps(val node: Int, var steps: Array[TransferFunction[U]] = null) {
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

  /** A node associated with its two extremes TransferFunction.
    *
    *   - First one : From vehicle to node.
    *   - Second one : From node to end of route.
    *
    * @param node
    *   The node
    * @param fromStart
    *   The TransferFunction starting at vehicle and ending at node.
    * @param toEnd
    *   The TransferFunction starting at node and ending at route's end.
    */
  private class NodeAndExtremePreComputes(
    val node: Int,
    var fromStart: TransferFunction[U] = null.asInstanceOf[TransferFunction[U]],
    var toEnd: TransferFunction[U] = null.asInstanceOf[TransferFunction[U]]
  ) {
    override def toString: String = {
      s"NodeAndExtremePreComputes(node:$node fromStart:$fromStart toEnd:$toEnd)"
    }
  }
}
