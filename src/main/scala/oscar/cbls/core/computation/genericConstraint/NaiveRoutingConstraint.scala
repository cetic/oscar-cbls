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

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.Invariant
import oscar.cbls.core.computation.genericConstraint.segment._
import oscar.cbls.core.computation.seq._
import oscar.cbls.modeling.routing.{StackedVehicleSearcher, VRS}

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

/** Defines the abstract naive routing constraint.<br>
  *
  * The constraint takes a type parameter `U`. It computes the value (of type `U`) associated to
  * each node using the function `fun`. The function `fun` must be able to compute the value for
  * node `to` using the node `from` and the value at node `from`. These computations include the
  * return to the depot.<br>
  *
  * The constraint is ''naive'' because it cannot make symbolic calculus. However, the constraint
  * only recomputes what is necessary.<br>
  *
  * The constraint uses a segment mechanism: after an update, the sequence is cut into segments that
  * represent the changes since the last time the constraint had been notified. <br>
  *
  * The constraint is independent of the direction of travel through the sequence. The sequence can
  * be travelled forward (see [[ForwardNaiveRoutingConstraint]]) or backward (see
  * [[BackwardNaiveRoutingConstraint]]).
  *
  * @param vrs
  *   The object that represents the vehicle routing structure.
  * @param defaultValueForUnroutedNodes
  *   The default value associated to unrouted nodes.
  * @param initValuePerVehicle
  *   The values used to start the computation along each vehicle routes (the beginning depends on
  *   the travel direction through the sequence).
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
abstract class NaiveRoutingConstraint[U: Manifest](
  vrs: VRS,
  defaultValueForUnroutedNodes: U,
  initValuePerVehicle: Array[U],
  fun: (Int, Int, U) => U,
  name: Option[String]
) extends Invariant(vrs.store, name)
    with SeqNotificationTarget {

  vrs.routes.registerStaticallyAndDynamicallyListeningElement(this)

  /** The current value of each node. */
  private[this] val valueOfNodes: Array[U] = Array.tabulate(vrs.n)(i => {
    if (i < vrs.v) initValuePerVehicle(i)
    else defaultValueForUnroutedNodes
  })

  private[this] var vehicleSearcher: StackedVehicleSearcher =
    StackedVehicleSearcher(vrs.routes.value(), vrs.v)

  /** This variable holds all the vehicles that have their routes during the notification digestion.
    */
  private[this] var changedVehiclesSinceLastNotified: HashSet[Int] = HashSet.empty

  /** The updated list of Segments which defines the precomputation that can used. Here the vehicle
    * nodes are not included in the segments.
    */
  private[this] val segmentsOfVehicle: Array[VehicleSegments] = new Array[VehicleSegments](vrs.v)

  // Initializes the output values for routed nodes
  for (vehicle <- vrs.vehicles)
    updateValueOfVehicleNodesFromScratch(vehicle, vrs.routes.value())

  // Initializes the output values for unrouted nodes
  for (node <- vrs.nodes) {
    if (valueOfNodes(node) == defaultValueForUnroutedNodes)
      updateValueOfNode(node, defaultValueForUnroutedNodes)
  }

  /** Returns an optional explorer on the next node of the route to consider. The next node depends
    * on the direction of travel through the sequence.
    *
    * @param exp
    *   The explorer at the current node of the segment.
    * @param seg
    *   The current segment.
    * @return
    *   - `None` if the current node is the end of the segment.
    *   - `Some(explorer)` else.
    */
  protected def getNextPointOfSegmentToUpdate(
    exp: IntSequenceExplorer,
    seg: Segment
  ): Option[IntSequenceExplorer]

  /** Returns the first node of the segment to update (the first node depends on the direction of
    * travel through the sequence).
    */
  protected def firstPointToUpdateOnSegment(seg: Segment): Int

  /** Returns the last node of the segment to update (the first node depends on the direction of
    * travel through the sequence).
    */
  protected def lastPointToUpdateOnSegment(seg: Segment): Int

  /** Returns the order in which the segments have to be processed (the order depends on the
    * direction of travel through the sequence).
    */
  protected def orderSegments(vehicleSegments: VehicleSegments): List[Segment]

  /** Method called by the framework to assign a value of type `U` to the output variable of the
    * invariant.
    *
    * @param node
    *   The node to focus on.
    * @param value
    *   The value to assign.
    */
  protected def assignNodeValue(node: Int, value: U): Unit

  override def notifySeqChanges(
    seqVariable: SeqVariable,
    contextualVarIndex: Int,
    changes: SeqUpdate
  ): Unit = {
    if (digestUpdates(changes)) {
      for (vehicle <- changedVehiclesSinceLastNotified)
        updateValuesOfVehicleNodes(vehicle, seqVariable.pendingValue)
    } else {
      for (vehicle <- vrs.vehicles)
        updateValueOfVehicleNodesFromScratch(vehicle, seqVariable.pendingValue)
    }
  }

  override def checkInternals(): Unit = {
    // Save the values computed incrementally
    val incremental = valueOfNodes.clone()

    // Recompute all the values from scratch
    for (vehicle <- vrs.vehicles)
      updateValueOfVehicleNodesFromScratch(vehicle, vrs.routes.pendingValue)

    for (node <- vrs.nodes) {
      require(
        valueOfNodes(node) == incremental(node),
        s"""Constraint ${this.getClass.getName} failed for node $node.
           |From scratch: ${valueOfNodes(node)}
           |Incremental: ${incremental(node)}
           |Sequence: ${vrs.routes.pendingValue}
           |""".stripMargin
      )
    }
  }

  /** Updates the internal value of the given node and update its associated output variable. */
  private[this] def updateValueOfNode(node: Int, value: U): Unit = {
    valueOfNodes(node) = value
    assignNodeValue(node: Int, value: U)
  }

  /** Updates the values of a given segment. The method starts at the beginning of the segment. <br>
    *
    * Two cases can lead to the end of the method. First, it reaches the end of the second. Second,
    * the value of a node do not change. In this case, the remaining nodes of the segment do not
    * have to be updated.
    *
    * @param exp
    *   The optional explorer on the current node to explore.
    * @param seg
    *   The segment to explore.
    * @param prevNode
    *   The id of the previous explored node.
    * @param forceRecompute
    *   A flag to force the update of the entire segment.
    * @return
    *   The last node of the segment if the end has been reached. [[scala.None]] instead.
    */
  @tailrec
  private[this] def updateSegmentValues(
    exp: Option[IntSequenceExplorer],
    seg: Segment,
    prevNode: Int,
    forceRecompute: Boolean = false
  ): Option[Int] = {
    exp match {
      case None => Some(prevNode)
      case Some(e) =>
        val currentNode    = e.value
        val newValueOfNode = fun(prevNode, currentNode, valueOfNodes(prevNode))
        if (forceRecompute || newValueOfNode != valueOfNodes(currentNode)) {
          updateValueOfNode(currentNode, newValueOfNode)
          updateSegmentValues(
            getNextPointOfSegmentToUpdate(e, seg),
            seg,
            currentNode,
            forceRecompute
          )
        } else None
    }
  }

  /** Recursively updates the values of a list of segments. The segments are updated following the
    * order given by the list.
    *
    * @param segments
    *   The list of segments to update.
    * @param prevValue
    *   The value of the node right before the first segment to update.
    * @param prevNode
    *   The node right before the firs segment to update.
    * @param route
    *   The current routes of vehicles.
    * @return
    *   The last updated node.
    */
  @tailrec
  private[this] def updateSegmentListValue(
    segments: List[Segment],
    prevValue: U,
    prevNode: Int,
    route: IntSequence
  ): Int = {
    if (segments.nonEmpty) {
      val currentSegment = segments.head
      currentSegment match {
        case NewNode(node) =>
          updateValueOfNode(node, fun(prevNode, node, prevValue))
          updateSegmentListValue(segments.tail, valueOfNodes(node), node, route)

        case _: PrecomputedSubSequence =>
          val firstNode: Int             = firstPointToUpdateOnSegment(currentSegment)
          val currentValueOfFirstNode: U = valueOfNodes(firstNode)
          val newValueOfFirstNode: U     = fun(prevNode, firstNode, prevValue)

          // If the new value is the same that the current, we don't need to update the segment.
          // We can keep previous precomputation.
          if (currentValueOfFirstNode != newValueOfFirstNode) {
            updateValueOfNode(firstNode, newValueOfFirstNode)
            updateSegmentValues(
              getNextPointOfSegmentToUpdate(
                route.explorerAtFirstOccurrence(firstNode).get,
                currentSegment
              ),
              currentSegment,
              firstNode
            )
          }
          val lastNode: Int = lastPointToUpdateOnSegment(currentSegment)
          val lastValue: U  = valueOfNodes(lastNode)
          updateSegmentListValue(segments.tail, lastValue, lastNode, route)

        case _: FlippedPreComputedSubSequence =>
          val firstNode: Int     = firstPointToUpdateOnSegment(currentSegment)
          val newValueOfFirst: U = fun(prevNode, firstNode, prevValue)
          updateValueOfNode(firstNode, newValueOfFirst)
          // When a segment is flipped, its precomputation are no more valid so we need to update
          // all its values.
          updateSegmentValues(
            getNextPointOfSegmentToUpdate(
              route.explorerAtFirstOccurrence(firstNode).get,
              currentSegment
            ),
            currentSegment,
            firstNode,
            forceRecompute = true
          )

          val lastNode: Int = lastPointToUpdateOnSegment(currentSegment)
          val lastValue: U  = valueOfNodes(lastNode)
          updateSegmentListValue(segments.tail, lastValue, lastNode, route)
      }
    } else prevNode
  }

  /** Incrementally updates the values of the nodes for a given vehicle.
    *
    * @param vehicle
    *   The vehicle to update.
    * @param route
    *   The current routes of vehicles.
    */
  private[this] def updateValuesOfVehicleNodes(vehicle: Int, route: IntSequence): Unit = {
    val segList: List[Segment] = orderSegments(segmentsOfVehicle(vehicle))
    val lastNode: Int =
      updateSegmentListValue(segList, initValuePerVehicle(vehicle), vehicle, route)
    if (lastNode != vehicle)
      updateValueOfNode(vehicle, fun(lastNode, vehicle, valueOfNodes(lastNode)))
    else // The vehicle route is now empty. We assign its initial value.
      updateValueOfNode(vehicle, initValuePerVehicle(vehicle))
  }

  /** Creates a segment associated to the given vehicle's route. The vehicle node is not included in
    * the segment.
    */
  private[this] def initSegmentsOfVehicle(vehicle: Int, routes: IntSequence): Unit = {
    val posOfVehicle = vehicleSearcher.startPosOfVehicle(vehicle)

    val (lastNodeOfVehicle, lastNodePos): (Int, Int) = {
      if (vehicle < vrs.v - 1) {
        val lastPos: Int  = vehicleSearcher.startPosOfVehicle(vehicle + 1) - 1
        val lastNode: Int = routes.valueAtPosition(lastPos).get
        (lastNode, lastPos)
      } else {
        val lastNode: Int = routes.valueAtPosition(routes.size - 1).get
        (lastNode, routes.size - 1)
      }
    }

    val firstNodePos: Int = posOfVehicle + 1

    val segList: List[Segment] = {
      if (firstNodePos <= lastNodePos) {
        val firstNodeOfVehicle: Int = routes.valueAtPosition(firstNodePos).get
        List(
          PrecomputedSubSequence(firstNodeOfVehicle, lastNodeOfVehicle, lastNodePos - posOfVehicle)
        )
      } else List.empty
    }

    segmentsOfVehicle(vehicle) = VehicleSegments(segList, vrs.v)
  }

  /** Initialize the segments for each vehicle and compute from scratch the value of each node.
    *
    * @param vehicle
    *   The vehicle to update.
    * @param routes
    *   The current routes of vehicles.
    */
  private[this] def updateValueOfVehicleNodesFromScratch(
    vehicle: Int,
    routes: IntSequence
  ): Unit = {
    initSegmentsOfVehicle(vehicle, routes)
    updateValueOfNode(vehicle, initValuePerVehicle(vehicle))

    if (segmentsOfVehicle(vehicle).segments.nonEmpty) {
      val seg: Segment = segmentsOfVehicle(vehicle).segments.head
      val exp: Option[IntSequenceExplorer] =
        routes.explorerAtFirstOccurrence(firstPointToUpdateOnSegment(seg))

      val lastNode = updateSegmentValues(exp, seg, vehicle, forceRecompute = true)
      if (lastNode.isDefined)
        updateValueOfNode(vehicle, fun(lastNode.get, vehicle, valueOfNodes(lastNode.get)))
    }
  }

  private[this] def digestUpdates(changes: SeqUpdate): Boolean = {
    changes match {
      case SeqUpdateAssign(newSeq: IntSequence) =>
        vehicleSearcher = StackedVehicleSearcher(newSeq, vrs.v)
        false

      case SeqUpdateLastNotified(sequence: IntSequence) =>
        for (vehicle <- changedVehiclesSinceLastNotified) initSegmentsOfVehicle(vehicle, sequence)
        changedVehiclesSinceLastNotified = HashSet.empty
        true

      case SeqUpdateDefineCheckpoint(prev: SeqUpdate, _) =>
        val digested: Boolean = digestUpdates(prev)
        vehicleSearcher = vehicleSearcher.defineCheckpoint()
        digested

      case rollBackUpdate: SeqUpdateRollBackToTopCheckpoint =>
        digestUpdates(rollBackUpdate.howToRollBack)
        vehicleSearcher = vehicleSearcher.rollbackToTopCheckpoint()
        true // No assign can be performed when a checkpoint is defined. So a rollback is always digested

      case releaseUpdate: SeqUpdateReleaseTopCheckpoint =>
        val digested: Boolean = digestUpdates(releaseUpdate.prev)
        vehicleSearcher = vehicleSearcher.releaseTopCheckpoint()
        digested

      case insertUpdate @ SeqUpdateInsert(
            toInsert: Int,
            insertAfterPosExp: IntSequenceExplorer,
            prev: SeqUpdate
          ) =>
        val digested: Boolean = digestUpdates(prev)

        if (digested) {
          // If an assign was performed, we can avoid the following updates.
          // All the values will be computed from scratch.
          val impactedVehicle: Int =
            vehicleSearcher.vehicleReachingPosition(insertAfterPosExp.position)
          val startPosOfImpactedVehicle: Int    = vehicleSearcher.startPosOfVehicle(impactedVehicle)
          val impactedSegments: VehicleSegments = segmentsOfVehicle(impactedVehicle)

          changedVehiclesSinceLastNotified += impactedVehicle
          segmentsOfVehicle(impactedVehicle) =
            impactedSegments.insertNode(toInsert, insertAfterPosExp, startPosOfImpactedVehicle + 1)
        }
        vehicleSearcher = vehicleSearcher.push(insertUpdate.oldPosToNewPos)

        digested

      case removeUpdate @ SeqUpdateRemove(toRemoveExp: IntSequenceExplorer, prev: SeqUpdate) =>
        val digested: Boolean = digestUpdates(prev)

        if (digested) {
          // If an assign was performed, we can avoid the following updates.
          // All the values will be computed from scratch or a new checkpoint will be defined.

          val impactedVehicle: Int = vehicleSearcher.vehicleReachingPosition(toRemoveExp.position)
          val startPosOfImpactedVehicle: Int    = vehicleSearcher.startPosOfVehicle(impactedVehicle)
          val impactedSegments: VehicleSegments = segmentsOfVehicle(impactedVehicle)

          updateValueOfNode(toRemoveExp.value, defaultValueForUnroutedNodes)
          changedVehiclesSinceLastNotified += impactedVehicle
          segmentsOfVehicle(impactedVehicle) = impactedSegments
            .removeNode(toRemoveExp, startPosOfImpactedVehicle + 1)
            ._1
        }
        vehicleSearcher = vehicleSearcher.push(removeUpdate.oldPosToNewPos)

        digested

      case moveUpdate @ SeqUpdateMove(
            fromExp: IntSequenceExplorer,
            toExp: IntSequenceExplorer,
            afterPosExp: IntSequenceExplorer,
            flip: Boolean,
            prev: SeqUpdate
          ) =>
        val digested = digestUpdates(prev)

        if (digested) {
          // If an assign was performed, we can avoid the following updates.
          // All the values will be computed from scratch or a new checkpoint will be defined.
          val fromImpactedVehicle = vehicleSearcher.vehicleReachingPosition(fromExp.position)
          val startOfFromVehicle  = vehicleSearcher.startPosOfVehicle(fromImpactedVehicle)
          val fromSegments        = segmentsOfVehicle(fromImpactedVehicle)
          val toImpactedVehicle   = vehicleSearcher.vehicleReachingPosition(afterPosExp.position)
          val startOfToVehicle    = vehicleSearcher.startPosOfVehicle(toImpactedVehicle)
          val toSegments          = segmentsOfVehicle(toImpactedVehicle)

          val (updatedFrom, updatedTo) = VehicleSegments.moveSegments(
            startOfFromVehicle + 1,
            startOfToVehicle + 1,
            fromSegments,
            toSegments,
            fromExp,
            toExp,
            afterPosExp,
            flip
          )

          segmentsOfVehicle(toImpactedVehicle) = updatedTo
          changedVehiclesSinceLastNotified += toImpactedVehicle
          if (fromImpactedVehicle != toImpactedVehicle) {
            segmentsOfVehicle(fromImpactedVehicle) = updatedFrom
            changedVehiclesSinceLastNotified += fromImpactedVehicle
          }
        }
        vehicleSearcher = vehicleSearcher.push(moveUpdate.oldPosToNewPos)

        digested

      case x: SeqUpdate => throw new IllegalArgumentException(s"Unexpected update $x")
    }
  }

}
