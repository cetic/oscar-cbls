package oscar.cbls.business.routing.invariants.timeWindow

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.invariants.group._
import oscar.cbls._
import oscar.cbls.algo.quick.QList

/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/

object TimeWindowConstraint {

  /**
    * This method instantiate a TimeWindow constraint given the following input
    * @param gc The GlobalConstraint to which this invariant is linked
    * @param n The number of nodes of the problem (including vehicle)
    * @param v The number of vehicles of the problem
    * @param earliestArrivalTime An array representing the earliest arrival time at a node (or vehicle's depot)
    * @param latestLeavingTime An array representing the latest leave time at a node (or vehicle's depot)
    * @param travelTimeMatrix A matrix representing the different travel time between the nodes
    * @param violations An array of CBLSIntVar maintaining the violation of each vehicle
    * @return a time window constraint
    */
  def apply(gc: GlobalConstraintCore,
            n: Long,
            v: Long,
            earliestArrivalTime: Array[Long],
            latestLeavingTime: Array[Long],
            travelTimeMatrix: Array[Array[Long]],
            violations: Array[CBLSIntVar]): TimeWindowConstraint ={

    new TimeWindowConstraint(gc, n, v,
      earliestArrivalTime,
      latestLeavingTime,
      earliestArrivalTime,
      latestLeavingTime,
      travelTimeMatrix, violations)
  }

  //TODO: this is not defensive and not useful to have two aply with different parameters. add taskDuration in both.
  // and generate the violation array in the apply, because it is boring to do.

  /**
    * This method instantiate a TimeWindow constraint given the following input.
    * @param gc The GlobalConstraint to which this invariant is linked
    * @param n The number of nodes of the problem (including vehicle)
    * @param v The number of vehicles of the problem
    * @param earliestArrivalTime An array (size n) representing the earliest arrival time at a node (or vehicle's depot)
    * @param latestLeavingTime An array (size n) representing the latest leaving time at a node (or vehicle's depot)
    * @param taskDurations An array (size n) representing the task duration at a node (or vehicle's depot)
    * @param travelTimeMatrix A matrix representing the different travel time between the nodes
    * @param violations An array of CBLSIntVar maintaining the violation of each vehicle
    * @return a time window constraint
    */
  def apply(gc: GlobalConstraintCore,
            n: Long,
            v: Long,
            earliestArrivalTime: Array[Long],
            latestLeavingTime: Array[Long],
            taskDurations: Array[Long],
            travelTimeMatrix: Array[Array[Long]],
            violations: Array[CBLSIntVar]): TimeWindowConstraint ={

    new TimeWindowConstraint(gc, n, v,
      earliestArrivalTime,
      (latestLeavingTime, taskDurations).zipped.map(_ - _),
      (earliestArrivalTime, taskDurations).zipped.map(_ + _),
      latestLeavingTime,
      travelTimeMatrix, violations)
  }
}

/**
  * This class represent a time window constraint.
  * Given parameters, it maintains the violation value of each vehicle (in violations var)
  * @param gc The GlobalConstraint to which this invariant is linked
  * @param n The number of nodes of the problem (including vehicle)
  * @param v The number of vehicles of the problem
  * @param earliestArrivalTime An array (size n) representing the earliest arrival time at a node (or vehicle's depot)
  * @param latestArrivalTime An array (size n) representing the latest arrival time at a node (or vehicle's depot)
  * @param earliestLeavingTime An array (size n) representing the earliest leaving time at a node (or vehicle's depot)
  * @param latestLeavingTime An array (size n) representing the latest leaving time at a node (or vehicle's depot)
  * @param travelTimeMatrix A matrix representing the different travel time between the nodes
  * @param violations An array of CBLSIntVar maintaining the violation of each vehicle
  */
class TimeWindowConstraint (gc: GlobalConstraintCore,
                            n: Long,
                            v: Long,
                            earliestArrivalTime: Array[Long],
                            latestArrivalTime: Array[Long],
                            earliestLeavingTime: Array[Long],
                            latestLeavingTime: Array[Long],
                            travelTimeMatrix: Array[Array[Long]],
                            val violations: Array[CBLSIntVar]
                           ) extends GlobalConstraintDefinition(gc,v) {

  type U = Boolean

  val preComputedValues: Array[Array[TransferFunction]] = Array.fill(n)(Array.fill(n)(EmptyTransferFunction))

  // Initialize the vehicles value, the precomputation value and link these invariant to the GlobalConstraintCore
  gc.register(this)
  vehiclesValueAtCheckpoint0 = Array.fill(v)(false)
  currentVehiclesValue = Array.fill(v)(false)
  for(outputVariable <- violations)outputVariable.setDefiningInvariant(gc)

  private val transferFunctionOfNode: Array[TransferFunction] = Array.tabulate(n)(
    node =>
      DefinedTransferFunction(
        earliestArrivalTime(node),
        latestArrivalTime(node),
        earliestLeavingTime(node),node,node))

  /**
    * This method makes the composition of two TransferFunction
    * @param f1 The first TransferFunction
    * @param f2 The second TransferFunction
    * @param m The distance between the two TransferFunction
    * @return The composed TransferFunction or an EmptyTransferFunction
    */
  private def composeFunction (f1: TransferFunction, f2: TransferFunction, m: Long): TransferFunction ={
    if(f1.isEmpty)
      return f1
    else if(f2.isEmpty)
      return f2

    val earliestArrivalTimeAt2 = f1.el + m
    val latestArrivalTimeAt2 =
      if(f1.la + f1.el - f1.ea + m < 0L)
        Long.MaxValue
      else f1.la + f1.el - f1.ea + m

    val earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 = earliestArrivalTimeAt2 <= f2.ea
    val earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2 = earliestArrivalTimeAt2 <= f2.la
    val latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 = latestArrivalTimeAt2 <= f2.ea
    val latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2 = latestArrivalTimeAt2 <= f2.la

    // A simple pattern matching would be much more concise unfortunately,
    // pattern matching with a tuple of size > 2 introduces boxing which has
    // a unwanted impact on performance
    var ea3: Long = 0
    var ll3: Long = 0
    var el3: Long = 0
      if(earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 &&
        earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2 &&
        latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 &&
        latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2) {
        ea3 = f1.la
        ll3 = f1.la
        el3 = f2.el
      }

      else if(earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 &&
        earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2 &&
        !latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 &&
        latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2) {
        ea3 = f2.ea - f1.el - m + f1.ea
        ll3 = f1.la
        el3 = f2.el
      }
      else if(earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 &&
        earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2 &&
        !latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 &&
        !latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2) {
        ea3 = f2.ea - f1.el - m + f1.ea
        ll3 = f2.la - f1.el - m + f1.ea
        el3 = f2.el
      }
      else if(!earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 &&
        earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2 &&
        !latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 &&
        latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2) {
        ea3 = f1.ea
        ll3 = f1.la
        el3 = f1.el + f2.el - f2.ea + m
      }
      else if(!earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 &&
        earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2 &&
        !latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 &&
        !latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2) {
        ea3 = f1.ea
        ll3 = f2.la - f1.el - m + f1.ea
        el3 = f1.el + f2.el - f2.ea + m
      }
      else if(!earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 &&
        !earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2 &&
        !latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 &&
        !latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2) {
        ea3 = 1L
        ll3 = -1L
        el3 = -1L
      }

      else
        throw new Error("Unhandled case : (" + earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 + ", " +
          earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2 + ", " +
          latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 + ", " +
          latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2 + ")")

    if(ea3 > ll3)
      EmptyTransferFunction
    else
      DefinedTransferFunction(ea3, ll3, el3, f1.from, f2.to)
  }

  /**
    * Return relevant information concercing the given segment.
    * In order to avoid boxing issues, the start and end node a grouped into their own tuple.
    * This way, we avoid to have a tuple of size 3.
    */
  private def segmentsTransferFunction(segment: Segment): TransferFunction ={
    segment match{
      case seg: PreComputedSubSequence => preComputedValues(seg.startNode)(seg.endNode)
      case seg: FlippedPreComputedSubSequence => preComputedValues(seg.startNode)(seg.endNode)
      case seg: NewNode => transferFunctionOfNode(seg.node)
    }
  }

  /**
    * This method is called by the framework when a pre-computation must be performed.
    * you are expected to assign a value of type T to each node of the vehicle "vehicle" through the method "setNodeValue"
    *
    * @param vehicle         the vehicle where pre-computation must be performed
    * @param routes          the sequence representing the route of all vehicle
    *                        BEWARE,other vehicles are also present in this sequence; you must only work on the given vehicle
    */
  override def performPreCompute(vehicle: Long, routes: IntSequence): Unit = {

    def performPreComputeForNode(node: Long, prevNode: Long, route: QList[Long], lastTF: TransferFunction): Unit ={
      if(route != null) {
        val curNode = route.head.toInt
        val newTF = if (lastTF.isEmpty) lastTF else composeFunction(lastTF, transferFunctionOfNode(curNode), travelTimeMatrix(prevNode)(curNode))
        preComputedValues(node)(curNode) = newTF
        performPreComputeForNode(node, curNode, route.tail, newTF)
      }
    }

    def performPreComputeOnRoute(route: QList[Long]): Unit ={
      val node = route.head
      if(preComputedValues(node) == null)preComputedValues(node) = Array.fill(n)(EmptyTransferFunction)
      preComputedValues(node)(node) = transferFunctionOfNode(node)
      val lastTF = preComputedValues(node)(node)
      val prevNode = node
      performPreComputeForNode(node, prevNode, route.tail, lastTF)
      if(route.tail != null)
        performPreComputeOnRoute(route.tail)
    }

    var continue = true
    var vExplorer = routes.explorerAtAnyOccurrence(vehicle).get.next
    var route: QList[Long] = QList(vehicle)
    while(continue){
      vExplorer match {
        case None => continue = false
        case Some(elem) =>
          if (elem.value < v && elem.value != vehicle){
            continue = false
          } else {
            route = QList(elem.value, route)
          }
          vExplorer = elem.next
      }
    }
    performPreComputeOnRoute(route)
    performPreComputeOnRoute(route.reverse)
  }

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle         the vehicle that we are focusing on
    * @param segments        the segments that constitute the route.
    *                        The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @param routes          the sequence representing the route of all vehicle
    * @return the value associated with the vehicle
    */
  override def computeVehicleValue(vehicle: Long, segments: QList[Segment], routes: IntSequence): Unit = {
    /**
      * @param segments The list of segment
      * @param prevLeavingTime The leave time at previous segment (0L if first one)
      * @return The leave time after going through all the segments
      */
    def arrivalAtDepot(segments: QList[Segment], previousSegmentEnd: Long = vehicle, prevLeavingTime: Long = 0L): Long ={
      val (segment, tail) = (segments.head, segments.tail)
      val transferFunction = segmentsTransferFunction(segment)
      val arrivalTimeAtSegment = prevLeavingTime + travelTimeMatrix(previousSegmentEnd)(segment.startNode())
      val leaveTimeAtSegment = transferFunction(arrivalTimeAtSegment)
      if(leaveTimeAtSegment >= 0L) {
        if (tail != null)
          arrivalAtDepot(tail, segment.endNode(), leaveTimeAtSegment)
        else
          leaveTimeAtSegment + travelTimeMatrix(segment.endNode())(vehicle)
      }
      else leaveTimeAtSegment
    }
    val arrivalTimeAtDepot = arrivalAtDepot(segments)
    saveVehicleValue(vehicle, arrivalTimeAtDepot < 0L || arrivalTimeAtDepot > latestLeavingTime(vehicle))
  }

  /**
    * the framework calls this method to assign the value U to he output variable of your invariant.
    * It has been dissociated from the method above because the framework memorizes the output value of the vehicle,
    * and is able to restore old value without the need to re-compute them, so it only will call this assignVehicleValue method
    *
    * @param vehicle the vehicle number
    */
  override def assignVehicleValue(vehicle: Long): Unit = {
    if(currentVehiclesValue(vehicle)) violations(vehicle) := 1L else violations(vehicle) := 0L
  }

  /**
    * This method is defined for verification purpose.
    * It computes the value of the vehicle from scratch.
    *
    * @param vehicle the vehicle on which the value is computed
    * @param routes  the sequence representing the route of all vehicle
    * @return the value of the constraint for the given vehicle
    */
  override def computeVehicleValueFromScratch(vehicle: Long, routes: IntSequence, save: Boolean = true): Boolean = {
    var arrivalTimeAtFromNode = earliestArrivalTime(vehicle)
    var leaveTimeAtFromNode = earliestLeavingTime(vehicle)
    var fromNode = vehicle
    val explorerAtVehicleStart = routes.explorerAtAnyOccurrence(vehicle).head
    var explorerAtCurrentNode = explorerAtVehicleStart.next
    var violationFound = false

    while(explorerAtCurrentNode.isDefined && explorerAtCurrentNode.get.value >= v && !violationFound){
      val toNode = explorerAtCurrentNode.get.value
      val travelDuration = travelTimeMatrix(fromNode)(toNode)
      val arrivalTimeAtToNode = leaveTimeAtFromNode + travelDuration
      val leaveTimeAtToNode = Math.max(earliestArrivalTime(toNode), arrivalTimeAtToNode) + earliestLeavingTime(toNode) - earliestArrivalTime(toNode)

      // Check violation
      if(leaveTimeAtToNode > latestLeavingTime(toNode))
        violationFound = true

      // Update values
      fromNode = toNode
      explorerAtCurrentNode = explorerAtCurrentNode.get.next
      arrivalTimeAtFromNode = arrivalTimeAtToNode
      leaveTimeAtFromNode = leaveTimeAtToNode
    }

    // Check travel back to depot
    val travelBackToDepot = travelTimeMatrix(fromNode)(vehicle)
    val arrivalTimeAtDepot = leaveTimeAtFromNode + travelBackToDepot
    val result = violationFound || arrivalTimeAtDepot > latestLeavingTime(vehicle)
    if(save) saveVehicleValue(vehicle, result)
    result
  }

  /**
    * Return all transfer functions of the problem.
    * Meant for post-optimisation. Don't use it during optimisation.
    * @return A 2L-dimension table of TransferFunction
    */
  def transferFunctions(routes: IntSequence): Array[Array[TransferFunction]] ={
    for(curV <- 0L to v){
      performPreCompute(curV, routes)
    }

    preComputedValues
  }
}
