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
package oscar.cbls.business.routing.invariants.timeWindow

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.algo.quick.QList
import oscar.cbls.business.routing.invariants.global._
import oscar.cbls.core.computation.CBLSIntVar

import scala.annotation.tailrec

object TimeWindowConstraint {

  /**
    * This method instantiate a TimeWindow constraint given the following input
    * @param gc The GlobalConstraint to which this invariant is linked
    * @param n The number of nodes of the problem (including vehicle)
    * @param v The number of vehicles of the problem
    * @param singleNodeTransferFunctions An array containing the single TransferFunction of each node
    * @param travelTimeMatrix A matrix representing the different travel time between the nodes
    * @param violations An array of CBLSIntVar maintaining the violation of each vehicle
    * @return a time window constraint
    */
  def apply(gc: GlobalConstraintCore,
            n: Int,
            v: Int,
            singleNodeTransferFunctions: Array[TransferFunction],
            travelTimeMatrix: Array[Array[Long]],
            violations: Array[CBLSIntVar]): TimeWindowConstraint ={

    new TimeWindowConstraint(gc, n, v,
      singleNodeTransferFunctions,
      travelTimeMatrix, violations)
  }
}

/**
  * This class represent a time window constraint.
  * Given parameters, it maintains the violation value of each vehicle (in violations var)
  * @param gc The GlobalConstraint to which this invariant is linked
  * @param n The number of nodes of the problem (including vehicle)
  * @param v The number of vehicles of the problem
  * @param singleNodeTransferFunctions An array containing the single TransferFunction of each node
  * @param travelTimeMatrix A matrix representing the different travel time between the nodes
  * @param violations An array of CBLSIntVar maintaining the violation of each vehicle
  */
class TimeWindowConstraint (gc: GlobalConstraintCore,
                            n: Int,
                            v: Int,
                            singleNodeTransferFunctions: Array[TransferFunction],
                            travelTimeMatrix: Array[Array[Long]],
                            val violations: Array[CBLSIntVar]
                           ) extends GlobalConstraintDefinition[Boolean](gc,v) {

  val preComputedValues: Array[Array[TransferFunction]] = Array.fill(n)(Array.fill(n)(EmptyTransferFunction))

  // Initialize the vehicles value, the precomputation value and link these invariant to the GlobalConstraintCore
  gc.register(this)
  //vehiclesValueAtCheckpoint0.map(vValues => false)
  //currentVehiclesValue.map(vValues => false)
  for(outputVariable <- violations)outputVariable.setDefiningInvariant(gc)

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
        throw new Error(s"Unhandled case : ($earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2, $earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2, $latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2, $latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2)")

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
      case seg: NewNode => singleNodeTransferFunctions(seg.node)
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
  override def performPreCompute(vehicle: Int, routes: IntSequence): Unit = {

    @tailrec
    def performPreComputeForNode(node: Int, prevNode: Int, route: QList[Int], lastTF: TransferFunction): Unit ={
      if(route != null) {
        val curNode = route.head.toInt
        val newTF = if (lastTF.isEmpty) lastTF else composeFunction(lastTF, singleNodeTransferFunctions(curNode), travelTimeMatrix(prevNode)(curNode))
        preComputedValues(node)(curNode) = newTF
        performPreComputeForNode(node, curNode, route.tail, newTF)
      }
    }

    @tailrec
    def performPreComputeOnRoute(route: QList[Int]): Unit ={
      val node = route.head
      if(preComputedValues(node) == null)preComputedValues(node) = Array.fill(n)(EmptyTransferFunction)
      preComputedValues(node)(node) = singleNodeTransferFunctions(node)
      val lastTF = preComputedValues(node)(node)
      val prevNode = node
      performPreComputeForNode(node, prevNode, route.tail, lastTF)
      if(route.tail != null)
        performPreComputeOnRoute(route.tail)
    }

    var continue = true
    var vExplorer = routes.explorerAtAnyOccurrence(vehicle).get.next
    var route: QList[Int] = QList(vehicle)
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
  override def computeVehicleValue(vehicle: Int, segments: QList[Segment], routes: IntSequence): Boolean = {
    /**
      * @param segments The list of segment
      * @param prevLeavingTime The leave time at previous segment (0L if first one)
      * @return The leave time after going through all the segments
      */
    @tailrec
    def arrivalAtDepot(segments: QList[Segment], previousSegmentEnd: Int = vehicle, prevLeavingTime: Long = 0L): Long ={
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
    val latestLeavingTimeAtVehicle = singleNodeTransferFunctions(vehicle).la +
      singleNodeTransferFunctions(vehicle).el -
      singleNodeTransferFunctions(vehicle).ea
    arrivalTimeAtDepot < 0L || arrivalTimeAtDepot > latestLeavingTimeAtVehicle
  }

  /**
    * the framework calls this method to assign the value U to he output variable of your invariant.
    * It has been dissociated from the method above because the framework memorizes the output value of the vehicle,
    * and is able to restore old value without the need to re-compute them, so it only will call this assignVehicleValue method
    *
    * @param vehicle the vehicle number
    */
  override def assignVehicleValue(vehicle: Int, value: Boolean): Unit = {
    if(value) violations(vehicle) := 1L else violations(vehicle) := 0L
  }

  /**
    * This method is defined for verification purpose.
    * It computes the value of the vehicle from scratch.
    *
    * @param vehicle the vehicle on which the value is computed
    * @param routes  the sequence representing the route of all vehicle
    * @return the value of the constraint for the given vehicle
    */
  override def computeVehicleValueFromScratch(vehicle: Int, routes: IntSequence): Boolean = {
    var arrivalTimeAtFromNode = singleNodeTransferFunctions(vehicle).ea
    var leaveTimeAtFromNode = singleNodeTransferFunctions(vehicle).el
    var fromNode = vehicle
    val explorerAtVehicleStart = routes.explorerAtAnyOccurrence(vehicle).head
    var explorerAtCurrentNode = explorerAtVehicleStart.next
    var violationFound = false

    while(explorerAtCurrentNode.isDefined && explorerAtCurrentNode.get.value >= v && !violationFound){
      val toNode = explorerAtCurrentNode.get.value
      val travelDuration = travelTimeMatrix(fromNode)(toNode)
      val arrivalTimeAtToNode = leaveTimeAtFromNode + travelDuration
      val leaveTimeAtToNode = Math.max(singleNodeTransferFunctions(toNode).ea, arrivalTimeAtToNode) + singleNodeTransferFunctions(toNode).el - singleNodeTransferFunctions(toNode).ea

      // Check violation
      if(leaveTimeAtToNode > singleNodeTransferFunctions(toNode).latestLeavingTime)
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
    violationFound || arrivalTimeAtDepot > singleNodeTransferFunctions(vehicle).latestLeavingTime
  }

  /**
    * Return all transfer functions of the problem.
    * Meant for post-optimisation. Don't use it during optimisation.
    * @return A 2L-dimension table of TransferFunction
    */
  def transferFunctions(routes: IntSequence): Array[Array[TransferFunction]] ={
    for(curV <- 0 to v){
      performPreCompute(curV, routes)
    }

    preComputedValues
  }
}
