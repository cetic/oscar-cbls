package oscar.cbls.business.routing.invariants

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.invariants.group._
import oscar.cbls.core.computation._

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
    * @param routes The route of the problem (ChangingSeqValue created in the VRP class)
    * @param n The number of nodes of the problem (including vehicle)
    * @param v The number of vehicles of the problem
    * @param earlylines An array representing the earliest leave time at a node (or vehicle's depot)
    * @param deadlines An array representing the latest arrival time at a node (or vehicle's depot)
    * @param travelTimeMatrix A matrix representing the different travel time between the nodes
    * @param violations An array representing the eventual violation of the vehicle
    * @return a time window constraint
    */
  def apply(routes: ChangingSeqValue,
            n: Int,
            v: Int,
            earlylines: Array[Int],
            deadlines: Array[Int],
            travelTimeMatrix: Array[Array[Int]],
            violations: Array[CBLSIntVar]): TimeWindowConstraint ={

    new TimeWindowConstraint(routes: ChangingSeqValue, n, v,
      earlylines,
      deadlines,
      earlylines,
      deadlines,
      travelTimeMatrix, violations)
  }

  /**
    * This method instantiate a TimeWindow constraint given the following input.
    * @param routes The route of the problem (ChangingSeqValue created in the VRP class)
    * @param n The number of nodes of the problem (including vehicle)
    * @param v The number of vehicles of the problem
    * @param earlylines An array (size n) representing the earliest leave time at a node (or vehicle's depot)
    * @param deadlines An array (size n) representing the latest arrival time at a node (or vehicle's depot)
    * @param taskDurations An array (size n) representing the task duration at a node (or vehicle's depot)
    * @param travelTimeMatrix A matrix representing the different travel time between the nodes
    * @param violations An array representing the eventual violation of the vehicle
    * @return a time window constraint
    */
  def apply(routes: ChangingSeqValue,
            n: Int,
            v: Int,
            earlylines: Array[Int],
            deadlines: Array[Int],
            taskDurations: Array[Int],
            travelTimeMatrix: Array[Array[Int]],
            violations: Array[CBLSIntVar]): TimeWindowConstraint ={

    new TimeWindowConstraint(routes: ChangingSeqValue, n, v,
      earlylines,
      deadlines,
      (earlylines, taskDurations).zipped.map(_ + _),
      (deadlines, taskDurations).zipped.map(_ + _),
      travelTimeMatrix, violations)
  }
}

class TimeWindowConstraint (routes: ChangingSeqValue,
                            n: Int,
                            v: Int,
                            earlylines: Array[Int],
                            deadlines: Array[Int],
                            earliestLeaveTime: Array[Int],
                            latestLeaveTime: Array[Int],
                            travelTimeMatrix: Array[Array[Int]],
                            violations: Array[CBLSIntVar]
                           ) extends GlobalConstraintDefinition[Array[TransfertFunction], Boolean](routes, v) with SeqNotificationTarget {

  private val transfertFunctionOfNode: Array[TransfertFunction] = Array.tabulate(n)(
    node =>
      DefinedTransfertFunction(
        earlylines(node),
        deadlines(node),
        earliestLeaveTime(node)))

  private def composeFunction (f1: TransfertFunction, f2: TransfertFunction, m: Int): TransfertFunction ={
    if(f1.isEmpty)
      return f1
    else if(f2.isEmpty)
      return f2

    val earliestArrivalTimeAt2 = f1.l + m
    val latestArrivalTimeAt2 = f1.d + f1.l - f1.e + m

    val earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 = earliestArrivalTimeAt2 <= f2.e
    val earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2 = earliestArrivalTimeAt2 <= f2.d
    val latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 = latestArrivalTimeAt2 <= f2.e
    val latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2 = latestArrivalTimeAt2 <= f2.d

    val (e3, d3, l3) =
      (earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
        earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2,
        latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
        latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2) match{
        case (true,true,true,true) =>
          (f1.d, f1.d, f2.l)                                    // e3 == d1 because latest arrival time at 2 is lower than earliest starting time at 2
                                                                // so it doesn't matter when you arrive at 1 the resulting leaving time at 2 will be l2
                                                                // => e3 == d1 (the formula says if (t <= e) => l
        case (true,true,false,true) =>
          (f2.e - f1.l - m + f1.e, f1.d, f2.l)
        case (false,true,false,true) =>
          (f1.e, f1.d,f1.l + f2.l - f2.e + m)
        case (true,true,false,false) =>
          (f2.e - f1.l - m + f1.e, f2.d - f1.l - m + f1.e, f2.l)
        case (false,true,false,false) =>
          (f1.e, f2.d - f1.l - m + f1.e, f1.l + f2.l - f2.e + m)
        case (false,false,false,false) =>
          (1, -1, -1)
        case _ =>
          throw new Error("Unhandled case : " + (earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
            earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2,
            latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
            latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2))
      }

    if(e3 > d3)
      EmptyTransfertFunction
    else
      DefinedTransfertFunction(e3, d3, l3)
  }

  private def segmentsInfo(segment: Segment[Array[TransfertFunction]]): (Int, Int, TransfertFunction) ={
    segment match{
      case seg: PreComputedSubSequence[Array[TransfertFunction]] =>
        (seg.startNode, seg.endNode, seg.startNodeValue(seg.endNode))
      case seg: FlippedPreComputedSubSequence[Array[TransfertFunction]] => (seg.startNode, seg.endNode, seg.startNodeValue(seg.endNode))
      case seg: NewNode[Array[TransfertFunction]] => (seg.node, seg.node, transfertFunctionOfNode(seg.node))
    }
  }

  /**
    * tis method is called by the framework when a pre-computation must be performed.
    * you are expected to assign a value of type T to each node of the vehicle "vehicle" through the method "setNodeValue"
    *
    * @param vehicle         the vehicle where pre-computation must be performed
    * @param routes          the sequence representing the route of all vehicle
    *                        BEWARE,other vehicles are also present in this sequence; you must only work on the given vehicle
    * @param preComputedVals The array of precomputed values
    */
  override def performPreCompute(vehicle: Int, routes: IntSequence, preComputedVals: Array[Array[TransfertFunction]]): Unit = {
    def updatePrevNodes(newNode: Int, prevNodes: List[Int]): Unit ={
      //prevNodes is reversed
      val lastNode = prevNodes.head
      for(node <- prevNodes){
        val fromTF = preComputedVals(node)(lastNode)
        preComputedVals(node)(newNode) = composeFunction(
          fromTF,
          transfertFunctionOfNode(newNode),
          travelTimeMatrix(lastNode)(newNode))
      }
    }

    var continue = true
    var vExplorer = routes.explorerAtAnyOccurrence(vehicle)
    var exploredNodes: List[Int] = List()
    while(continue){
      vExplorer match {
        case None => continue = false
        case Some(elem) =>
          if (elem.value < v && elem.value != vehicle){
            continue = false
          } else {
            if(exploredNodes.nonEmpty)
              updatePrevNodes(elem.value, exploredNodes)
            if(preComputedVals(elem.value) == null)preComputedVals(elem.value) = Array.fill(n)(EmptyTransfertFunction)
            preComputedVals(elem.value)(elem.value) = transfertFunctionOfNode(elem.value)
            exploredNodes = elem.value :: exploredNodes
          }
          vExplorer = elem.next
      }
    }
  }

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle         the vehicle that we are focusing on
    * @param segments        the segments that constitute the route.
    *                        The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @param routes          the sequence representing the route of all vehicle
    * @param preComputedVals The array of precomputed values
    * @return the value associated with the vehicle
    */
  override def computeVehicleValue(vehicle: Int, segments: List[Segment[Array[TransfertFunction]]], routes: IntSequence, preComputedVals: Array[Array[TransfertFunction]]): Boolean = {
    def concatSegments(segments: List[Segment[Array[TransfertFunction]]], concatenateFunction: TransfertFunction, prevSegment: Segment[Array[TransfertFunction]]): TransfertFunction ={
      if(segments.isEmpty)
        return concatenateFunction

      val lastSegmentInfo = segmentsInfo(prevSegment)
      val currentSegmentInfo = segmentsInfo(segments.head)
      val travelDurationBetweenSegments = travelTimeMatrix(lastSegmentInfo._2)(currentSegmentInfo._1)
      val newComposedFunction = composeFunction(concatenateFunction, currentSegmentInfo._3, travelDurationBetweenSegments)

      if(newComposedFunction.isEmpty)
        return newComposedFunction

      concatSegments(segments.tail, newComposedFunction, segments.head)
    }

    val firstSegmentInfo = segmentsInfo(segments.head)
    val transfertFunctionOfConcatenatedSegments =
      concatSegments(segments.tail :+ NewNode[Array[TransfertFunction]](vehicle), firstSegmentInfo._3, segments.head)

    transfertFunctionOfConcatenatedSegments.isEmpty
  }

  /**
    * the framework calls this method to assign the value U to he output variable of your invariant.
    * It has been dissociated from the method above because the framework memorizes the output value of the vehicle,
    * and is able to restore old value without the need to re-compute them, so it only will call this assignVehicleValue method
    *
    * @param vehicle the vehicle number
    * @param value   the value of the vehicle
    */
  override def assignVehicleValue(vehicle: Int, value: Boolean): Unit = {
    if(value) violations(vehicle) := 1 else violations(vehicle) := 0
  }

  /**
    * this method is defined for verification purpose. It computes the value of the vehicle from scratch.
    *
    * @param vehicle the vehicle on which the value is computed
    * @param routes  the sequence representing the route of all vehicle
    * @return the value of the constraint for the given vehicle
    */
  override def computeVehicleValueFromScratch(vehicle: Int, routes: IntSequence): Boolean = {
    var arrivalTimeAtFromNode = earlylines(vehicle)
    var leaveTimeAtFromNode = earliestLeaveTime(vehicle)
    var fromNode = vehicle
    val explorerAtVehicleStart = routes.explorerAtAnyOccurrence(vehicle).head
    var explorerAtCurrentNode = explorerAtVehicleStart.next
    var violationFound = false

      while(explorerAtCurrentNode.isDefined && explorerAtCurrentNode.get.value >= v && !violationFound){
        val toNode = explorerAtCurrentNode.get.value
        val travelDuration = travelTimeMatrix(fromNode)(toNode)
        val arrivalTimeAtToNode = leaveTimeAtFromNode + travelDuration
        val leaveTimeAtToNode = Math.max(earlylines(toNode), arrivalTimeAtToNode) + earliestLeaveTime(toNode) - earlylines(toNode)

        // Check violation
        if(leaveTimeAtToNode > latestLeaveTime(toNode))
            violationFound = true

//        encounteredNodes = encounteredNodes :+ toNode
//        travelPerformed = travelPerformed :+ travelDuration

        // Update values
        fromNode = toNode
        explorerAtCurrentNode = explorerAtCurrentNode.get.next
        arrivalTimeAtFromNode = arrivalTimeAtToNode
        leaveTimeAtFromNode = leaveTimeAtToNode
    }

    // Check travel back to depot
    val travelBackToDepot = travelTimeMatrix(fromNode)(vehicle)
    val arrivalTimeAtDepot = leaveTimeAtFromNode + travelBackToDepot
    violationFound || arrivalTimeAtDepot >= latestLeaveTime(vehicle)
  }

  override def outputVariables: Iterable[Variable] = {
    violations
  }
}


abstract class TransfertFunction (val e: Int, val d: Int, val l: Int){

  def apply(t: Int): Option[Int]

  def isEmpty: Boolean

  override def toString: String = {
    "e : " + e + "\n d : " + d + "\n l : " + l
  }
}

case class DefinedTransfertFunction (override val e: Int, override val d: Int, override val l: Int) extends TransfertFunction(e,d,l){
  require(d >= e && l >= e, "e : " + e + ", d : " + d + ", l : " + l)
  override def apply(t: Int): Option[Int] = {
    if(t <= e)
      Some(l)
    else if(t <= d)
      Some(t + l - e)
    else
      None
  }

  override def isEmpty: Boolean = false
}

case object EmptyTransfertFunction extends TransfertFunction(1,-1,-1){
  override def apply(t: Int): Option[Int] = None

  override def isEmpty: Boolean = true
}