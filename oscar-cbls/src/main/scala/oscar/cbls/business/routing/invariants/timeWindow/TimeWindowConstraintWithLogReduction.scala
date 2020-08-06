package oscar.cbls.business.routing.invariants.timeWindow

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.invariants.global._
import oscar.cbls.core.computation.CBLSIntVar

import scala.annotation.tailrec

object TimeWindowConstraintWithLogReduction {

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
            violations: Array[CBLSIntVar]): TimeWindowConstraintWithLogReduction ={

    new TimeWindowConstraintWithLogReduction(gc, n, v,
      singleNodeTransferFunctions,
      travelTimeMatrix, violations)
  }
}

/**
 * This class represent a time window constraint with a logarithmic reduction
 *       --> reducing the preComputation duration but increasing the value computation duration .
 * Given parameters, it maintains the violation value of each vehicle (in violations var)
 * @param gc The GlobalConstraint to which this invariant is linked
 * @param n The number of nodes of the problem (including vehicle)
 * @param v The number of vehicles of the problem
 * @param singleNodeTransferFunctions An array containing the single TransferFunction of each node
 * @param travelTimeMatrix A matrix representing the different travel time between the nodes
 * @param violations An array of CBLSIntVar maintaining the violation of each vehicle
 */
class TimeWindowConstraintWithLogReduction (gc: GlobalConstraintCore,
                            n: Int,
                            v: Int,
                            singleNodeTransferFunctions: Array[TransferFunction],
                            travelTimeMatrix: Array[Array[Long]],
                            val violations: Array[CBLSIntVar]) extends LogReducedGlobalConstraintWithExtremes [TwoWaysTransferFunction,Boolean](gc,n,v){

  // Initialize the vehicles value, the precomputation value and link these invariant to the GlobalConstraintCore
  gc.register(this)
  for(outputVariable <- violations)outputVariable.setDefiningInvariant(gc)

  private val twoWaysTransferFunctionOfNode: Array[TwoWaysTransferFunction] = Array.tabulate(n)(
    node =>
      TwoWaysTransferFunction(singleNodeTransferFunctions(node), singleNodeTransferFunctions(node)))

  private def computeLeavingTime(previousLeavingTime: Long, fromNode: Int, nextTransferFunction: TwoWaysTransferFunction, flipped: Boolean): Long ={
    if(nextTransferFunction.isEmpty(flipped)) return -1L
    val toNode = nextTransferFunction.from(flipped)
    val travelDuration = travelTimeMatrix(fromNode)(toNode)
    nextTransferFunction(previousLeavingTime + travelDuration, flipped)
  }

  private def composeTwoWaysFunctions(f1: TwoWaysTransferFunction, f2: TwoWaysTransferFunction, nonFlippedM: Long, flippedM: Long): TwoWaysTransferFunction ={
    val nonFlippedTF = if(nonFlippedM == -1) EmptyTransferFunction else composeFunction(f1.nonFlippedTF, f2.nonFlippedTF, nonFlippedM)
    val flippedTF = if(flippedM == -1) EmptyTransferFunction else composeFunction(f2.flippedTF, f1.flippedTF, flippedM)
    TwoWaysTransferFunction(nonFlippedTF, flippedTF)
  }

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

    val (ea3, la3, el3) =
      (earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
        earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2,
        latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
        latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2) match{
        case (true,true,true,true) =>
          (f1.la, f1.la, f2.el)                                    // e3 == d1 because latest arrival time at 2L is lower than earliest starting time at 2L
        // so it doesn't matter when you arrive at 1L the resulting leaving time at 2L will be l2
        // => e3 == d1 (the formula says if (t <= e) => l
        case (true,true,false,true) =>
          (f2.ea - f1.el - m + f1.ea, f1.la, f2.el)
        case (true,true,false,false) =>
          (f2.ea - f1.el - m + f1.ea, f2.la - f1.el - m + f1.ea, f2.el)
        case (false,true,false,true) =>
          (f1.ea, f1.la,f1.el + f2.el - f2.ea + m)
        case (false,true,false,false) =>
          (f1.ea, f2.la - f1.el - m + f1.ea, f1.el + f2.el - f2.ea + m)
        case (false,false,false,false) =>
          (1L, -1L, -1L)
        case _ =>
          throw new Error(s"Unhandled case : ${(earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
            earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2,
            latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
            latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2)}")
      }

    if(ea3 > la3)
      EmptyTransferFunction
    else
      DefinedTransferFunction(ea3, la3, el3, f1.from, f2.to)
  }

  /**
    * this method is for composing steps into bigger steps.
    *
    * @param firstStep  the type T associated with stepping over a sequence of nodes (which can be minial two)
    * @param secondStep the type T associated with stepping over a sequence of nodes (which can be minial two)
    * @return the type T associated wit hthe first step followed by the second step
    */
  override def composeSteps(firstStep: TwoWaysTransferFunction, secondStep: TwoWaysTransferFunction): TwoWaysTransferFunction = {

    def travelTimeBetweenTF(flipped: Boolean): Long ={
      val firstStepEndsAt = if(!flipped)firstStep.to(flipped) else secondStep.to(flipped)
      val secondStepStartsAt = if(!flipped)secondStep.from(flipped) else firstStep.from(flipped)
      if(firstStepEndsAt == -1 || secondStepStartsAt == -1) return -1
      travelTimeMatrix(firstStepEndsAt)(secondStepStartsAt)
    }

    composeTwoWaysFunctions(firstStep, secondStep, travelTimeBetweenTF(false), travelTimeBetweenTF(true))
  }

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle  the vehicle that we are focusing on
    * @param segments the segments that constitute the route.
    *                 The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @return the value associated with the vehicle. This value should only be computed based on the provided segments
    */
  override def computeVehicleValueComposed(vehicle: Int, segments: QList[LogReducedSegment[TwoWaysTransferFunction]]): Boolean = {
    @tailrec
    def composeTransferFunctions(transferFunctions: QList[TwoWaysTransferFunction], previousLeavingTime: Long, lastNode: Int, flipped: Boolean): Long ={
      val currentTransferFunction = transferFunctions.head
      if(currentTransferFunction.isEmpty(flipped)) -1L
      else {
        val newLeavingTime = computeLeavingTime(previousLeavingTime, lastNode, currentTransferFunction, flipped)
        if (transferFunctions.tail == null || newLeavingTime < 0L)
          newLeavingTime
        else
          composeTransferFunctions(transferFunctions.tail, newLeavingTime, currentTransferFunction.to(flipped), flipped)
      }
    }

    @tailrec
    def composeLogReduceSegments(logReducedSegments: QList[LogReducedSegment[TwoWaysTransferFunction]],
                                 lastNode: Int = vehicle,
                                 previousLeavingTime: Long = 0L): Long ={
      if(logReducedSegments == null) previousLeavingTime
      else {
        val (newLastNode, newLeavingTime): (Int, Long) = logReducedSegments.head match {
          case s@LogReducedPreComputedSubSequence(_, endNode, steps) =>
            (endNode, composeTransferFunctions(steps, previousLeavingTime, lastNode, false))

          case s@LogReducedFlippedPreComputedSubSequence(_, endNode, steps) =>
            (endNode, composeTransferFunctions(steps.reverse, previousLeavingTime, lastNode, true))

          case s@LogReducedNewNode(node, transferFunctionOfNode) =>
            (node, computeLeavingTime(previousLeavingTime, lastNode, transferFunctionOfNode, false))
        }
        if (newLeavingTime >= 0L)
          composeLogReduceSegments(logReducedSegments.tail, newLastNode, newLeavingTime)
        else
          newLeavingTime
      }
    }
    composeLogReduceSegments(segments) < 0L
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
    * this method is defined for verification purpose. It computes the value of the vehicle from scratch.
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
    * this method delivers the value of the node
    *
    * @return the type T associated with the node "node"
    */
  override def nodeValue(node: Int): TwoWaysTransferFunction = twoWaysTransferFunctionOfNode(node)

  /**
    * this one is similar to the nodeValue except that it only is applied on vehicle,
    * to represent the return to the vehicle start at teh end of its route
    *
    * @param vehicle
    * @return
    */
  override def endNodeValue(vehicle: Int): TwoWaysTransferFunction = twoWaysTransferFunctionOfNode(vehicle)
}
