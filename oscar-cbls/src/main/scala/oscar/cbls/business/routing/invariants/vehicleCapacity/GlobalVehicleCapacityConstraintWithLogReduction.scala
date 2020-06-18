package oscar.cbls.business.routing.invariants.vehicleCapacity

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.invariants.global._
import oscar.cbls.core.computation.CBLSIntVar

import scala.annotation.tailrec

object GlobalVehicleCapacityConstraintWithLogReduction{
  def apply(gc: GlobalConstraintCore, n: Int, v: Int,
            vehiclesCapacity: Array[Long],
            contentVariationAtNode: Array[Long],
            violationPerVehicle: Array[CBLSIntVar]): GlobalVehicleCapacityConstraintWithLogReduction =
    new GlobalVehicleCapacityConstraintWithLogReduction(
      gc, n, v,
      vehiclesCapacity,
      contentVariationAtNode,
      violationPerVehicle)

  /**
   * This method returns for each node an iterable of nodes that could be his neighbor
   *  In clear ==>  given A the node and B a relevant neighbor :
   *                capacity variation of node A + capacity variation of node B < max capacity of all vehicles
   * @param capacityConstraint A capacity constraint
   * @return A map : Node -> relevant neighbors
   */
  def relevantPredecessorsOfNodes(capacityConstraint: GlobalVehicleCapacityConstraintWithLogReduction): Map[Int,Iterable[Int]] ={
    val allNodes = (0 until capacityConstraint.n).toList
    val vehicleMaxCapacity = capacityConstraint.vehiclesCapacity.max
    Array.tabulate(capacityConstraint.n)(node =>
      node -> allNodes.filter(neighbor =>
        capacityConstraint.contentVariationAtNode(node) + capacityConstraint.contentVariationAtNode(neighbor) <= vehicleMaxCapacity)).toMap
  }
}

class GlobalVehicleCapacityConstraintWithLogReduction(gc: GlobalConstraintCore, val n: Int, val v: Int,
                                                      val vehiclesCapacity: Array[Long],
                                                      val contentVariationAtNode: Array[Long],
                                                      violationPerVehicle: Array[CBLSIntVar]) extends LogReducedGlobalConstraint[TwoWaysVehicleContentFunction, Boolean](gc, n, v) {
  violationPerVehicle.foreach(violation => violation.setDefiningInvariant(gc))
  gc.register(this)

  val contentFunctionAtNode: Array[TwoWaysVehicleContentFunction] =
    Array.tabulate(n)(node => TwoWaysVehicleContentFunction(
      DefinedContentFunction(contentVariationAtNode(node),contentVariationAtNode(node),contentVariationAtNode(node), node, node),
        DefinedContentFunction(contentVariationAtNode(node),contentVariationAtNode(node),contentVariationAtNode(node), node, node)))

  // For the vehicle return value we consider that by default nothing is loaded/unloaded at the depot
  // (it's a fictive node)
  val contentFunctionForVehicleReturn: Array[TwoWaysVehicleContentFunction] =
    Array.tabulate(v)(vehicle => TwoWaysVehicleContentFunction(
      DefinedContentFunction(0,0,0,vehicle, vehicle),DefinedContentFunction(0,0,0,vehicle, vehicle)))

  /**
    * this method delivers the value of the node
    *
    * @return the type T associated with the node "node"
    */
  override def nodeValue(node: Int): TwoWaysVehicleContentFunction = contentFunctionAtNode(node)

  /**
    * this one is similar to the nodeValue except that it only is applied on vehicle,
    * to represent the return to the vehicle start at teh end of its route
    *
    * @param vehicle
    * @return
    */
  override def endNodeValue(vehicle: Int): TwoWaysVehicleContentFunction = contentFunctionForVehicleReturn(vehicle)

  /**
    * this method is for composing steps into bigger steps.
    *
    * @param firstStep  the type T associated with stepping over a sequence of nodes (which can be minial two)
    * @param secondStep the type T associated with stepping over a sequence of nodes (which can be minial two)
    * @return the type T associated wit hthe first step followed by the second step
    */
  override def composeSteps(firstStep: TwoWaysVehicleContentFunction, secondStep: TwoWaysVehicleContentFunction): TwoWaysVehicleContentFunction = {
    val flipped = composeVehicleContentFunctions(secondStep.flippedFunction, firstStep.flippedFunction)
    val nonFlipped = composeVehicleContentFunctions(firstStep.nonFlippedFunction, secondStep.nonFlippedFunction)
    TwoWaysVehicleContentFunction(nonFlipped, flipped)
  }

  private def composeVehicleContentFunctions(f1: VehicleContentFunction, f2: VehicleContentFunction): VehicleContentFunction ={
    if(f1.isEmpty || f2.isEmpty) return EmptyContentFunction
    val from = f1.from
    val to = f2.to
    val max = Math.max(f1.maxContentIfStartAt0, f1.contentAtEndIfStartAt0 + f2.maxContentIfStartAt0)
    val min = Math.min(f1.minContentIfStartAt0, f1.contentAtEndIfStartAt0 + f2.minContentIfStartAt0)
    val end = f1.contentAtEndIfStartAt0 + f2.contentAtEndIfStartAt0
    DefinedContentFunction(max, min, end, from, to)
  }

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle  the vehicle that we are focusing on
    * @param segments the segments that constitute the route.
    *                 The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @return the value associated with the vehicle. This value should only be computed based on the provided segments
    */
  override def computeVehicleValueComposed(vehicle: Int, segments: QList[LogReducedSegment[TwoWaysVehicleContentFunction]]): Boolean = {

    @tailrec
    def composeSubSegments(vehicleContentFunctions: QList[TwoWaysVehicleContentFunction], previousOutCapa: Long, flipped: Boolean): Long ={
      val twoWaysVehicleContentFunction = vehicleContentFunctions.head
      val newOutCapa = previousOutCapa + twoWaysVehicleContentFunction.contentAtEndIfStartAt0(flipped)
      val isMaxCapaOfSegmentViolated = twoWaysVehicleContentFunction(previousOutCapa, vehiclesCapacity(vehicle), flipped)
      if(isMaxCapaOfSegmentViolated) -1
      else if(vehicleContentFunctions.tail == null) newOutCapa
      else composeSubSegments(vehicleContentFunctions.tail, newOutCapa, flipped)
    }

    @tailrec
    def isVehicleCapacityViolated(logReducedSegments: QList[LogReducedSegment[TwoWaysVehicleContentFunction]],
                                  previousOutCapa: Long = 0L): Boolean ={
      if(logReducedSegments == null) false
      else {
        val newOutCapa: Long = logReducedSegments.head match {
          case s@LogReducedPreComputedSubSequence(_, _, steps) =>
            composeSubSegments(steps, previousOutCapa, false)

          case s@LogReducedFlippedPreComputedSubSequence(_, _, steps) =>
            composeSubSegments(steps.reverse, previousOutCapa, true)

          case s@LogReducedNewNode(node, vehicleContentFunctionOfNode) =>
            val isMaxCapaOfNodeViolated = vehicleContentFunctionOfNode(previousOutCapa, vehiclesCapacity(vehicle), true)
            if(isMaxCapaOfNodeViolated) -1
            else previousOutCapa + vehicleContentFunctionOfNode.contentAtEndIfStartAt0(true)
        }
        if (newOutCapa < 0) true
        else
          isVehicleCapacityViolated(logReducedSegments.tail, newOutCapa)
      }
    }
    isVehicleCapacityViolated(segments)
  }

  /**
    * The framework calls this method to assign the value U corresponding to a specific checkpointLevel to the output variable of your invariant.
    * It has been dissociated from the method computeVehicleValue because the system should be able to restore a previously computed value without re-computing it.
    *
    * @param vehicle the vehicle number
    * @param value   The value to assign to the output variable
    */
  override def assignVehicleValue(vehicle: Int, value: Boolean): Unit = {
    if(value) violationPerVehicle(vehicle) := 1 else violationPerVehicle(vehicle) := 0
  }

  /**
    * This method is mainly defined for verification purpose.
    * But it's also used when we can't compute the vehicle value incrementally
    * (at the beginning of the search or when we assign the value of the route)
    * It computes the value of the vehicle from scratch.
    *
    * @param vehicle the vehicle on which the value is computed
    * @param routes  the sequence representing the route of all vehicle
    */
  override def computeVehicleValueFromScratch(vehicle: Int, routes: IntSequence): Boolean = {
    var explorer = routes.explorerAtAnyOccurrence(vehicle)
    var currentContent = contentVariationAtNode(vehicle)
    val maxCapacity = vehiclesCapacity(vehicle)

    // The vehicle content at start is greater than the max allowed in the vehicle (shouldn't happen)
    if(currentContent > maxCapacity) return true
    explorer = explorer.get.next

    // No node in this vehicle route
    if(vehicle == v-1 && explorer.isEmpty) return false
    else if(vehicle < v-1 && explorer.get.value < v) return false

    while(explorer.isDefined && explorer.get.value >= v){
      val currentNode = explorer.get
      currentContent += contentVariationAtNode(currentNode.value)
      if(currentContent > maxCapacity || currentContent < 0) return true
      explorer = currentNode.next
    }
    false
  }
}
