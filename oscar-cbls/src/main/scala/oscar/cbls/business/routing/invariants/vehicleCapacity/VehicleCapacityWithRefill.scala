/*package oscar.cbls.business.routing.invariants.vehicleCapacity

import oscar.cbls._
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.business.routing.invariants.global._
import oscar.cbls.business.routing.invariants.vehicleCapacity.VehicleCapacityWithRefill.ActionAtNode
import oscar.cbls.core.ChangingSeqValue

import scala.annotation.tailrec

abstract sealed class VehicleContentFunctionRefill() {
  def max(startContent: Long): Long
  def min(startContent: Long): Long
  def compose(c:VehicleContentFunctionRefill):VehicleContentFunctionRefill
}

case class VehicleContentFunctionNoRefill(maxContentIfStartAt0: Long,
                                          minContentIfStartAt0: Long,
                                          contentAtEndIfStartAt0: Long)
  extends VehicleContentFunctionRefill(){

  def max(startContent: Long): Long = {
    maxContentIfStartAt0 + startContent
  }

  def min(startContent: Long): Long = {
    minContentIfStartAt0 + startContent
  }

  def compose(c:VehicleContentFunctionRefill):VehicleContentFunctionRefill = {
    c match{
      case that:VehicleContentFunctionNoRefill =>
        VehicleContentFunctionNoRefill(
          maxContentIfStartAt0 = Math.max(this.maxContentIfStartAt0, this.contentAtEndIfStartAt0 + that.maxContentIfStartAt0),
          minContentIfStartAt0 = Math.min(this.minContentIfStartAt0, this.contentAtEndIfStartAt0 + that.minContentIfStartAt0),
          contentAtEndIfStartAt0 = this.contentAtEndIfStartAt0 + that.contentAtEndIfStartAt0)
      case that:VehicleContentFunctionWithRefill =>
        VehicleContentFunctionWithRefill(
          maxContentBeforeFirstRefillIfStartAt0 = Math.max(this.maxContentIfStartAt0, this.contentAtEndIfStartAt0 + that.maxContentBeforeFirstRefillIfStartAt0),
          minContentBeforeFirstRefillIfStartAt0 = Math.min(this.minContentIfStartAt0, this.contentAtEndIfStartAt0 + that.minContentBeforeFirstRefillIfStartAt0),
          contentBeforeFirstRefillIfStartAt0 = this.contentAtEndIfStartAt0 + that.contentBeforeFirstRefillIfStartAt0,
          maxContentAfterFirstRefill = that.maxContentAfterFirstRefill,
          minContentAfterFirstRefill = that.minContentAfterFirstRefill,
          contentAtEnd = that.contentAtEnd)
    }
  }
}

case class VehicleContentFunctionWithRefill(maxContentBeforeFirstRefillIfStartAt0: Long,
                                            minContentBeforeFirstRefillIfStartAt0: Long,
                                            contentBeforeFirstRefillIfStartAt0: Long,
                                            maxContentAfterFirstRefill: Long,
                                            minContentAfterFirstRefill: Long,
                                            contentAtEnd: Long)
  extends VehicleContentFunctionRefill() {

  def max(startContent: Long): Long = {
    Math.max(maxContentBeforeFirstRefillIfStartAt0 + startContent,maxContentAfterFirstRefill)
  }

  def min(startContent: Long): Long = {
    Math.min(minContentBeforeFirstRefillIfStartAt0 + startContent,minContentAfterFirstRefill)
  }

  override def compose(c: VehicleContentFunctionRefill): VehicleContentFunctionRefill =
    c match{
      case that:VehicleContentFunctionNoRefill =>
        VehicleContentFunctionWithRefill(
          maxContentBeforeFirstRefillIfStartAt0 = this.maxContentBeforeFirstRefillIfStartAt0,
          minContentBeforeFirstRefillIfStartAt0 = this.minContentBeforeFirstRefillIfStartAt0,
          contentBeforeFirstRefillIfStartAt0 = this.contentBeforeFirstRefillIfStartAt0,
          maxContentAfterFirstRefill = Math.max(this.maxContentAfterFirstRefill, this.contentAtEnd + that.maxContentIfStartAt0),
          minContentAfterFirstRefill = Math.min(this.minContentBeforeFirstRefillIfStartAt0, this.contentAtEnd + that.minContentIfStartAt0),
          contentAtEnd = this.contentAtEnd + that.contentAtEndIfStartAt0)

      case that:VehicleContentFunctionWithRefill =>
        VehicleContentFunctionWithRefill(
          maxContentBeforeFirstRefillIfStartAt0 = this.maxContentBeforeFirstRefillIfStartAt0,
          minContentBeforeFirstRefillIfStartAt0 = this.minContentBeforeFirstRefillIfStartAt0,
          contentBeforeFirstRefillIfStartAt0 = this.contentBeforeFirstRefillIfStartAt0,
          maxContentAfterFirstRefill = List(this.maxContentAfterFirstRefill, this.contentAtEnd + that.maxContentBeforeFirstRefillIfStartAt0, that.maxContentAfterFirstRefill).max,
          minContentAfterFirstRefill = List(this.minContentAfterFirstRefill, this.contentAtEnd + that.minContentBeforeFirstRefillIfStartAt0, that.minContentAfterFirstRefill).min,
          contentAtEnd = that.contentAtEnd)
    }
}

object VehicleCapacityWithRefill{

  def delta(delta:Long):VehicleContentFunctionRefill = VehicleContentFunctionNoRefill(delta, delta, delta)
  def setContent(content:Long):VehicleContentFunctionRefill = VehicleContentFunctionWithRefill(Long.MinValue, Long.MaxValue, 0, content, content, content)
  def noAction:VehicleContentFunctionRefill = delta(0)

  def apply(routes: ChangingSeqValue, n: Int, v: Int,
            vehiclesCapacity: Array[Long],
            actionAtNode: Array[VehicleContentFunctionRefill],
            violationPerVehicle: Array[CBLSIntVar]): VehicleCapacityWithRefill =
    new VehicleCapacityWithRefill(
      routes, n, v,
      vehiclesCapacity,
      actionAtNode,
      violationPerVehicle)
}

protected case class TwoWaysVehicleContentWithRefillFunction(nonFlippedFunction: VehicleContentFunctionRefill,
                                                             flippedFunction: VehicleContentFunctionRefill){


  def contentAtEndIfStartAt0(flipped: Boolean): Long =
    if(flipped) flippedFunction.contentAtEndIfStartAt0
    else nonFlippedFunction.contentAtEndIfStartAt0

  def apply(startContent: Long, maxVehicleContent: Long, flipped: Boolean): Boolean ={
    val vehicleContentFunction = if(flipped)flippedFunction else nonFlippedFunction
    vehicleContentFunction.max(startContent) > maxVehicleContent || vehicleContentFunction.min(startContent) < 0
  }

  def isEmpty(flipped: Boolean): Boolean =
    if(flipped)flippedFunction.isEmpty
    else nonFlippedFunction.isEmpty

  override def toString: String = {
    s"""Two ways vehicle content function :
       |Non-flipped : $nonFlippedFunction
       |Flipped : $flippedFunction
       |""".stripMargin
  }
}

class VehicleCapacityWithRefill(routes: ChangingSeqValue, override val n: Int, val v: Int,
                                val vehiclesCapacity: Array[Long],
                                val actionAtNode: Array[VehicleContentFunctionRefill],
                                violationPerVehicle: Array[CBLSIntVar])
  extends LogReducedGlobalConstraint[TwoWaysVehicleContentWithRefillFunction, Boolean](routes, n, v) {

  violationPerVehicle.foreach(violation => violation.setDefiningInvariant(this))

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

          case x =>
            throw new Error(s"Unhandled match with $x")
        }
        (newOutCapa < 0) || isVehicleCapacityViolated(logReducedSegments.tail, newOutCapa)
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

  /**
   * WARNING : know what you're doing
   * Perform the precomputation on all vehicle given the current value of routes.
   * The main purpose of this method is to update the vehicle content functions after the end
   * of the optimisation for result extraction purpose.
   */
  def performPreComputeOnAllVehicle() ={
    val curRoutes = routes.value
    for(curV <- 0 until v){
      performPreCompute(curV, curRoutes)
    }
  }

  def contentsOfRoute(vehicle: Int): Array[Long] = {
    def contentAtNode(explorer: Option[IntSequenceExplorer], contentAtPreviousNode: Long = 0L): List[Long] ={
      if(explorer.isEmpty || (vehicle != v - 1 && explorer.get.value == vehicle + 1))
        List.empty
      else{
        val content = contentAtPreviousNode + contentVariationAtNode(explorer.get.value)
        List(content) ::: contentAtNode(explorer.get.next, content)
      }
    }
    contentAtNode(routes.value.explorerAtAnyOccurrence(vehicle)).toArray
  }
}
*/