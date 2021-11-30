package oscar.cbls.business.routing.invariants.vehicleCapacity

import oscar.cbls._
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.invariants.global._
import oscar.cbls.core.ChangingSeqValue

import scala.annotation.tailrec

/**
 * This constraint represents a vehicle capacity with refill.
 *
 * All vehicles have a given max capacity; the content must be between zero and this max capacity
 * The content of the vehicle changes at eah node through two possible actions:
 * - through a delta: content' = content + delta_node
 * - through a refill: content! = constantValue_node
 *
 * WARNING: this is very low maturity constraint and needs validation!
 */
object VehicleCapacityWithRefill{

  /**
   * when getting to the node,the capacity is changed by a delta, positive or negative)
   * @param delta the delta on the content of the vehicle
   * @return
   */
  def delta(delta:Long):VehicleContentFunctionRefill = VehicleContentFunctionNoRefill(delta, delta, delta)

  /**
   * wje, getting to the node, the content is set to a definite content (refill, empty, whatever)
   * @param content the content of the vehicle when leaves this node
   * @return
   */
  def setContent(content:Long):VehicleContentFunctionRefill = VehicleContentFunctionWithRefill(Long.MinValue, Long.MaxValue, 0, content, content, content)

  /**
   * when getting to the node, the content of the vehicle is unchanged
   * this is identical to delta(0)
   * @return
   */
  def noAction:VehicleContentFunctionRefill = delta(0)

  /**
   * intantiate the CapacityWithRefill constraint
   * @param routes the route
   * @param n the number of points
   * @param v the number of vehicles
   * @param vehiclesCapacity the max capacity of each vehicle (min is zero)
   * @param actionAtNode for each node, the action on the capacity; check the methods in thi object: noAction, setContent and delta
   * @param violationPerVehicle the output of the constraint: 1 if there is a violation, 0 otherwise
   * @return
   */
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

abstract sealed class VehicleContentFunctionRefill() {
  def min(startContent: Long): Long
  def max(startContent: Long): Long
  def contentAtEnd(startContent:Long):Long
  def compose(c:VehicleContentFunctionRefill):VehicleContentFunctionRefill
}

case class VehicleContentFunctionNoRefill(maxContentIfStartAt0: Long,
                                          minContentIfStartAt0: Long,
                                          contentAtEndIfStartAt0: Long)
  extends VehicleContentFunctionRefill(){

  def min(startContent: Long): Long = minContentIfStartAt0 + startContent

  def max(startContent: Long): Long = maxContentIfStartAt0 + startContent

  override def contentAtEnd(startContent: Long): Long = startContent + contentAtEndIfStartAt0

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

  def min(startContent: Long): Long =
    Math.min(minContentBeforeFirstRefillIfStartAt0 + startContent,minContentAfterFirstRefill)

  def max(startContent: Long): Long =
    Math.max(maxContentBeforeFirstRefillIfStartAt0 + startContent,maxContentAfterFirstRefill)

  override def contentAtEnd(startContent: Long): Long =
    contentAtEnd

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


protected case class TwoWaysVehicleContentWithRefillFunction(nonFlippedFunction: VehicleContentFunctionRefill,
                                                             flippedFunction: VehicleContentFunctionRefill){

  def apply(startContent: Long, maxVehicleContent: Long, flipped: Boolean): Boolean ={
    val vehicleContentFunction = if(flipped)flippedFunction else nonFlippedFunction
    vehicleContentFunction.max(startContent) > maxVehicleContent || vehicleContentFunction.min(startContent) < 0
  }

  def compose(that:TwoWaysVehicleContentWithRefillFunction):TwoWaysVehicleContentWithRefillFunction = {
    TwoWaysVehicleContentWithRefillFunction(
      nonFlippedFunction = this.nonFlippedFunction compose that.nonFlippedFunction,
      flippedFunction = that.flippedFunction compose this.flippedFunction)
  }

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

  val contentFunctionAtNode: Array[TwoWaysVehicleContentWithRefillFunction] =
    Array.tabulate(n)(node => TwoWaysVehicleContentWithRefillFunction(actionAtNode(node),actionAtNode(node)))

  /**
   * this method delivers the value of the node
   *
   * @return the type T associated with the node "node"
   */
  override def nodeValue(node: Int): TwoWaysVehicleContentWithRefillFunction = contentFunctionAtNode(node)

  /**
   * this one is similar to the nodeValue except that it only is applied on vehicle,
   * to represent the return to the vehicle start at the end of its route
   *
   * @param vehicle th
   * @return
   */
  override def endNodeValue(vehicle: Int): TwoWaysVehicleContentWithRefillFunction =
    TwoWaysVehicleContentWithRefillFunction(VehicleCapacityWithRefill.noAction,VehicleCapacityWithRefill.noAction)

  override def composeSteps(firstStep: TwoWaysVehicleContentWithRefillFunction,
                            secondStep: TwoWaysVehicleContentWithRefillFunction): TwoWaysVehicleContentWithRefillFunction = {
    firstStep.compose(secondStep)
  }

  override def computeVehicleValueComposed(vehicle: Int,
                                           segments: QList[LogReducedSegment[TwoWaysVehicleContentWithRefillFunction]]): Boolean = {
    val maxCapa = vehiclesCapacity(vehicle)

    @tailrec
    def evaluateOnSegments(currentContent:Long,
                           segments : QList[LogReducedSegment[TwoWaysVehicleContentWithRefillFunction]]):Option[Long] = {
      if(currentContent > maxCapa) return None
      if(currentContent < 0) return None
      if(segments == null) return Some(currentContent)
      val head = segments.head
      evaluateOnSubSegments(currentContent,head) match{
        case None => None
        case Some(outContent) => evaluateOnSegments(outContent, segments.tail)
      }
    }

    def evaluateOnSubSegments(currentContent:Long,
                              subsegment:LogReducedSegment[TwoWaysVehicleContentWithRefillFunction]):Option[Long] = {
      subsegment match{
        case l:LogReducedPreComputedSubSequence[TwoWaysVehicleContentWithRefillFunction] =>
          evaluateOnStepsForward(currentContent,l.steps)
        case f:LogReducedFlippedPreComputedSubSequence[TwoWaysVehicleContentWithRefillFunction] =>
          evaluateOnStepsBackwards(currentContent,f.steps)
        case n:LogReducedNewNode[TwoWaysVehicleContentWithRefillFunction] =>
          evaluateOnStepsForward(currentContent:Long, steps = QList(n.value))
      }
    }

    @tailrec
    def evaluateOnStepsForward(currentContent:Long,
                               steps : QList[TwoWaysVehicleContentWithRefillFunction]):Option[Long] = {
      if(currentContent > maxCapa) return None
      if(currentContent < 0) return None
      if(steps == null) return Some(currentContent)
      val head = steps.head
      if(head.nonFlippedFunction.max(currentContent) > maxCapa) return None
      if(head.nonFlippedFunction.min(currentContent) < 0) return None
      evaluateOnStepsForward(head.nonFlippedFunction.contentAtEnd(currentContent),steps.tail)
    }

    @tailrec
    def evaluateOnStepsBackwards(currentContent:Long,
                                 steps : QList[TwoWaysVehicleContentWithRefillFunction]):Option[Long] = {
      //TODO: I do not know if I have to iterate in reverse order here!!!
      if(currentContent > maxCapa) return None
      if(currentContent < 0) return None
      if(steps == null) return Some(currentContent)
      val head = steps.head
      if(head.flippedFunction.max(currentContent) > maxCapa) return None
      if(head.flippedFunction.min(currentContent) < 0) return None
      evaluateOnStepsBackwards(head.flippedFunction.contentAtEnd(currentContent),steps.tail)
    }

    evaluateOnSegments(0,segments).isEmpty //empty means violation
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
    var currentContent:Long = actionAtNode(vehicle).contentAtEnd(0)
    val maxCapacity = vehiclesCapacity(vehicle)

    // The vehicle content at start is greater than the max allowed in the vehicle (shouldn't happen)
    if(currentContent > maxCapacity) return true
    if(currentContent < 0) return true

    explorer = explorer.get.next

    // No node in this vehicle route
    if(vehicle == v-1 && explorer.isEmpty) return false
    else if(vehicle < v-1 && explorer.get.value < v) return false

    while(explorer.isDefined && explorer.get.value >= v){
      val currentNode = explorer.get
      currentContent = actionAtNode(currentNode.value).contentAtEnd(currentContent)
      if(currentContent < 0) return true
      if(currentContent > maxCapacity) return true
      explorer = currentNode.next
    }
    false
  }
}
