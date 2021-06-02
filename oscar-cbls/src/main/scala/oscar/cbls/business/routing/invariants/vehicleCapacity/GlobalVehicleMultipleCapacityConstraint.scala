package oscar.cbls.business.routing.invariants.vehicleCapacity

import oscar.cbls.CBLSIntVar
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.business.routing.invariants.global._
import oscar.cbls.core.computation.ChangingSeqValue

import scala.annotation.tailrec

object GlobalVehicleMultipleCapacityConstraint {
  def apply(routes: ChangingSeqValue, n: Int, v: Int, c: Int,
            vehiclesCapacity: Array[Array[Long]],
            contentVariationAtNode: Array[Array[Long]],
            violationPerVehicle: Array[CBLSIntVar]): GlobalVehicleMultipleCapacityConstraint =
    new GlobalVehicleMultipleCapacityConstraint(
      routes, n, v, c,
      vehiclesCapacity,
      contentVariationAtNode,
      violationPerVehicle)



  def apply(routes: ChangingSeqValue, n: Int, v: Int, c: Int,
            vehiclesCapacity: Array[Array[Long]],
            contentVariationAtNode: Array[Array[Long]]): Array[CBLSIntVar] = {

    val violationOfContentOfVehicle = Array.tabulate(v)(vehicle =>
      CBLSIntVar(routes.model, name = "Violation of capacity of vehicle " + vehicle))

    new GlobalVehicleMultipleCapacityConstraint(
      routes, n, v, c,
      vehiclesCapacity,
      contentVariationAtNode,
      violationOfContentOfVehicle)

    violationOfContentOfVehicle
  }

}

class GlobalVehicleMultipleCapacityConstraint(routes: ChangingSeqValue, override val n: Int, val v: Int,
                                      val c: Int, // Nb of contents
                                      val vehiclesCapacity: Array[Array[Long]],
                                      val contentVariationAtNode: Array[Array[Long]],
                                      violationPerVehicle: Array[CBLSIntVar]) extends GlobalConstraintCore[Boolean](routes, v){

  violationPerVehicle.foreach(violation => violation.setDefiningInvariant(this))

  val preComputedValues: Array[Array[VehicleMultipleContentFunction]] =
    Array.tabulate(n)(from => Array.tabulate(n)(to =>{
      if(from == to ){
        if(from < v){
          DefinedMultipleContentFunction(c,Array.fill(c)(0),Array.fill(c)(0),Array.fill(c)(0),from,to)
        } else {
          DefinedMultipleContentFunction(
            c,
            Array.tabulate(c)(i => Math.max(0,contentVariationAtNode(from)(i))),
            Array.tabulate(c)(i => Math.min(0,contentVariationAtNode(from)(i))),
            contentVariationAtNode(from),
            from,to)
        }
      }
      else EmptyMultipleContentFunction(c)
    }
    ))

  /**
   * This method is called by the framework when a pre-computation must be performed.
   *
   * @param vehicle the vehicle for which a pre-computation must be performed
   * @param routes  the sequence representing the route of all vehicle
   *                BEWARE,other vehicles are also present in this sequence; you must only work on the given vehicle
   */
  override def performPreCompute(vehicle: Int, routes: IntSequence): Unit = {

    @tailrec
    def performPreComputeForNode(node: Int, prevNode: Int, route: QList[Int], lastVCF: VehicleMultipleContentFunction): Unit ={
      if(route != null) {
        val curNode = route.head.toInt
        val newVCF = if (lastVCF.isEmpty) lastVCF else composeVehicleContentFunctions(lastVCF, preComputedValues(curNode)(curNode))
        preComputedValues(node)(curNode) = newVCF
        performPreComputeForNode(node, curNode, route.tail, newVCF)
      }
    }

    @tailrec
    def performPreComputeOnRoute(route: QList[Int]): Unit ={
      val node = route.head
      val lastVCF = preComputedValues(node)(node)
      val prev = node
      performPreComputeForNode(node, prev, route.tail, lastVCF)
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

  private def composeVehicleContentFunctions(f1: VehicleMultipleContentFunction, f2: VehicleMultipleContentFunction): VehicleMultipleContentFunction ={
    if(f1.isEmpty || f2.isEmpty) return EmptyMultipleContentFunction(c)
    val from = f1.from
    val to = f2.to
    val max = Array.tabulate(c)(i => Math.max(f1.maxContentsIfStartAt0(i), f1.contentsAtEndIfStartAt0(i) + f2.maxContentsIfStartAt0(i)))
    val min = Array.tabulate(c)(i => Math.min(f1.minContentsIfStartAt0(i), f1.contentsAtEndIfStartAt0(i) + f2.minContentsIfStartAt0(i)))
    val end = Array.tabulate(c)(i => f1.contentsAtEndIfStartAt0(i) + f2.contentsAtEndIfStartAt0(i))
    DefinedMultipleContentFunction(c, max, min, end, from, to)
  }

  private def segmentsVehicleMultipleContentsFunction(segment: Segment): VehicleMultipleContentFunction ={
    segment match{
      case seg: PreComputedSubSequence => preComputedValues(seg.startNode)(seg.endNode)
      case seg: FlippedPreComputedSubSequence => preComputedValues(seg.startNode)(seg.endNode)
      case seg: NewNode => preComputedValues(seg.node)(seg.node)
    }
  }

  /**
   * This method is called by the framework when the value of a vehicle must be computed.
   *
   * @param vehicle  the vehicle for which we must compute the value
   * @param segments the segments that constitute the route.
   *                 The route of the vehicle is equal to the concatenation of all given segments in the order they appear in this list
   * @param routes   the sequence representing the route of all vehicle
   */
  override protected def computeVehicleValue(vehicle: Int, segments: QList[Segment], routes: IntSequence): Boolean = {
    @tailrec
    def contentAtDepot(segments: QList[Segment], previousSegmentOutputContent: Array[Long] = preComputedValues(vehicle)(vehicle).contentsAtEndIfStartAt0): Array[Long] ={
      val (segment, tail) = (segments.head, segments.tail)
      val vehicleMultipleContentFunction = segmentsVehicleMultipleContentsFunction(segment)
      val tooMuch = (0 until c).exists(i => vehicleMultipleContentFunction.maxContentsIfStartAt0(i) + previousSegmentOutputContent(i) > vehiclesCapacity(vehicle)(i))
      val tooFew = (0 until c).exists(i => vehicleMultipleContentFunction.minContentsIfStartAt0(i) + previousSegmentOutputContent(i) < 0)
      val newOutput = Array.tabulate(c)(i => vehicleMultipleContentFunction.contentsAtEndIfStartAt0(i) + previousSegmentOutputContent(i))
      if(tooMuch || tooFew)
        Array.fill(1)(-1L)
      else if(tail != null)
        contentAtDepot(tail, newOutput)
      else newOutput
    }
    val contentAtDepotOrViolationContent = contentAtDepot(segments)
    (0 until c).exists(i => contentAtDepotOrViolationContent(i) < 0L || contentAtDepotOrViolationContent(i) > vehiclesCapacity(vehicle)(i))
  }

  /**
   * The framework calls this method to assign the value U corresponding to a specific checkpointLevel to the output variable of your invariant.
   * It has been dissociated from the method computeVehicleValue because the system should be able to restore a previously computed value without re-computing it.
   *
   * @param vehicle the vehicle number
   * @param value   The value to assign to the output variable
   */
  override protected def assignVehicleValue(vehicle: Int, value: Boolean): Unit = {
    violationPerVehicle(vehicle) := (if(value) 1L else 0L)
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
  override def computeVehicleValueFromScratch(vehicle: Int, routes:  IntSequence): Boolean = {
    var explorer = routes.explorerAtAnyOccurrence(vehicle)
    var currentContent = contentVariationAtNode(vehicle)
    val maxCapacity = vehiclesCapacity(vehicle)

    // The vehicle content at start is greater than the max allowed in the vehicle (shouldn't happen)
    if((0 until c).exists(i => currentContent(i) > maxCapacity(i))) return true
    explorer = explorer.get.next

    // No node in this vehicle route
    if(vehicle == v-1 && explorer.isEmpty) return false
    else if(vehicle < v-1 && explorer.get.value < v) return false

    while(explorer.isDefined && explorer.get.value >= v){
      val currentNode = explorer.get
      currentContent = currentContent.lazyZip(contentVariationAtNode(currentNode.value)).map(_ + _)
      if((0 until c).exists(i => currentContent(i) > maxCapacity(i) || currentContent(i) < 0)) return true
      explorer = currentNode.next
    }
    false
  }

  /**
   * This method returns for each node an iterable of nodes that could be his neighbor
   *  In clear ==>  given A the node and B a relevant neighbor :
   *                capacity variation of node A + capacity variation of node B < max capacity of all vehicles
   * @return A map : Node -> relevant neighbors
   */
  def relevantPredecessorsOfNodes: Map[Int,Iterable[Int]] ={
    val allNodes = (0 until n).toList
    val vehicleMultipleMaxCapacity = vehiclesCapacity.map(_.max)
    Array.tabulate(n)(node =>
      node -> allNodes.filter(neighbor =>
        !(0 until c).exists(i =>
          contentVariationAtNode(node)(i) + contentVariationAtNode(neighbor)(i) > vehicleMultipleMaxCapacity(i))
        )).toMap
  }

  def multipleContentsOfRoute(vehicle: Int): MultipleCapacitiesResult = {
    val totalPickedUp: Array[Long] = Array.fill(c)(0L)
    val totalDroppedOff: Array[Long] = Array.fill(c)(0L)
    def multipleContentsAtNode(explorer: Option[IntSequenceExplorer], multipleContentsAtPreviousNode: Array[Long] = Array.fill(c)(0L)): List[(Array[Long],Array[Long])] ={
      if(explorer.isEmpty || (vehicle != v - 1 && explorer.get.value == vehicle + 1))
        List.empty[(Array[Long],Array[Long])]
      else{
        val node = explorer.get.value
        val delta = contentVariationAtNode(node)
        val newContent = multipleContentsAtPreviousNode.lazyZip(delta).map(_+_)

        (0 until c).foreach(i =>
          if(delta(i) > 0) totalPickedUp(i)+=delta(i)
          else totalDroppedOff(i)+=delta(i)
        )

        List((multipleContentsAtPreviousNode,newContent)) ::: multipleContentsAtNode(explorer.get.next, newContent)
      }
    }
    val contentFlow: List[(Array[Long],Array[Long])] = multipleContentsAtNode(routes.value.explorerAtAnyOccurrence(vehicle))
    val lastNode = if(vehicle == v-1) routes.value.last else routes.value.explorerAtAnyOccurrence(vehicle+1).get.prev.get.value
    val maxContentAnyTime = preComputedValues(vehicle)(lastNode).maxContentsIfStartAt0
    MultipleCapacitiesResult(totalPickedUp, totalDroppedOff, maxContentAnyTime, contentFlow.toArray)
  }

}

case class MultipleCapacitiesResult(totalPickedUp: Array[Long], totalDroppedOff: Array[Long], maximumContentAnyTime: Array[Long], contentFlow: Array[(Array[Long],Array[Long])])