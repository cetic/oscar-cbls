package oscar.cbls.business.routing.invariants.vehicleCapacity

import oscar.cbls._
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.invariants.global._
import oscar.cbls.core.computation.CBLSIntVar

object GlobalVehicleCapacityConstraint {
  def apply(gc: GlobalConstraintCore, n: Int, v: Int,
            vehiclesCapacity: Array[Long],
            contentVariationAtNode: Array[Long],
            violationPerVehicle: Array[CBLSIntVar]): GlobalVehicleCapacityConstraint =
    new GlobalVehicleCapacityConstraint(
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
  def relevantPredecessorsOfNodes(capacityConstraint: GlobalVehicleCapacityConstraint): Map[Int,Iterable[Int]] ={
    val allNodes = (0 until capacityConstraint.n).toList
    val vehicleMaxCapacity = capacityConstraint.vehiclesCapacity.max
    Array.tabulate(capacityConstraint.n)(node =>
      node -> allNodes.filter(neighbor =>
        capacityConstraint.contentVariationAtNode(node) + capacityConstraint.contentVariationAtNode(neighbor) <= vehicleMaxCapacity)).toMap
  }
}



class GlobalVehicleCapacityConstraint(gc: GlobalConstraintCore, val n: Int, val v: Int,
                                      val vehiclesCapacity: Array[Long],
                                      val contentVariationAtNode: Array[Long],
                                      violationPerVehicle: Array[CBLSIntVar]) extends GlobalConstraintDefinition[Boolean](gc, v){

  violationPerVehicle.foreach(violation => violation.setDefiningInvariant(gc))
  gc.register(this)

  val preComputedValues: Array[Array[VehicleContentFunction]] =
    Array.tabulate(n)(from => Array.tabulate(n)(to =>{
      if(from == to ){
        if(from < v){
          DefinedContentFunction(0,0,0,from,to)
        } else {
          DefinedContentFunction(
            Math.max(0,contentVariationAtNode(from)),
            Math.min(0,contentVariationAtNode(from)),
            contentVariationAtNode(from),
            from,to)
        }
      }
      else EmptyContentFunction
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

    def performPreComputeForNode(node: Int, prevNode: Int, route: QList[Int], lastVCF: VehicleContentFunction): Unit ={
      if(route != null) {
        val curNode = route.head.toInt
        val newVCF = if (lastVCF.isEmpty) lastVCF else composeVehicleContentFunctions(lastVCF, preComputedValues(curNode)(curNode))
        preComputedValues(node)(curNode) = newVCF
        performPreComputeForNode(node, curNode, route.tail, newVCF)
      }
    }

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

  private def composeVehicleContentFunctions(f1: VehicleContentFunction, f2: VehicleContentFunction): VehicleContentFunction ={
    if(f1.isEmpty || f2.isEmpty) return EmptyContentFunction
    val from = f1.from
    val to = f2.to
    val max = Math.max(f1.maxContentIfStartAt0, f1.contentAtEndIfStartAt0 + f2.maxContentIfStartAt0)
    val min = Math.min(f1.minContentIfStartAt0, f1.contentAtEndIfStartAt0 + f2.minContentIfStartAt0)
    val end = f1.contentAtEndIfStartAt0 + f2.contentAtEndIfStartAt0
    DefinedContentFunction(max, min, end, from, to)
  }

  private def segmentsVehicleContentFunction(segment: Segment): VehicleContentFunction ={
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
    def contentAtDepot(segments: QList[Segment], previousSegmentOutputContent: Long = preComputedValues(vehicle)(vehicle).contentAtEndIfStartAt0): Long ={
      val (segment, tail) = (segments.head, segments.tail)
      val vehicleContentFunction = segmentsVehicleContentFunction(segment)
      val tooMuch = vehicleContentFunction.maxContentIfStartAt0 + previousSegmentOutputContent > vehiclesCapacity(vehicle)
      val tooFew = vehicleContentFunction.minContentIfStartAt0 + previousSegmentOutputContent < 0
      val newOutput = vehicleContentFunction.contentAtEndIfStartAt0 + previousSegmentOutputContent
      if(tooMuch || tooFew)
        vehiclesCapacity(vehicle) + 1
      else if(tail != null)
        contentAtDepot(tail, newOutput)
      else newOutput
    }
    val contentAtDepotOrViolationContent = contentAtDepot(segments)
    contentAtDepotOrViolationContent < 0L || contentAtDepotOrViolationContent > vehiclesCapacity(vehicle)
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