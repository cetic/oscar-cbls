package oscar.cbls.business.routing.invariants

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.business.routing.invariants.global._
import oscar.cbls.core.computation.CBLSIntVar

object WeightedNodesPerVehicle{
  /**
    * this constraints maintains the number of node per vehicle.
    *
    * @param gc The GlobalConstraint linked to this constraint
    * @param v number of vehicle
    * @param weightPerVehicle an array telling how many nodes are reached per vehicle
    */
  def apply(gc: GlobalConstraintCore, n: Int, v : Int, nodeWeight:Array[Long], weightPerVehicle : Array[CBLSIntVar]) =
    new WeightedNodesPerVehicle(gc, n, v, nodeWeight, weightPerVehicle)
}

/**
  * this constraints maintains the number of node per vehicle.
  *
  * @param gc               The GlobalConstraint linked to this constraint
  * @param v                number of vehicle
  * @param weightPerVehicle an array telling how many nodes are reached per vehicle
  */
class WeightedNodesPerVehicle(gc: GlobalConstraintCore, n: Int, v : Int, nodeWeight:Array[Long], weightPerVehicle : Array[CBLSIntVar])
  extends GlobalConstraintDefinition[Long](gc,v){

  val preComputedVals: Array[Long] = Array.fill(n)(0L)

  /**
    * tis method is called by the framework when a pre-computation must be performed.
    * you are expected to assign a value of type T to each node of the vehicle "vehicle" through the method "setNodeValue"
    *
    * @param vehicle      the vehicle where pre-computation must be performed
    * @param routes       the sequence representing the route of all vehicle
    *                     BEWARE,other vehicles are also present in this sequence; you must only work on the given vehicle
    */
  override def performPreCompute(vehicle: Int, routes: IntSequence): Unit = {
    var cumulatedWeight = 0L
    var continue = true
    var vExplorer = routes.explorerAtAnyOccurrence(vehicle)
    while(continue){
      vExplorer match {
        case None => continue = false
        case Some(elem) =>
          if (elem.value < v && elem.value != vehicle){
            continue = false
          } else {
            cumulatedWeight = cumulatedWeight + nodeWeight(elem.value)
            preComputedVals(elem.value) = cumulatedWeight
          }
          vExplorer = elem.next
      }
    }
  }

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle   the vehicle that we are focusing on
    * @param segments  the segments that constitute the route.
    *                  The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @param routes    the sequence representing the route of all vehicle
    * @return the value associated with the vehicle
    */
  override def computeVehicleValue(vehicle: Int, segments: QList[Segment], routes: IntSequence): Long = {
    QList.qMap(segments, (s: Segment) =>
      s match {
        case PreComputedSubSequence (fstNode, lstNode, _) =>
          preComputedVals(lstNode) - preComputedVals(fstNode) + nodeWeight(fstNode)
        case FlippedPreComputedSubSequence(lstNode,fstNode,_) =>
          preComputedVals(lstNode) - preComputedVals(fstNode) + nodeWeight(fstNode)
        case NewNode(node) =>
          nodeWeight(node)
      }).sum
    //println("Vehicle : " + vehicle + "--" + segments.mkString(","))
  }

  /**
    * the framework calls this method to assign the value U to he output variable of your invariant.
    * It has been dissociated from the method above because the framework memorizes the output value of the vehicle,
    * and is able to restore old value without the need to re-compute them, so it only will call this assignVehicleValue method
    *
    * @param vehicle the vehicle number
    */
  override def assignVehicleValue(vehicle: Int, value: Long): Unit = {
    weightPerVehicle(vehicle) := value
  }

  def computeRouteWeight(vehicle : Int,vExplorer : Option[IntSequenceExplorer]) : Long = {
    vExplorer match {
      case None => 0
      case Some(elem) =>
        if (elem.value < v) 0
        else nodeWeight(elem.value) + computeRouteWeight(vehicle,elem.next)
    }
  }

  /**
    *
    * @param vehicle
    * @param routes
    * @return
    */
  override def computeVehicleValueFromScratch(vehicle: Int, routes: IntSequence): Long = {
    nodeWeight(vehicle) + computeRouteWeight(vehicle,routes.explorerAtAnyOccurrence(vehicle).get.next)
  }
}
