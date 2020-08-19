package oscar.cbls.business.routing.invariants.global

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.CBLSIntVar

case class PreComputedDistances(distanceFromStart:Long,
                                distanceToStart:Long)

class RouteLength(gc: GlobalConstraintCore, n: Int, v:Int, vehicleToRouteLength:Array[CBLSIntVar], assymetricDistance:(Int,Int)=>Long)
  extends GlobalConstraintDefinition[Long](gc,v){

  val preComputedVals: Array[PreComputedDistances] = Array.fill(n)(PreComputedDistances(0,0))

  // Initialize the vehicles value, the precomputation value and link these invariant to the GlobalConstraintCore
  gc.register(this)
  for(outputVariable <- vehicleToRouteLength)outputVariable.setDefiningInvariant(gc)

  override def performPreCompute(vehicle: Int, routes: IntSequence): Unit = {
    var previousNode = vehicle
    var prevPreComputedValue =PreComputedDistances(0,0)
    preComputedVals(vehicle) = prevPreComputedValue

    var currentExplorerOPt:Option[IntSequenceExplorer] = routes.explorerAtAnyOccurrence(vehicle).get.next
    //We start at the first node after vehicle start

    while(currentExplorerOPt match{
      case None =>
        //we are at the end of the last route
        false
      case Some(explorer) =>
        if(explorer.value < v && explorer.value == vehicle+1){
          //we start the next vehicle
          false
        }else{
          //we are not starting the next vehicle, just continue on the current one
          //We tag the current node with the proper value accumulatin on gthe previous node

          prevPreComputedValue = PreComputedDistances(
            distanceFromStart = prevPreComputedValue.distanceFromStart + assymetricDistance(previousNode,explorer.value),
            distanceToStart = prevPreComputedValue.distanceToStart + assymetricDistance(explorer.value,previousNode))

          previousNode = explorer.value
          preComputedVals(explorer.value) = prevPreComputedValue
          currentExplorerOPt = explorer.next
          true
        }
    }){}
  }

  override def computeVehicleValue(vehicle: Int,
                                   segments: QList[Segment],
                                   routes: IntSequence): Long = {
    def digestListOfSegments(segments: QList[Segment], prevNode: Int): Long = {
      segments match {
        case null =>
          //return home
          assymetricDistance(prevNode,vehicle)
        case segmentQList =>
          val head = segmentQList.head
          val tail = segmentQList.tail
          head match {
            case PreComputedSubSequence(startNode, endNode, _) =>
              val distanceToEnterThisSegment = if (prevNode == -1) 0 else assymetricDistance(prevNode,startNode)
              val lengthOfThisSegment = preComputedVals(endNode).distanceFromStart - preComputedVals(startNode).distanceFromStart
              require(lengthOfThisSegment >= 0)
              distanceToEnterThisSegment + lengthOfThisSegment + digestListOfSegments(tail, endNode)

            case FlippedPreComputedSubSequence(startNode, endNode, _) =>
              val distanceToEnterThisSegment = if (prevNode == -1) 0 else assymetricDistance(prevNode,startNode)
              val lengthOfThisSegment = preComputedVals(startNode).distanceToStart - preComputedVals(endNode).distanceToStart
              require(lengthOfThisSegment >= 0)
              distanceToEnterThisSegment + lengthOfThisSegment + digestListOfSegments(tail, endNode)

            case NewNode(node) =>
              val distanceToEnterThisSegment = if (prevNode == -1) 0 else assymetricDistance(prevNode,node)
              distanceToEnterThisSegment + digestListOfSegments(tail, node)
          }
      }
    }
    digestListOfSegments(segments,-1)
  }

  override def assignVehicleValue(vehicle: Int, value: Long): Unit = {
    vehicleToRouteLength(vehicle) := value
  }

  override def computeVehicleValueFromScratch(vehicle: Int, routes: IntSequence): Long = {
    var previousNode = vehicle
    var toReturn:Long = 0

    var currentExplorerOPt:Option[IntSequenceExplorer] = routes.explorerAtAnyOccurrence(vehicle).get.next
    //We start at the first node after vehicle start

    while(currentExplorerOPt match{
      case None =>
        //we are at the end of the last route
        false
      case Some(explorer) =>
        if(explorer.value < v && explorer.value == vehicle+1){
          //we start the next vehicle
          false
        }else{
          //we are not starting the next vehicle, just continue on the current one
          toReturn += assymetricDistance(previousNode,explorer.value)
          previousNode = explorer.value
          currentExplorerOPt = explorer.next
          true
        }
    }){}

    toReturn + assymetricDistance(previousNode,vehicle) //return
  }
}
