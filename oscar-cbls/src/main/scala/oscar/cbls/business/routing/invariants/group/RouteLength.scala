package oscar.cbls.business.routing.invariants.group

import oscar.cbls._
import oscar.cbls.algo.quick.QList
import oscar.cbls.{CBLSIntVar, Variable}
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.ChangingSeqValue

case class PreComputedDistances(distanceFromStart:Long,
                                distanceToStart:Long)

@deprecated("needs testing","")
class RouteLength(gc: GlobalConstraintCore, n: Int, v:Int, vehicleToRouteLength:Array[CBLSIntVar], assymetricDistance:(Long,Long)=>Long)
  extends GlobalConstraintDefinition(gc,v){

  type U = Long

  val preComputedVals: Array[PreComputedDistances] = Array.fill(n)(PreComputedDistances(0,0))

  // Initialize the vehicles value, the precomputation value and link these invariant to the GlobalConstraintCore
  gc.register(this)
  vehiclesValueAtCheckpoint0 = Array.fill(v)(0)
  currentVehiclesValue = Array.fill(v)(0)
  for(outputVariable <- vehicleToRouteLength)outputVariable.setDefiningInvariant(gc)

  override def performPreCompute(vehicle: Long, routes: IntSequence): Unit = {

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

  override def computeVehicleValue(vehicle: Long,
                                   segments: QList[Segment],
                                   routes: IntSequence): Unit = {
    def digestListOfSegments(segments: QList[Segment], prevNode: Long): Long = {
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

    saveVehicleValue(vehicle, digestListOfSegments(segments,-1))
  }

  override def assignVehicleValue(vehicle: Long): Unit = {
    vehicleToRouteLength(vehicle) := currentVehiclesValue(vehicle)
  }


  override def computeVehicleValueFromScratch(vehicle: Long, routes: IntSequence, save: Boolean = true): Long = {
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

    val result = toReturn + assymetricDistance(previousNode,vehicle) //return
    if(save) saveVehicleValue(vehicle, result)
    result
  }
}
