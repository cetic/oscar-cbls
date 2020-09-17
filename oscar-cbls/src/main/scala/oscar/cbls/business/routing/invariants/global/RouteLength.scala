package oscar.cbls.business.routing.invariants.global

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.CBLSIntVar

case class PreComputedDistances(distanceFromStart:Long,
                                distanceToStart:Long)

object RouteLength{
  def apply(gc: GlobalConstraintCore, n: Int, v:Int, distanceMatrix:(Int,Int)=>Long):Array[CBLSIntVar] = {
    val routeLengthForVehicle:Array[CBLSIntVar] =
      Array.tabulate(v)(v => CBLSIntVar(gc.model,name=s"routeLengthForVehicle$v"))

    new RouteLength(gc: GlobalConstraintCore, n: Int, v:Int, routeLengthForVehicle, distanceMatrix)
    routeLengthForVehicle
  }
}

/**
 * @param gc The GlobalConstraintCore you want to associate this constraint to
 * @param n then umber of nodes
 * @param v The number of vehicle
 * @param vehicleToRouteLength the output of the constraint: for each vehicle, the length of the route
 * @param distanceMatrix the distance from each node to each node; it can be asymmetrical
 */
class RouteLength(gc: GlobalConstraintCore, n: Int, v:Int, vehicleToRouteLength:Array[CBLSIntVar], distanceMatrix:(Int,Int)=>Long)
  extends GlobalConstraintDefinition[Long](gc,v){

  val preComputedVals: Array[PreComputedDistances] = Array.fill(n)(PreComputedDistances(0,0))

  // Initialize the vehicles value, the precomputation value and link these invariant to the GlobalConstraintCore
  gc.register(this)
  for(outputVariable <- vehicleToRouteLength)outputVariable.setDefiningInvariant(gc)

  override def performPreCompute(vehicle: Int, routes: IntSequence): Unit = {
    var previousNode = vehicle
    var prevPreComputedValue = PreComputedDistances(0,0)
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
          //We tag the current node with the proper value accumulation on the previous node

          prevPreComputedValue = PreComputedDistances(
            distanceFromStart = prevPreComputedValue.distanceFromStart + distanceMatrix(previousNode,explorer.value),
            distanceToStart = prevPreComputedValue.distanceToStart + distanceMatrix(explorer.value,previousNode))

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
          distanceMatrix(prevNode,vehicle)
        case segmentQList =>
          val head = segmentQList.head
          val tail = segmentQList.tail
          head match {
            case PreComputedSubSequence(startNode, endNode, _) =>
              val distanceToEnterThisSegment = if (prevNode == -1) 0 else distanceMatrix(prevNode,startNode)
              val lengthOfThisSegment = preComputedVals(endNode).distanceFromStart - preComputedVals(startNode).distanceFromStart
              require(lengthOfThisSegment >= 0)
              distanceToEnterThisSegment + lengthOfThisSegment + digestListOfSegments(tail, endNode)

            case FlippedPreComputedSubSequence(startNode, endNode, _) =>
              val distanceToEnterThisSegment = if (prevNode == -1) 0 else distanceMatrix(prevNode,startNode)
              val lengthOfThisSegment = preComputedVals(startNode).distanceToStart - preComputedVals(endNode).distanceToStart
              require(lengthOfThisSegment >= 0)
              distanceToEnterThisSegment + lengthOfThisSegment + digestListOfSegments(tail, endNode)

            case NewNode(node) =>
              val distanceToEnterThisSegment = if (prevNode == -1) 0 else distanceMatrix(prevNode,node)
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
          toReturn += distanceMatrix(previousNode,explorer.value)
          previousNode = explorer.value
          currentExplorerOPt = explorer.next
          true
        }
    }){}

    toReturn + distanceMatrix(previousNode,vehicle) //return
  }
}
