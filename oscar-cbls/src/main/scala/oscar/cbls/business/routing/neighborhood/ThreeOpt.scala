/**
 * *****************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 * ****************************************************************************
 */

package oscar.cbls.business.routing.neighborhood

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.search.{HotRestart, Pairs}
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.core.computation.CBLSSeqVar
import oscar.cbls.core.search._
import oscar.cbls.lib.search.combinators.{Atomic, ExhaustList}

import scala.collection.immutable.SortedMap

/**
 * This implementation of threeOpt explores the classical threeOpt by aspiration
 * it first iterates on position in routes, then searches for segments to "aspirate" from somewhere else on the routes.
 *
 * @param potentialInsertionNodes the nodes where we might insert a segment.
 *                                Segments are inserted after these positions.
 *                                they must be routed, and can include vehicles
 *                                It expects nodes, not positions in the sequence
 * @param relevantNeighbors for a node, specify here the set of nodes that can define a segment to be inserted afer the node.
 * @param vrp the VRP problem
 * @param neighborhoodName the name for this neighborhood
 * @param selectInsertionPointBehavior first or est for the insertion point
 * @param selectMovedSegmentBehavior among all segments, first or best
 * @param selectFlipBehavior first or best for the flip
 * @param hotRestart hot restart on the insertion point
 * @param skipOnePointMove if set to true, segment will include more than one point.
 * @param breakSymmetry there is a symmetry in the 3-opt
 *                      when moving a segment within the same vehicle without flipping it,
 *                      it is equivalent to moving the nodes between the segment and the insertion position in the other direction
 * @param tryFlip true if flip should be considered, false otherwise.
 */
case class ThreeOpt(potentialInsertionNodes:()=>Iterable[Int], //must be routed, can include vehicles
                    relevantNeighbors:()=>Int=>Iterable[Int], //must be routed, vehicles are filtered away
                    override val vrp: VRP,
                    neighborhoodName:String = "ThreeOpt",
                    selectInsertionPointBehavior:LoopBehavior = First(),
                    selectMovedSegmentBehavior:LoopBehavior = First(),
                    selectFlipBehavior:LoopBehavior = Best(),
                    hotRestart:Boolean = true,
                    skipOnePointMove:Boolean = false,
                    breakSymmetry:Boolean = true,
                    tryFlip:Boolean = true)
  extends AbstractThreeOpt(vrp, neighborhoodName) {

  //the node in the route, for hotRestart
  private var startNodeForHotRestart: Int = 0

  def exploreNeighborhood(initialObj: Long): Unit = {
    val checkpoint = seqVar.defineCurrentValueAsCheckpoint()

    val (iterationSchemeOnZone,notifyFound1) = selectInsertionPointBehavior.toIterable(
      if (hotRestart) HotRestart(potentialInsertionNodes(), startNodeForHotRestart)
      else potentialInsertionNodes())

    def evalObjAndRollBack() : Long = {
      val a = obj.value
      seqVar.rollbackToTopCheckpoint(checkpoint)
      a
    }

    val relevantNeighborsNow = relevantNeighbors()

    val nodeToVehicle = vrp.vehicleOfNode.map(_.value)

    for (insertionNodeTmp <- iterationSchemeOnZone){
      insertionPointForInstantiation = insertionNodeTmp

      checkpoint.explorerAtAnyOccurrence(insertionPointForInstantiation) match{
        case None => //not routed?!
        case Some(explorerAtInsertionPoint) =>
          insertionPointPositionForInstantiation = explorerAtInsertionPoint.position

          toVehicleForInstantiation = nodeToVehicle(insertionPointForInstantiation).toInt

          val relevantNeighbors = relevantNeighborsNow(insertionPointForInstantiation)

          val routedRelevantNeighbors = relevantNeighbors.filter((neighbor : Int) => nodeToVehicle(neighbor) != -1 && neighbor != insertionPointForInstantiation && neighbor >= v)

          val (routedRelevantNeighborsByVehicle,notifyFound2) = selectMovedSegmentBehavior.toIterable(routedRelevantNeighbors.groupBy((i : Int) => nodeToVehicle(i)).toList)

          for((vehicleOfMovedSegment,relevantNodes) <- routedRelevantNeighborsByVehicle if vehicleOfMovedSegment != v /*not sure this is useful since the nodes are routed*/){
            val pairsOfNodesWithPosition = Pairs.makeAllSortedPairs(relevantNodes.map(node => (node,checkpoint.explorerAtAnyOccurrence(node).head)).toList)
            val orderedPairsOfNode = pairsOfNodesWithPosition.map({case (a, b) =>
              if (a._2.position < b._2.position) (a, b) else (b, a)
            })

            fromVehicleForInstantiation = vehicleOfMovedSegment.toInt

            val (relevantPairsToExplore,notifyFound3) = selectMovedSegmentBehavior.toIterable(
              if (skipOnePointMove) orderedPairsOfNode.filter({case (a, b) => a._1 != b._1}) else orderedPairsOfNode)

            for (((segmentStart,explorerAtSegmentStart), (segmentEnd,explorerAtSegmentEnd)) <- relevantPairsToExplore) {

              if (explorerAtInsertionPoint.position < explorerAtSegmentStart.position
                || explorerAtSegmentEnd.position < explorerAtInsertionPoint.position) {
                //moved segment does not include insertion point

                segmentStartPositionForInstantiation = explorerAtSegmentStart.position
                segmentEndPositionForInstantiation = explorerAtSegmentEnd.position

                //skip this if same vehicle, no flip, and to the left

                if(!breakSymmetry
                  || toVehicleForInstantiation != fromVehicleForInstantiation
                  || explorerAtSegmentStart.position < explorerAtInsertionPoint.position){

                  val (flipValuesToTest,notifyFound4) =
                    selectFlipBehavior.toIterable(if(tryFlip) List(false,true) else List(false))

                  for(flipForInstantiationTmp <- flipValuesToTest){
                    flipForInstantiation = flipForInstantiationTmp
                    doMove(insertionPointPositionForInstantiation,
                      segmentStartPositionForInstantiation,
                      segmentEndPositionForInstantiation,
                      flipForInstantiation)

                    if (evaluateCurrentMoveObjTrueIfSomethingFound(evalObjAndRollBack())) {
                      notifyFound1()
                      notifyFound2()
                      notifyFound3()
                      notifyFound4()
                    }
                  }
                }
              }
            }
          }
      }
    }
    seqVar.releaseTopCheckpoint()
    startNodeForHotRestart = insertionPointForInstantiation
    segmentStartPositionForInstantiation = -1
  }

  //this resets the internal state of the Neighborhood
  override def reset(): Unit ={
    startNodeForHotRestart = 0
  }
}

object TreeOpt{
  /**
   * declares a neighborhood that performs a 3-ot on a given vehicle
   * @param myVRP the VRP
   * @param vehicle the statically defined vehicle
   * @param maxSizeOfMovedSegments the max size of the moved segments
   *                               (also the segment from the insertion node to the start of the moved segment)
   * @param maxDistanceOfMove the max distance between the moved segment and the insertion point
   *
   * @return a 3-opt neighborhood
   */
  def threeOptOnVehicle(myVRP:VRP, vehicle:Int,
                        maxSizeOfMovedSegments:Int,
                        maxDistanceOfMove:Int,
                        selectInsertionPointBehavior:LoopBehavior = First(),
                        selectMovedSegmentBehavior:LoopBehavior = First(),
                        hotRestart:Boolean = true,
                        breakSymmetry:Boolean = true,
                        tryFlip:Boolean = true
                       ): Neighborhood = {
    ThreeOptByNodes(
      neighborhoodName = s"3-Opt(v:$vehicle,max:$maxSizeOfMovedSegments)",
      breakSymmetry = breakSymmetry,
      selectInsertionPointBehavior = selectInsertionPointBehavior,
      selectMovedSegmentBehavior = selectMovedSegmentBehavior,
      hotRestart = hotRestart,
      tryFlip = tryFlip,
      potentialInsertionNodes = () => myVRP.getRouteOfVehicle(vehicle),
      relevantMovedSegmentStartNode = () => {
        //(insertionNode,insertionPosition,toVehicle) => segmentStartNodes
        val route:Array[Int] = myVRP.getRouteOfVehicle(vehicle).toArray
        val positionOfEachNodeInArray:SortedMap[Int,Int] =
          SortedMap.empty[Int,Int] ++ route.indices.map(i => (route(i),i))
        (insertionNode:Int,insertionPosition:Int,toVehicle:Int) => {
          val positionInTheArray:Int = positionOfEachNodeInArray(insertionNode)
          val minPos = 1 max positionInTheArray-(maxSizeOfMovedSegments + maxDistanceOfMove)
          val maxPos = (route.length-1) min (positionInTheArray + maxDistanceOfMove)
          (minPos to maxPos).flatMap(i =>
            if (i == positionInTheArray || i == positionInTheArray+1) None
            else Some(route(i))
          )
        }
      },
      relevantMovedSegmentEndNode = () => {
        //(insertionNode,insertionPosition,toVehicle) => (segmentStartNode,segmentStartPosition,fromVehicle) => segmentEndNodes
        val routeWithoutVehicle = myVRP.getRouteOfVehicle(vehicle).filter(_ != vehicle)
        (insertionNode: Int, insertionPosition: Int, toVehicle: Int) => {
          //(segmentStartNode,segmentStartPosition,fromVehicle) => segmentEndNodes
          (segmentStartNode: Int, segmentStartPosition: Int, fromVehicle: Int) => {
            var potentialSegmentEnds: List[Int] = Nil
            var nbCandidateEnd = 0

            var startNodeOfSegmentSeen = false
            var finished = false
            for (node <- routeWithoutVehicle if !finished) {
              if (node == insertionNode) {
                finished = startNodeOfSegmentSeen
              } else if (node == segmentStartNode) {
                startNodeOfSegmentSeen = true
              } else if (startNodeOfSegmentSeen) {
                potentialSegmentEnds = node :: potentialSegmentEnds
                nbCandidateEnd += 1
                if (nbCandidateEnd > maxSizeOfMovedSegments) finished = true
              }
            }
            potentialSegmentEnds.reverse
          }
        }
      },
      vrp = myVRP)
  }

  def threeOptByVehicle(myVRP:VRP, vehicles:Iterable[Int],
                        maxSizeOfMovedSegments:Int,
                        maxDistanceOfMove:Int,
                        selectInsertionPointBehavior:LoopBehavior = First(),
                        selectMovedSegmentBehavior:LoopBehavior = First(),
                        hotRestart:Boolean = true,
                        breakSymmetry:Boolean = true,
                        tryFlip:Boolean = true
                       ): Neighborhood = {
    ExhaustList(
      vehicles.map(vehicle =>
        threeOptOnVehicle(
          myVRP,
          vehicle,
          maxSizeOfMovedSegments,
          maxDistanceOfMove,
          selectInsertionPointBehavior,
          selectMovedSegmentBehavior,
          hotRestart,
          breakSymmetry,
          tryFlip
        )),
      backOnExhaust = true,
    )
  }

  def threeOptReOptimizeByVehicle(myVRP:VRP, vehicles:Iterable[Int],
                                 maxSizeOfMovedSegments:Int,
                                 maxDistanceOfMove:Int,
                                 selectInsertionPointBehavior:LoopBehavior = First(),
                                 selectMovedSegmentBehavior:LoopBehavior = First(),
                                 hotRestart:Boolean = true,
                                 breakSymmetry:Boolean = true,
                                 tryFlip:Boolean = true): Neighborhood = {
    Atomic(ExhaustList(
      vehicles.map(vehicle =>
        Atomic(threeOptOnVehicle(
          myVRP,
          vehicle,
          maxSizeOfMovedSegments,
          maxDistanceOfMove,
          selectInsertionPointBehavior,
          selectMovedSegmentBehavior,
          hotRestart,
          breakSymmetry,
          tryFlip
        ),shouldStop = _ => false) onlyIfUpdateOn(() => myVRP.getRouteOfVehicle(vehicle))),
      backOnExhaust = false,
    ),shouldStop = _ => false)
  }
}

/**
 * This implementation of threeOpt explores the classical threeOpt by aspiration
 * it first iterates on position in routes, then searches for segments to "aspirate" from somewhere else on the routes.
 *
 * @param potentialInsertionNodes the nodes where we might insert a segment.
 *                                Segments are inserted after these positions.
 *                                the potential insertion nodes must be routed, and can include vehicles
 *                                It expects nodes, not positions in the sequence
 * @param relevantMovedSegmentStartNode () => (insertionNode,insertionPosition,toVehicle) => segmentStartNodes
 *                                      They must be routed, but it will not crash if not routed
 * @param relevantMovedSegmentEndNode () => (insertionNode,insertionPosition,toVehicle) => (segmentStartNode,segmentStartPosition,fromVehicle) => segmentEndNodes
 *                                    this node must be after segment start, in the same vehicle and routed, otherwise it is not considered.
 * @param vrp the VRP problem
 * @param neighborhoodName the name for this neighborhood
 * @param selectInsertionPointBehavior first or est for the insertion point
 * @param selectMovedSegmentBehavior among all segments, first or best
 * @param hotRestart hot restart on the insertion point
 * @param breakSymmetry there is a symmetry in the 3-opt
 *                      when moving a segment within the same vehicle without flipping it,
 *                      it is equivalent to moving the nodes between the segment and the insertion position in the other direction
 * @param tryFlip true if flip should be considered, false otherwise.
 */
case class ThreeOptByNodes(potentialInsertionNodes:()=>Iterable[Int], //must be routed, can include vehicles
                           relevantMovedSegmentStartNode:()=>(Int,Int,Int)=>Iterable[Int], //() => (insertionNode,insertionPosition,toVehicle) => segmentStartNodes
                           relevantMovedSegmentEndNode:()=>(Int,Int,Int)=>(Int,Int,Int)=>Iterable[Int], //() => (insertionNode,insertionPosition,toVehicle) => (segmentStartNode,segmentStartPosition,fromVehicle) => segmentEndNodes
                           override val vrp: VRP,
                           neighborhoodName:String = "ThreeOpt",
                           selectInsertionPointBehavior:LoopBehavior = First(),
                           selectMovedSegmentBehavior:LoopBehavior = First(),
                           hotRestart:Boolean = true,
                           breakSymmetry:Boolean = true,
                           tryFlip:Boolean = true)
  extends AbstractThreeOpt(vrp, neighborhoodName) {

  //the node in the route, for hotRestart
  private var startNodeForHotRestart: Int = 0

  def exploreNeighborhood(initialObj: Long): Unit = {
    val checkpoint = seqVar.defineCurrentValueAsCheckpoint()

    val (iterationSchemeOnZone,notifyFound1) = selectInsertionPointBehavior.toIterable(
      if (hotRestart) HotRestart(potentialInsertionNodes(), startNodeForHotRestart)
      else potentialInsertionNodes())

    def evalObjAndRollBack() : Long = {
      val a = obj.value
      seqVar.rollbackToTopCheckpoint(checkpoint)
      a
    }

    val nodeToVehicle = vrp.vehicleOfNode.map(_.value.toInt)
    val positionOfAllNode = vrp.getGlobalRoutePositionOfAllNode
    val relevantMovedSegmentStartNodeNow = relevantMovedSegmentStartNode() //(insertionNode,insertionPosition,toVehicle) => segmentStartNodes
    val relevantMovedSegmentEndNodeNow = relevantMovedSegmentEndNode() //(insertionNode,insertionPosition,toVehicle) => (segmentStartNode,segmentStartPosition,fromVehicle) => segmentEndNodes

    //insertion points
    for (insertionNodeTmp <- iterationSchemeOnZone if positionOfAllNode(insertionNodeTmp) < vrp.n) {
      insertionPointForInstantiation = insertionNodeTmp
      insertionPointPositionForInstantiation = positionOfAllNode(insertionNodeTmp)
      toVehicleForInstantiation = nodeToVehicle(insertionPointForInstantiation)

      val relevantSegmentStartsNodes = relevantMovedSegmentStartNodeNow(insertionPointForInstantiation,insertionPointPositionForInstantiation,toVehicleForInstantiation)
      val relevantMovedSegmentEndNodeNow1 = //(segmentStartNode,segmentStartPosition,fromVehicle) => segmentEndNodes
        relevantMovedSegmentEndNodeNow(insertionPointForInstantiation,insertionPointPositionForInstantiation,toVehicleForInstantiation)


      val routedRelevantSegmentStartNodes = relevantSegmentStartsNodes.filter((node: Int) => nodeToVehicle(node) != -1 && node != insertionPointForInstantiation && node >= v)

      //segment start
      val (routedRelevantNeighborsByVehicle, notifyFound2) = selectMovedSegmentBehavior.toIterable(routedRelevantSegmentStartNodes)
      for (relevantSegmentStart <- routedRelevantNeighborsByVehicle if positionOfAllNode(relevantSegmentStart) < vrp.n) {

        fromVehicleForInstantiation = nodeToVehicle(relevantSegmentStart)

        segmentStartPositionForInstantiation = positionOfAllNode(relevantSegmentStart)

        val relevantSegmentEndNodes =
          relevantMovedSegmentEndNodeNow1(
            relevantSegmentStart, segmentStartPositionForInstantiation, fromVehicleForInstantiation)

        //segment end
        val (segmentEndIt, notifyFound3) = selectMovedSegmentBehavior.toIterable(relevantSegmentEndNodes)
        for (segmentEndNode <- segmentEndIt
             if (positionOfAllNode(segmentEndNode) < vrp.n
               && fromVehicleForInstantiation == nodeToVehicle(segmentEndNode)
               && positionOfAllNode(relevantSegmentStart) < positionOfAllNode(segmentEndNode)
               && (positionOfAllNode(insertionNodeTmp) < positionOfAllNode(relevantSegmentStart) ||
               positionOfAllNode(segmentEndNode) < positionOfAllNode(insertionNodeTmp)))) {

          segmentEndPositionForInstantiation = positionOfAllNode(segmentEndNode)

          //flip
          if (!breakSymmetry
            || toVehicleForInstantiation != fromVehicleForInstantiation
            || insertionPointPositionForInstantiation < segmentEndPositionForInstantiation) {

            for (flipForInstantiationTmp <- if (tryFlip) List(false, true) else List(false)) {
              flipForInstantiation = flipForInstantiationTmp

              doMove(insertionPointPositionForInstantiation,
                segmentStartPositionForInstantiation,
                segmentEndPositionForInstantiation,
                flipForInstantiation)

              if (evaluateCurrentMoveObjTrueIfSomethingFound(evalObjAndRollBack())) {
                notifyFound1()
                notifyFound2()
                notifyFound3()
              }
            }
          }
        }
      }
    }
    seqVar.releaseTopCheckpoint()
    startNodeForHotRestart = insertionPointForInstantiation
    segmentStartPositionForInstantiation = -1
  }

  //this resets the internal state of the Neighborhood
  override def reset(): Unit ={
    startNodeForHotRestart = 0
  }
}

abstract class AbstractThreeOpt(val vrp: VRP,
                                neighborhoodName:String = "ThreeOpt")
  extends EasyNeighborhoodMultiLevel[ThreeOptMove](neighborhoodName) {

  val v: Int = vrp.v
  val seqVar: CBLSSeqVar = vrp.routes

  var segmentStartPositionForInstantiation:Int = -1
  var segmentEndPositionForInstantiation:Int = -1
  var insertionPointPositionForInstantiation:Int = -1
  var insertionPointForInstantiation:Int = -1
  var flipForInstantiation:Boolean = false

  var fromVehicleForInstantiation:Int = -1
  var toVehicleForInstantiation:Int = -1

  def doMove(insertionPosition: Int, segmentStartPosition: Int, segmentEndPosition: Int, flip: Boolean): Unit ={
    seqVar.move(segmentStartPosition,segmentEndPosition,insertionPosition,flip)
  }

  override def instantiateCurrentMove(newObj: Long): ThreeOptMove =
    ThreeOptMove(segmentStartPositionForInstantiation,
      segmentEndPositionForInstantiation,
      insertionPointPositionForInstantiation,
      insertionPointForInstantiation,
      flipForInstantiation,
      fromVehicleForInstantiation,
      toVehicleForInstantiation,
      newObj,
      this,
      neighborhoodName)

}

case class ThreeOptMove(segmentStartPosition:Int,
                        segmentEndPosition:Int,
                        insertionPointPosition: Int,
                        insertionPoint:Int,
                        flipSegment: Boolean,
                        fromVehicle:Int,
                        toVehicle:Int,
                        override val objAfter: Long,
                        override val neighborhood:AbstractThreeOpt,
                        override val neighborhoodName:String = "ThreeOptMove")
  extends VRPSMove(objAfter, neighborhood, neighborhoodName,neighborhood.vrp){

  override def impactedPoints: Iterable[Int] = QList(insertionPoint,neighborhood.vrp.routes.value.valuesBetweenPositionsQList(segmentStartPosition,segmentEndPosition))

  // overriding methods
  override def commit(): Unit ={
    neighborhood.doMove(insertionPointPosition, segmentStartPosition, segmentEndPosition, flipSegment)
  }

  override def toString: String =
    s"${neighborhoodNameToString}TreeOpt(segmentStartPosition:$segmentStartPosition segmentEndPosition:$segmentEndPosition insertionPointPosition:$insertionPointPosition insertionPoint:$insertionPoint${if(flipSegment) " flip" else " noFlip"}$objToString)"
}
