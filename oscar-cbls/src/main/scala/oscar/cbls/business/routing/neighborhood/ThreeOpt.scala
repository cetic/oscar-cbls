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

/**
 * This implementation of threeOpt explores the classical threeOpt by aspiration
 * it first iterates on position in routes, then searches for segments to "aspirate" from somewhere else on the routes.
 *
 * @param potentialInsertionNodes the nodes where we might insert a segment.
 *                                Segments are inserted after these positions.
 *                                they must be routed, and can include vehicles
 *                                It expects nodes, not positions in the sequence
 * @param relevantNeighbors for a node, you must specify the
 * @param vrp the VRP problem
 * @param neighborhoodName the name for this neighborhood
 * @param selectInsertionPointBehavior first or est for the insertion point
 * @param selectMovedSegmentBehavior among all segments, first or best
 * @param selectFlipBehavior first or best for the flip
 * @param hotRestart hot restart on the insertion point
 * @param skipOnePointMove if set to true, segment will include more than one point.
 * @param breakSymmetry there is a symmetry in the 3-opt
 *                      when moving a segment within the same vehicle without flipping it,
 *                      it is equivalent to moving hte nodes between the segment and the insertion position in the other direction
 * @param tryFlip true if flip should be considered, false otherwise.
 */
case class ThreeOpt(potentialInsertionNodes:()=>Iterable[Int], //must be routed, can include vehicles
                    relevantNeighbors:()=>Int=>Iterable[Int], //must be routed, vehicles are filtered away
                    vrp: VRP,
                    neighborhoodName:String = "ThreeOpt",
                    selectInsertionPointBehavior:LoopBehavior = First(),
                    selectMovedSegmentBehavior:LoopBehavior = First(),
                    selectFlipBehavior:LoopBehavior = Best(),
                    hotRestart:Boolean = true,
                    skipOnePointMove:Boolean = false,
                    breakSymmetry:Boolean = true,
                    tryFlip:Boolean = true)
  extends EasyNeighborhoodMultiLevel[ThreeOptMove](neighborhoodName) {

  //the node in the route, for hotRestart
  private var startNodeForHotRestart: Int = 0

  val v: Int = vrp.v
  val seqVar: CBLSSeqVar = vrp.routes

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

          val vehicleForInsertion = nodeToVehicle(insertionPointForInstantiation)

          val relevantNeighbors = relevantNeighborsNow(insertionPointForInstantiation)

          val routedRelevantNeighbors = relevantNeighbors.filter((neighbor : Int) => nodeToVehicle(neighbor) != -1 && neighbor != insertionPointForInstantiation && neighbor >= v)

          val (routedRelevantNeighborsByVehicle,notifyFound2) = selectMovedSegmentBehavior.toIterable(routedRelevantNeighbors.groupBy((i : Int) => nodeToVehicle(i)).toList)

          for((vehicleOfMovedSegment,relevantNodes) <- routedRelevantNeighborsByVehicle if vehicleOfMovedSegment != v /*not sure this is useful since the nodes are routed*/){
            val pairsOfNodesWithPosition = Pairs.makeAllSortedPairs(relevantNodes.map(node => (node,checkpoint.explorerAtAnyOccurrence(node).head)).toList)
            val orderedPairsOfNode = pairsOfNodesWithPosition.map({case (a, b) =>
              if (a._2.position < b._2.position) (a, b) else (b, a)
            })

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
                  || vehicleForInsertion != vehicleOfMovedSegment
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

  var segmentStartPositionForInstantiation:Int = -1
  var segmentEndPositionForInstantiation:Int = -1
  var insertionPointPositionForInstantiation:Int = -1
  var insertionPointForInstantiation:Int = -1
  var flipForInstantiation:Boolean = false

  override def instantiateCurrentMove(newObj: Long): ThreeOptMove =
    ThreeOptMove(segmentStartPositionForInstantiation,
      segmentEndPositionForInstantiation,
      insertionPointPositionForInstantiation,
      insertionPointForInstantiation,
      flipForInstantiation,
      newObj,
      this,
      neighborhoodName)

  //this resets the internal state of the Neighborhood
  override def reset(): Unit ={
    startNodeForHotRestart = 0
  }

  def doMove(insertionPosition: Int, segmentStartPosition: Int, segmentEndPosition: Int, flip: Boolean): Unit ={
    seqVar.move(segmentStartPosition,segmentEndPosition,insertionPosition,flip)
  }
}

case class ThreeOptMove(segmentStartPosition:Int,
                        segmentEndPosition:Int,
                        insertionPointPosition: Int,
                        insertionPoint:Int,
                        flipSegment: Boolean,
                        override val objAfter: Long,
                        override val neighborhood:ThreeOpt,
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
