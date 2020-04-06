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

/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer and Florent Ghilain.
 *
 *     Refactored with respect to the new architecture by Yoann Guyot
 * ****************************************************************************
 */


package oscar.cbls.business.routing.neighborhood

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.search.{HotRestart, Pairs}
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.core.search._
import oscar.cbls._


/**
 * Removes three edges of routes, and rebuilds routes from the segments.
 * Finds 3L candidate points for a 3L-opt move, and then
 * chooses on-the-fly between simple 3L-opt move and reverse 3L-opt move.
 *
 * Info : it also could be saw as the move of a route's segment to another place.
 * The search complexity is O(n³).
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 */
case class ThreeOpt(potentialInsertionPoints:()=>Iterable[Int], //must be routed
                    relevantNeighbors:()=>Int=>Iterable[Int], //must be routed
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

  //the indice to start with for the exploration
  var startIndice: Int = 0

  val v = vrp.v
  val seq = vrp.routes

  def exploreNeighborhood(initialObj: Long): Unit = {
    val seqValue = seq.defineCurrentValueAsCheckpoint(true)

    val (iterationSchemeOnZone,notifyFound1) = selectInsertionPointBehavior.toIterable(
      if (hotRestart) HotRestart(potentialInsertionPoints(), startIndice)
      else potentialInsertionPoints())

    def evalObjAndRollBack() : Long = {
      val a = obj.value
      seq.rollbackToTopCheckpoint(seqValue)
      a
    }

    val relevantNeighborsNow = relevantNeighbors()

    val nodeToVehicle = vrp.vehicleOfNode.map(_.value)

    var insertionPoint = -1
    for (insertionPointTmp <- iterationSchemeOnZone){
      insertionPoint = insertionPointTmp

      seqValue.positionOfAnyOccurrence(insertionPoint) match{
        case None => //not routed?!
        case Some(insertionPosition) =>

          val vehicleForInsertion = nodeToVehicle(insertionPoint)

          val relevantNeighbors = relevantNeighborsNow(insertionPoint)
          val routedRelevantNeighbors = relevantNeighbors.filter((neighbor : Int) => nodeToVehicle(neighbor) != -1L && neighbor != insertionPoint && neighbor > v)

          val (routedRelevantNeighborsByVehicle,notifyFound2) = selectMovedSegmentBehavior.toIterable(routedRelevantNeighbors.groupBy((i : Int) => nodeToVehicle(i)).toList)

          for((vehicleOfMovedSegment,relevantNodes) <- routedRelevantNeighborsByVehicle if vehicleOfMovedSegment != v){
            val pairsOfNodesWithPosition = Pairs.makeAllSortedPairs(relevantNodes.map(node => (node,seqValue.positionOfAnyOccurrence(node).head)).toList)
            val orderedPairsOfNode = pairsOfNodesWithPosition.map({case (a, b) => if (a._2 < b._2) (a, b) else (b, a)})

            val (relevantPairsToExplore,notifyFound3) =
              selectMovedSegmentBehavior.toIterable(
                if (skipOnePointMove) orderedPairsOfNode.filter({case (a, b) => a._1 != b._1})
                else orderedPairsOfNode)

            for (((segmentStart,segmentStartPosition), (segmentEnd,segmentEndPosition)) <- relevantPairsToExplore) {

              if (insertionPosition < segmentStartPosition || segmentEndPosition < insertionPosition) {

                segmentStartPositionForInstantiation = segmentStartPosition
                segmentEndPositionForInstantiation = segmentEndPosition
                insertionPointPositionForInstantiation = insertionPosition
                insertionPointForInstantiation = insertionPoint

                //skip this if same vehicle, no flip, and to the left

                if(!breakSymmetry || vehicleForInsertion != vehicleOfMovedSegment || insertionPosition > segmentStartPosition){

                  val (flipValuesToTest,notifyFound4) =
                    selectFlipBehavior.toIterable(if(tryFlip) List(false,true) else List(false))


                  for(flipForInstantiationTmp <- flipValuesToTest){
                    flipForInstantiation = flipForInstantiationTmp
                    doMove(insertionPosition, segmentStartPosition, segmentEndPosition, flipForInstantiation)

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
    seq.releaseTopCheckpoint()
    startIndice = insertionPoint + 1
    segmentStartPositionForInstantiation = -1
  }

  var segmentStartPositionForInstantiation:Int = -1
  var segmentEndPositionForInstantiation:Int = -1
  var insertionPointPositionForInstantiation:Int = -1
  var insertionPointForInstantiation:Int = -1
  var flipForInstantiation:Boolean = false

  override def instantiateCurrentMove(newObj: Long) =
    ThreeOptMove(segmentStartPositionForInstantiation,
      segmentEndPositionForInstantiation,
      insertionPointPositionForInstantiation,
      insertionPointForInstantiation,
      flipForInstantiation,
      newObj,
      this,
      neighborhoodName)

  //this resets the internal state of the Neighborhood
  override def reset(){
    startIndice = 0
  }

  def doMove(insertionPosition: Int, segmentStartPosition: Int, segmentEndPosition: Int, flip: Boolean) {
    seq.move(segmentStartPosition,segmentEndPosition,insertionPosition,flip)
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
  override def commit() {
    neighborhood.doMove(insertionPointPosition, segmentStartPosition, segmentEndPosition, flipSegment)
  }

  override def toString: String =
    neighborhoodNameToString + "TreeOpt(segmentStartPosition:" + segmentStartPosition +
      " segmentEndPosition:" + segmentEndPosition +
      " insertionPointPosition:" + insertionPointPosition +
      " insertionPoint:" + insertionPoint +
      (if(flipSegment) " flip" else " noFlip") + objToString + ")"
}
