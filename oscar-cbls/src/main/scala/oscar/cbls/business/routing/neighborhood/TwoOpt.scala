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
 *     This code has been initially developed by Ghilain Florent.
 *     Refactored with respect to the new architecture by Yoann Guyot.
 * ****************************************************************************
 */

package oscar.cbls.business.routing.neighborhood

import oscar.cbls.algo.search.HotRestart
import oscar.cbls.business.routing.model.{VRP, VehicleLocation}
import oscar.cbls.core.search.{EasyNeighborhoodMultiLevel, First, LoopBehavior}
import oscar.cbls._

/**
 * Removes two edges of routes, and rebuilds routes from the segments.
 * (with one reverse required)
 *
 * The search complexity is O(n²).
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 * */
case class TwoOpt(segmentStartValues:()=>Iterable[Long],
                  relevantNewSuccessors:()=>Long=>Iterable[Long],
                  vrp: VRP,
                  neighborhoodName:String = "TwoOpt",
                  selectSegmentStartBehavior:LoopBehavior = First(),
                  selectSegmentEndBehavior:LoopBehavior = First(),
                  hotRestart:Boolean = true)
  extends EasyNeighborhoodMultiLevel[TwoOptMove](neighborhoodName){

  val v = vrp.v
  val seq = vrp.routes

  def doMove(fromPositionIncluded:Long,toPositionIncluded:Long) {
    seq.flip(fromPositionIncluded,toPositionIncluded)
  }

  var segmentStartPositionForInstantiate:Long = -1L
  var segmentEndPositionForInstantiate:Long = -1L
  var segmentStartValue = -1L

  override def instantiateCurrentMove(newObj: Long) =
    TwoOptMove(segmentStartPositionForInstantiate, segmentEndPositionForInstantiate, newObj, this, vrp, neighborhoodName)


  //the indice to start with for the exploration
  var startIndice: Long = 0

  /**
   * Removes two edges of a route and flips the obtained segment before
   * reconnecting it.
   * The search complexity is O(n²).
   */
  override def exploreNeighborhood(initialObj: Long): Unit = {

    val seqValue = seq.defineCurrentValueAsCheckpoint(true)

    def evalObjAndRollBack() : Long = {
      val a = obj.value
      seq.rollbackToTopCheckpoint(seqValue)
      a
    }

    val (iterationSchemeOnZone,notifyFound1) =
      selectSegmentStartBehavior.toIterable(
        if (hotRestart) HotRestart(segmentStartValues(), startIndice)
        else segmentStartValues())

    val relevantNeighborsNow = relevantNewSuccessors()

    val nodesToVehicle = vrp.vehicleOfNode.map(_.value)

    val vehicleSearcher = VehicleLocation(v,seqValue.positionOfAnyOccurrence(_).get)

    for (segmentStartValueTmp <- iterationSchemeOnZone if segmentStartValueTmp >= v) {
      segmentStartValue = segmentStartValueTmp

      assert(vrp.isRouted(segmentStartValue),
        "The search zone should be restricted to routed.")

      val segmentStartPositionExplorer = seqValue.explorerAtAnyOccurrence(segmentStartValue).head
      val segmentStartPosition = segmentStartPositionExplorer.position
      val predecessorOfSegmentStartValue = segmentStartPositionExplorer.prev.head.value

      val vehicleReachingSegmentStart = vehicleSearcher.vehicleReachingPosition(segmentStartPosition)
      val (segmentEndIterable,notifyFound2) = selectSegmentEndBehavior.toIterable(relevantNeighborsNow(predecessorOfSegmentStartValue))

      for (
        segmentEndValue <- segmentEndIterable
        if (segmentEndValue >= v
          && nodesToVehicle(segmentEndValue) == vehicleReachingSegmentStart)
      ) {

        val segmentEndPosition = seqValue.positionOfAnyOccurrence(segmentEndValue).head

        if(segmentEndPosition > segmentStartPosition) {

          segmentStartPositionForInstantiate = segmentStartPosition
          segmentEndPositionForInstantiate = segmentEndPosition

          doMove(segmentStartPosition, segmentEndPosition)

          if (evaluateCurrentMoveObjTrueIfSomethingFound(evalObjAndRollBack())) {
            notifyFound1()
            notifyFound2()
          }
        }
      }
    }

    seq.releaseTopCheckpoint()
    segmentStartPositionForInstantiate = -1L
    startIndice = segmentStartValue + 1L
  }

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0L
  }
}


/**
 * Models a two-opt-move operator of a given VRP problem.
 * @param objAfter the objective value if we performed this two-opt-move operator.
 * @param neighborhood the originating neighborhood
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 * */
case class TwoOptMove(segmentStartPosition:Long,
                      segmentEndPosition:Long,
                      override val objAfter: Long,
                      override val neighborhood:TwoOpt,
                      vrp:VRP,
                      override val neighborhoodName:String = "TwoOptMove")
  extends VRPSMove(objAfter, neighborhood, neighborhoodName, vrp){

  override def impactedPoints: Iterable[Long] = vrp.routes.value.valuesBetweenPositionsQList(segmentStartPosition,segmentEndPosition)

  override def commit() {
    neighborhood.doMove(segmentStartPosition, segmentEndPosition)
  }

  override def toString: String = (neighborhoodNameToString + "TwoOpt(segmentStartPosition:"
    + segmentStartPosition
    + "; segmentEndPosition:" + segmentEndPosition + objToString + ")")
}
