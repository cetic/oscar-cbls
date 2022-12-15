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
import oscar.cbls.core.search.{EasyNeighborhoodMultiLevel, First, LoopBehavior, Neighborhood}
import oscar.cbls.lib.search.combinators.{Atomic, AtomicDyn, ExhaustList}

import scala.collection.immutable.SortedMap

/**
 * Removes two edges of routes, and rebuilds routes from the segments.
 * (with one reverse required)
 *
 * The search complexity is O(n²).
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 * */
case class TwoOpt(segmentStartValues:()=>Iterable[Int],
                  relevantNewSuccessors:()=>Int=>Iterable[Int],
                  vrp: VRP,
                  neighborhoodName:String = "TwoOpt",
                  selectSegmentStartBehavior:LoopBehavior = First(),
                  selectSegmentEndBehavior:LoopBehavior = First(),
                  hotRestart:Boolean = true)
  extends EasyNeighborhoodMultiLevel[TwoOptMove](neighborhoodName){

  val v = vrp.v
  val seq = vrp.routes

  def doMove(fromPositionIncluded:Int,toPositionIncluded:Int): Unit ={
    seq.flip(fromPositionIncluded,toPositionIncluded)
  }

  var segmentStartPositionForInstantiate:Int = -1
  var segmentEndPositionForInstantiate:Int = -1
  var segmentStartValue = -1

  override def instantiateCurrentMove(newObj: Long): TwoOptMove =
    TwoOptMove(segmentStartPositionForInstantiate, segmentEndPositionForInstantiate, newObj, this, vrp, neighborhoodName)

  //the indice to start with for the exploration
  var startIndice: Int = 0

  /**
   * Removes two edges of a route and flips the obtained segment before
   * reconnecting it.
   * The search complexity is O(n²).
   */
  override def exploreNeighborhood(initialObj: Long): Unit = {

    val seqValue = seq.defineCurrentValueAsCheckpoint()

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
    segmentStartPositionForInstantiate = -1
    startIndice = segmentStartValue + 1
  }

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}

object TwoOpt {
  def twoOptOnVehicle(vehicle: Int,
                      vrp: VRP,
                      selectSegmentStartBehavior:LoopBehavior = First(),
                      selectSegmentEndBehavior:LoopBehavior = First()): Neighborhood = {
    TwoOpt(
      segmentStartValues = () => {
        vrp.getRouteOfVehicle(vehicle).tail
      },
      relevantNewSuccessors = () => {
        val routeOfVehicle = vrp.getRouteOfVehicle(vehicle).toArray
        val positionOfEachNodeInArray: SortedMap[Int, Int] =
          SortedMap.empty[Int, Int] ++ routeOfVehicle.indices.map(i => (routeOfVehicle(i), i))
        (segmentStartNode: Int) => {
          val minPos = positionOfEachNodeInArray(segmentStartNode) + 1
          (minPos until routeOfVehicle.length).map(pos => routeOfVehicle(pos))
        }
      },
      vrp = vrp,
      selectSegmentStartBehavior = selectSegmentStartBehavior,
      selectSegmentEndBehavior = selectSegmentEndBehavior,
      neighborhoodName = s"twoOpt(v:$vehicle)")
  }

  def reOptimizeVehicleLateAcceptance(vehicle: Int, vrp: VRP): Neighborhood = {
    AtomicDyn(() =>
      twoOptOnVehicle(vehicle, vrp,
        selectSegmentStartBehavior = First(randomized=true),
        selectSegmentEndBehavior = First(randomized=true))
        lateAcceptanceHillClimbing(10, maxObj = obj => obj + (obj*0.5/vrp.v).toLong),
      shouldStop = _ => false,
      aggregateIntoSingleMove = true) onlyIfUpdateOn (() => vrp.getRouteOfVehicle(vehicle))
  }

  def reOptimizeVehiclesLateAcceptance(vrp: VRP, vehicles: Iterable[Int]): Neighborhood = {
    Atomic(ExhaustList(
      vehicles.map(vehicle => reOptimizeVehicleLateAcceptance(vehicle, vrp))),
      shouldStop = _ => false, aggregateIntoSingleMove = true)
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
case class TwoOptMove(segmentStartPosition:Int,
                      segmentEndPosition:Int,
                      override val objAfter: Long,
                      override val neighborhood:TwoOpt,
                      vrp:VRP,
                      override val neighborhoodName:String = "TwoOptMove")
  extends VRPSMove(objAfter, neighborhood, neighborhoodName, vrp){

  override def impactedPoints: Iterable[Int] = vrp.routes.value.valuesBetweenPositionsQList(segmentStartPosition,segmentEndPosition)

  override def commit(): Unit = {
    neighborhood.doMove(segmentStartPosition, segmentEndPosition)
  }

  override def toString: String =
    s"${neighborhoodNameToString}TwoOpt(segmentStartPosition:$segmentStartPosition; segmentEndPosition:$segmentEndPosition$objToString)"
}
