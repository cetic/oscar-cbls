/*******************************************************************************
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
  ******************************************************************************/

package oscar.cbls.business.routing.modeling

import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.neighborhood._
import oscar.cbls.core.search.{Best, First, LoopBehavior}

/**
 * Created by rdl on 11L-09L-1L7.
 */
trait RoutingNeighborhoods
  extends InsertPointAPI
  with OnePointMoveAPI
  with RemovePointAPI
  with RouteExchangeAPI
  with SegmentExchangeAPI
  with ThreeOptAPI
  with TwoOptAPI

trait InsertPointAPI{
  type InsertPointMove = oscar.cbls.business.routing.neighborhood.InsertPointMove
  val InsertPointMove = oscar.cbls.business.routing.neighborhood.InsertPointMove

  /**
   * OnePoint insert neighborhood that primarily iterates over insertion point,s and then over poitns that can be iserted.
   * @param insertionPoints the positions where we can insert points, can be unrouted, in this case it is ignored (but time is wasted)
   * @param relevantSuccessorsToInsert the points to insert, given an insertion point
   * @param vrp the routing problem
   * @param neighborhoodName the name of the neighborhood
   * @param selectInsertionPointBehavior specifies how the insertion point should be selected
   * @param selectInsertedNodeBehavior specifies how the inserted point should be selected
   * @param hotRestart hot restart on the insertion point
   * @param insertedPointsSymetryClass a function that input the ID of an unrouted node and returns a symmetry class;
   *                      ony one of the unrouted node in each class will be considered for insert
   *                      Int.MinValue is considered different to itself
   *                      if you set to None this will not be used at all
   * @author renaud.delandtsheer@cetic.be
   */
  def insertPointRoutedFirst(insertionPoints:()=>Iterable[Int],
                             relevantSuccessorsToInsert: () => Int => Iterable[Int],
                             vrp: VRP,
                             neighborhoodName: String = "InsertPointRoutedFirst",
                             selectInsertionPointBehavior:LoopBehavior = First(),
                             selectInsertedNodeBehavior:LoopBehavior = First(),
                             hotRestart: Boolean = true,
                             insertedPointsSymetryClass:Option[Int => Int] = None) =
    InsertPointRoutedFirst(insertionPoints,
      relevantSuccessorsToInsert,
      vrp,
      neighborhoodName,
      selectInsertionPointBehavior,
      selectInsertedNodeBehavior,
      hotRestart,
      insertedPointsSymetryClass)

  /**
   * Inserts an unrouted point in a route. The size of the neighborhood is O(u*n).
   * where u is the numberof unrouted points, and n is the number of routed points
   * it can be cut down to u*k by using the relevant neighbors, and specifying k neighbors for each unrouted point
   * @param unroutedNodesToInsert the nodes that this neighborhood will try to insert SHOULD BE NOT ROUTED
   * @param relevantPredecessor a function that, for each unrouted node gives a list of routed node
   *                          such that it is relevant to insert the unrouted node after this routed node
   * @param vrp the routing problem
   * @param neighborhoodName the name of this neighborhood
   * @param selectNodeBehavior how should it iterate on nodes to insert?
   * @param selectInsertionPointBehavior how should it iterate on position for insertion?
   * @param hotRestart set to true fo a hot restart fearture on the node to insert
   * @param nodeSymmetryClass a function that input the ID of an unrouted node and returns a symmetry class;
   *                      ony one of the unrouted node in each class will be considered for insert
   *                      Int.MinValue is considered different to itself
   *                      if you set to None this will not be used at all
   * @param hotRestartOnNextSymmetryClass when you have symmetries among points to insert and hotRestart,
   *                                  this option will try to have the hotRestart starting
   *                                  at a different symmetry class than the last one.
   * @author renaud.delandtsheer@cetic.be
   * @author Florent Ghilain (UMONS)
   * @author yoann.guyot@cetic.be
   */
  def insertPointUnroutedFirst(unroutedNodesToInsert: () => Iterable[Int],
                               relevantPredecessor: () => Int => Iterable[Int],
                               vrp: VRP,
                               neighborhoodName: String = "InsertPointUnroutedFirst",
                               hotRestart: Boolean = true,
                               selectNodeBehavior:LoopBehavior = First(),
                               selectInsertionPointBehavior:LoopBehavior = First(),
                               nodeSymmetryClass:Option[Int => Int] = None,
                               hotRestartOnNextSymmetryClass:Boolean = false,
                               positionIndependentMoves:Boolean = false) =
    InsertPointUnroutedFirst(unroutedNodesToInsert,
      relevantPredecessor,
      vrp,
      neighborhoodName,
      hotRestart,
      selectNodeBehavior,
      selectInsertionPointBehavior,
      nodeSymmetryClass,
      hotRestartOnNextSymmetryClass,
      positionIndependentMoves)
}


trait OnePointMoveAPI{

  type OnePointMoveMove = oscar.cbls.business.routing.neighborhood.OnePointMoveMove
  val OnePointMoveMove = oscar.cbls.business.routing.neighborhood.OnePointMoveMove

  /**
   * Moves a point of a route to another place in the same or in an other route.
   * The search complexity is O(nk).
   * @author renaud.delandtsheer@cetic.be
   * @author yoann.guyot@cetic.be
   * @author Florent Ghilain (UMONS)
   */
  def onePointMove(nodesToMove: () => Iterable[Int],
                   relevantNewPredecessors: () => Int => Iterable[Int],
                   vrp:VRP,
                   neighborhoodName: String = "OnePointMove",
                   selectPointToMoveBehavior:LoopBehavior = First(),
                   selectDestinationBehavior:LoopBehavior = First(),
                   hotRestart: Boolean = true,
                   allPointsToMoveAreRouted:Boolean = true,
                   allRelevantNeighborsAreRouted:Boolean = true,
                   positionIndependentMoves:Boolean = false) =
    OnePointMove(nodesToMove,
      relevantNewPredecessors,
      vrp,
      neighborhoodName,
      selectPointToMoveBehavior,
      selectDestinationBehavior,
      hotRestart,
      allPointsToMoveAreRouted,
      allRelevantNeighborsAreRouted,
      positionIndependentMoves)

}


trait RemovePointAPI{

  type RemovePointMove = oscar.cbls.business.routing.neighborhood.RemovePointMove

  /**
   * Removes a point of route.
   * The search complexity is O(n).
   * @param relevantPointsToRemove: the predecessors of the points that we will try to remove
   * @param vrp the routing problem
   * @param neighborhoodName the name of the neighborhood, for verbosities
   * @param selectNodeBehavior how to select node to remove
   * @param hotRestart true if hotRestart is needed, false otherwise
   * @author renaud.delandtsheer@cetic.be
   * @author yoann.guyot@cetic.be
   * @author Florent Ghilain (UMONS)
   */
  def removePoint(relevantPointsToRemove:()=>Iterable[Int],
                  vrp: VRP,
                  neighborhoodName:String = "RemovePoint",
                  selectNodeBehavior:LoopBehavior = First(),
                  hotRestart:Boolean = true,
                  positionIndependentMoves:Boolean = false) =
    RemovePoint(
      relevantPointsToRemove,
      vrp,
      neighborhoodName,
      selectNodeBehavior,
      hotRestart,
      positionIndependentMoves)

}


trait RouteExchangeAPI{
  type RouteExchangeMove= oscar.cbls.business.routing.neighborhood.RouteExchangeMove

  /**
   * a neighborhood that exchanges the route of two vehicles.
   * It does not modifies the routes themselves. It just exchanges the vehicles
   * @author renaud.delandtsheer@cetic.be
   */
  def routeExchange(firstVehicles:()=>Iterable[Int],
                    secondVehicles:()=>Int=>Iterable[Int],
                    vrp:VRP,
                    neighborhoodName: String = "RouteExchange",
                    selectFirstVehicleBehavior:LoopBehavior = First(),
                    selectSecondVehicleBehavior:LoopBehavior = First(),
                    hotRestart: Boolean = true,
                    breakSymmetriesAmongVehicles:Boolean = true,
                    skipFirstVehicleIfEmptyRoute:Boolean = false)
  = RouteExchange(
    firstVehicles,
    secondVehicles,
    vrp,
    neighborhoodName,
    selectFirstVehicleBehavior,
    selectSecondVehicleBehavior,
    hotRestart,
    breakSymmetriesAmongVehicles,
    skipFirstVehicleIfEmptyRoute)

}


trait SegmentExchangeAPI{

  type SegmentExchangeMove = oscar.cbls.business.routing.neighborhood.SegmentExchangeMove
  type SegmentExchangeOnSegmentsMove = oscar.cbls.business.routing.neighborhood.SegmentExchangeOnSegments

  /**
   * exchanges segments of different vehicles (not on the same vehicle!)
   *
   * @param vrp the routing problem
   * @param relevantNeighbors given the start and end of the first segment, which are the relevant neighbors for the other segment? (will be filtered for vehicle by the neighborhood)
   * @param vehicles the set of vehicles to consider
   * @param neighborhoodName the name of the neighborhood, used for verbosities
   * @param hotRestart
   * @param tryFlip if false, will not flip any segment (maybe you do not want flipping if using time windows?)
   */
  def segmentExchange(vrp: VRP,
                      relevantNeighbors:()=>Int=>Iterable[Int], //must be routed
                      vehicles:() => Iterable[Int],
                      neighborhoodName:String = "SegmentExchange",
                      hotRestart:Boolean = true,
                      selectFirstVehicleBehavior:LoopBehavior = First(),
                      selectFirstNodeOfFirstSegmentBehavior:LoopBehavior = First(),
                      selectSecondNodeOfFirstSegmentBehavior:LoopBehavior = First(),
                      selectFirstNodeOfSecondSegmentBehavior:LoopBehavior = First(),
                      selectSecondNodeOfSecondSegmentBehavior:LoopBehavior = First(),
                      tryFlip:Boolean = true)
  = SegmentExchange(
    vrp,
    relevantNeighbors,
    vehicles,
    neighborhoodName,
    hotRestart,
    selectFirstVehicleBehavior,
    selectFirstNodeOfFirstSegmentBehavior,
    selectSecondNodeOfFirstSegmentBehavior,
    selectFirstNodeOfSecondSegmentBehavior,
    selectSecondNodeOfSecondSegmentBehavior,
    tryFlip)

  /**
    * exchanges segments of different vehicles (not on the same vehicle!)
    *
    * @param vrp the routing problem
    * @param segmentsToExchangeGroupedByVehicles the map of segments you want to exchange grouped by vehicles
    * @param relevantNeighbors given the start and end of the first segment, which are the relevant neighbors for the other segment? (will be filtered for vehicle by the neighborhood)
    * @param neighborhoodName the name of the neighborhood, used for verbosities
    * @param hotRestart
    * @param tryFlip if false, will not flip any segment (maybe you do not want flipping if using time windows?)
    */
  def segmentExchangeOnSegments(vrp: VRP,
                                segmentsToExchangeGroupedByVehicles: () => Map[Int,(List[(Int,Int)])],
                                relevantNeighbors:()=>Int=>Iterable[Int], //must be routed
                                vehicles:() => Iterable[Int],
                                neighborhoodName:String = "SegmentExchange",
                                hotRestart:Boolean = true,

                                selectFirstVehicleBehavior:LoopBehavior = First(),
                                selectSecondVehicleBehavior:LoopBehavior = First(),
                                selectFirstSegmentBehavior:LoopBehavior = First(),
                                selectSecondSegmentBehavior:LoopBehavior = First(),
                                tryFlip:Boolean = true)
  = SegmentExchangeOnSegments(
    vrp,
    segmentsToExchangeGroupedByVehicles,
    relevantNeighbors,
    vehicles,
    neighborhoodName,
    hotRestart,
    selectFirstVehicleBehavior,
    selectSecondVehicleBehavior,
    selectFirstSegmentBehavior,
    selectSecondSegmentBehavior)
}


trait ThreeOptAPI{

  type ThreeOptMove = oscar.cbls.business.routing.neighborhood.ThreeOptMove

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
  def threeOpt(potentialInsertionPoints:()=>Iterable[Int], //must be routed
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
  = ThreeOpt(
    potentialInsertionPoints,
    relevantNeighbors,
    vrp,
    neighborhoodName,
    selectInsertionPointBehavior,
    selectMovedSegmentBehavior,
    selectFlipBehavior,
    hotRestart,
    skipOnePointMove,
    breakSymmetry,
    tryFlip)
}


trait TwoOptAPI{

  type TwoOptMove = oscar.cbls.business.routing.neighborhood.TwoOptMove


  /**
   * Removes two edges of routes, and rebuilds routes from the segments.
   * (with one reverse required)
   *
   * The search complexity is O(n²).
   * @author renaud.delandtsheer@cetic.be
   * @author yoann.guyot@cetic.be
   * @author Florent Ghilain (UMONS)
   * */
  def twoOpt(segmentStartValues:()=>Iterable[Int],
             relevantNewSuccessors:()=>Int=>Iterable[Int],
             vrp: VRP,
             neighborhoodName:String = "TwoOpt",
             selectSegmentStartBehavior:LoopBehavior = First(),
             selectSegmentEndBehavior:LoopBehavior = First(),
             hotRestart:Boolean = true) =
    TwoOpt(segmentStartValues,
      relevantNewSuccessors,
      vrp,
      neighborhoodName,
      selectSegmentStartBehavior,
      selectSegmentEndBehavior,
      hotRestart)

}
