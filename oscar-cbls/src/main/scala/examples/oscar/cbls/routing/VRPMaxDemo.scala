package examples.oscar.cbls.routing

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

import oscar.cbls._
import oscar.cbls.algo.generator.RoutingMatrixGenerator
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.model.helpers.DistanceHelper
import oscar.cbls.business.routing.visu.RoutingMapTypes
import oscar.cbls.core.computation.Store
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.core.objective.{CascadingObjective, Objective}
import oscar.cbls.core.search.{Best, First}
import oscar.cbls.util.StopWatch

//50.404631, 4.452595
//50.415162, 4.440849

object VRPMaxDemo extends App {

  println("usage: VRPMaxDemo n v")
  //val n = args(0).toLong
  val n = 1000
  //val v = args(1).toLong
  val v = 10

  //50.404631, 4.452595
  //50.415162, 4.440849

  val displayDelay = 100 //ms
  val verbose = 1
  val maxPivotPerValuePercent = 4
  val mapSide = 1000

  new VRPMaxDemo(n,v,maxPivotPerValuePercent,verbose,displayDelay, mapSide)
}

class VRPMaxDemo(n:Int, v:Int, maxPivotPerValuePercent:Int, verbose:Int, displayDelay:Int, mapSide:Int) extends StopWatch {

  val minLat = 50.404631
  val maxLat = 50.415162
  val minLong = 4.440849
  val maxLong = 4.452595

  val (symmetricDistanceMatrix1,nodesPositions) = RoutingMatrixGenerator.geographicRandom(n, minLong,maxLong,minLat,maxLat)

  val symmetricDistanceMatrix = Array.tabulate(n)({a =>
    Array.tabulate(n)({b =>
      symmetricDistanceMatrix1(a min b)(a max b).toLong
    })})

  val maxWorkloadPerVehicle = 2500
  val serviceTimePerNode = 100

  //val maxWorkloadPerVehicle = 4000
  //val serviceTimePerNode = 100

  startWatch()
  val model = Store()

  val myVRP = new VRP(model,n,v)
  val routeLengthPerVehicle = routeLength(myVRP.routes,n,v,perVehicle = true,symmetricDistanceMatrix,true)
  val totalRouteLength = sum(routeLengthPerVehicle)
  val nodesPerVehicle = nodesOfVehicle(myVRP.routes,v)

  val totalServiceTimePerVehicle = nodesPerVehicle.map(cardinality(_)*serviceTimePerNode)

  val c = ConstraintSystem(model)

  for(vehicle <- 0 until v){
    val workLoadOfVehicle = totalServiceTimePerVehicle(vehicle) + routeLengthPerVehicle(vehicle)
    c.add(workLoadOfVehicle le maxWorkloadPerVehicle)
  }

  c.close()

  val graphicExtension = display(myVRP, nodesPositions, None, displayDelay, RoutingMapTypes.RealRoutingMap)
  val penaltyForUnrouted  = 10000

  val obj = new CascadingObjective(
    c,
    Objective(totalRouteLength + (penaltyForUnrouted*(n - length(myVRP.routes))))
  )

  model.close()

  val relevantPredecessorsOfNodes = (node:Int) => myVRP.nodes
  val closestRelevantNeighborsByDistance = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistanceMatrix,relevantPredecessorsOfNodes)(_))

  val routedPostFilter = (node:Int) => (neighbor:Int) => myVRP.isRouted(neighbor)
  val unRoutedPostFilter = (node:Int) => (neighbor:Int) => !myVRP.isRouted(neighbor)

  val routeUnroutedPoint = insertPointUnroutedFirst(myVRP.unrouted,
    ()=>myVRP.kFirst(10,closestRelevantNeighborsByDistance(_),routedPostFilter),
    myVRP,
    neighborhoodName = "InsertUF",
    hotRestart = false,
    selectNodeBehavior = First(),
    selectInsertionPointBehavior = Best())

  val routeUnroutedPointBad =  insertPointUnroutedFirst(myVRP.unrouted,
    ()=> myVRP.kFirst(20,closestRelevantNeighborsByDistance(_),routedPostFilter),
    myVRP,
    neighborhoodName = "InsertUF",
    hotRestart = false)


  //using post-filters on k-nearest is probably a bit slower than possible for large problems.
  //that's why we prefer to block this neighborhood when many nodes are already routed (so few are unrouted, so the filter filters many nodes away)
  val routeUnroutedPoint2 = insertPointRoutedFirst(
    myVRP.routed,
    ()=>myVRP.kFirst(10,closestRelevantNeighborsByDistance(_),unRoutedPostFilter),  //should be the backward ones but this is a symmetric distance so we do not care
    myVRP,
    neighborhoodName = "InsertRF") guard(() => myVRP.routed.value.size < n/2)

  def onePtMove(k:Int) = onePointMove(
    myVRP.routed,
    () => myVRP.kFirst(k,closestRelevantNeighborsByDistance(_),routedPostFilter),
    myVRP,
    selectDestinationBehavior = Best())

  val customTwoOpt = twoOpt(myVRP.routed, ()=>myVRP.kFirst(40,closestRelevantNeighborsByDistance(_),routedPostFilter), myVRP)

  def customThreeOpt(k:Int, breakSym:Boolean) =
    threeOpt(myVRP.routed, ()=>myVRP.kFirst(k,closestRelevantNeighborsByDistance(_),routedPostFilter), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")")

  val vlsn1pt = mu[OnePointMoveMove](
    onePointMove(myVRP.routed, () => myVRP.kFirst(5,closestRelevantNeighborsByDistance(_),routedPostFilter),myVRP),
    l => Some(onePointMove(() => List(l.head.newPredecessor).filter(_ >= v), () => myVRP.kFirst(3,closestRelevantNeighborsByDistance(_),routedPostFilter),myVRP, hotRestart = false)),
    intermediaryStops = true,
    maxDepth = 6)

  def segExchange(k:Int) = segmentExchange(myVRP,()=>myVRP.kFirst(k,closestRelevantNeighborsByDistance(_),routedPostFilter), () => myVRP.vehicles)

  val search = (
    bestSlopeFirst(List(
    routeUnroutedPoint,
    routeUnroutedPoint2,
    vlsn1pt,
    onePtMove(10),
    customTwoOpt,
    customThreeOpt(10,true),
    segExchange(10)))
    exhaust bestSlopeFirst(List(
    customThreeOpt(30,true),
    segExchange(30),
    routeUnroutedPoint2,
    customTwoOpt,
    (insertPointUnroutedFirst(
      myVRP.unrouted,
      ()=>myVRP.kFirst(20,closestRelevantNeighborsByDistance(_),routedPostFilter),
      myVRP,
      neighborhoodName = "InsertUF",
      hotRestart = false,
      selectNodeBehavior = First(),
      selectInsertionPointBehavior = Best())
      andThen onePtMove(10)) name "RouteAndMove",
    vlsn1pt))).afterMove(
    graphicExtension.drawRoutes()
  ) //showObjectiveFunction(myVRP.obj)

  search.verbose = verbose
  //search.verboseWithExtraInfo(1, ()=> "" + myVRP)
  //  routeUnroutdPoint.verbose= 4
  search.doAllMoves(obj = obj)

  graphicExtension.drawRoutes()
  print(myVRP)
}
