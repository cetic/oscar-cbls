package examples.oscar.cbls.routing

import oscar.cbls._
import oscar.cbls.algo.generator.RoutingMatrixGenerator
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.global._
import oscar.cbls.business.routing.model.helpers.DistanceHelper
import oscar.cbls.business.routing.neighborhood.{ThreeOpt, TwoOpt}
import oscar.cbls.core.search.Best

object SimpleVRP extends App{
  val n: Int = 2000
  val v: Int = 20
  val size: Int = 10000

  val start = System.currentTimeMillis()
  for(i <- 0 until 1)
    new SimpleVRP(n,v, size, i)
  println("Done in " + (System.currentTimeMillis() - start))
}

class SimpleVRP(n: Int, v: Int, size: Int, iteration: Int) {

  val m = new Store()

  val myVRP = new VRP(m, n, v)
  RoutingMatrixGenerator.random.setSeed(iteration)
  val (distancesMatrix, _) = RoutingMatrixGenerator(n, size)

  val routeLength = RouteLength(myVRP.routes, n, v, (from: Int, to: Int) => distancesMatrix(from)(to))

  val obj = Objective(sum(routeLength) + 1000000*(n - length(myVRP.routes)))

  m.close()

  val closestRelevantNeighbors = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(distancesMatrix,_ => myVRP.nodes)(_))


  def routeUnroutedPoint =
    profile(insertPointUnroutedFirst(myVRP.unrouted,
      () => myVRP.kFirst(10,closestRelevantNeighbors(_),_ => node => myVRP.isRouted(node)),
      myVRP,
      selectInsertionPointBehavior = Best(),
      neighborhoodName = "InsertUR 1"))

  def onePtMove =
    profile(onePointMove(myVRP.routed,
      () => myVRP.kFirst(10,closestRelevantNeighbors(_),_ => node => myVRP.isRouted(node)),
      myVRP,
      selectPointToMoveBehavior = Best(),
      selectDestinationBehavior = Best()))

  val twoOpt =
    profile(TwoOpt(myVRP.routed,
      () => myVRP.kFirst(10,closestRelevantNeighbors(_),_ => node => myVRP.isRouted(node)),
      myVRP,
      selectSegmentStartBehavior = Best(),
      selectSegmentEndBehavior = Best()))

  def threeOpt =
    profile(ThreeOpt(myVRP.routed,
      () => myVRP.kFirst(10,closestRelevantNeighbors(_),_ => node => myVRP.isRouted(node)),
      myVRP,
      selectInsertionPointBehavior = Best(),
      selectMovedSegmentBehavior = Best(),
      selectFlipBehavior = Best()))

  val search =
  //bestSlopeFirst(List(routeUnroutedPoint, twoOpt, onePtMove, threeOpt))
    bestSlopeFirst(List(routeUnroutedPoint, onePtMove, twoOpt, threeOpt))

  search.verbose = 0
  search.doAllMoves(obj = obj)
  println(search.profilingStatistics)
  //println(myVRP)

  /*
 val n2 = profile(TreeOpt.threeOptOnVehicle(myVRP,0,6))
 n2.verbose = 1
 n2.doAllMoves(obj = obj)
 println(n2.profilingStatistics)
 println(myVRP)
 //println(myVRP)

 val n3 = profile(TreeOpt.threeOptOnVehicle(myVRP,1,6))
 n3.verbose = 1
 n3.doAllMoves(obj = obj)
 println(n3.profilingStatistics)
*/
}