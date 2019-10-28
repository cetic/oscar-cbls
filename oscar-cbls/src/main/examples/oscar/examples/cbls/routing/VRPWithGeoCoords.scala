package oscar.examples.cbls.routing

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.core.search.{Best, First}
import oscar.cbls.visual.routing.RoutingMapTypes

object VRPWithGeoCoords extends App{
  val n = 1000
  val v = 10

  val minLat = 49.404631
  val maxLat = 51.415162
  val minLong = 3.440849
  val maxLong = 5.452595

  new VRPWithGeoCoords(n,v,minLat,maxLat,minLong,maxLong)
}

/**
  * Simple example of VRP resolution with OscaR with display on a real map (OSM)
  * @param n The total number of nodes of the problem (with the depots)
  * @param v The total number of vehicles (aka depots)
  * @param minLat The minimum latitude of generated nodes
  * @param maxLat The maximum latitude of generated nodes
  * @param minLong The minimum longitude of generated nodes
  * @param maxLong The maximum longitude of generated nodes
  */
class VRPWithGeoCoords(n: Int, v: Int, minLat: Double, maxLat: Double, minLong: Double, maxLong: Double) {
  //////////////////// MODEL ////////////////////
  // The Store : used to store all the model of the problem
  val store = new Store

  // The basic VRP problem, containing the basic needed invariant
  val myVRP = new VRP(store,n,v)
  // Generating the nodes of the problem and making it symmetrical
  val (asymetricDistanceMatrix, geoCoords) = RoutingMatrixGenerator.geographicRandom(n,minLong,maxLong,minLat,maxLat)
  val symmetricDistanceMatrix = Array.tabulate(n)({a =>
    Array.tabulate(n)({b =>
      asymetricDistanceMatrix(a min b)(a max b).toLong
    })})

  // An invariant that store the total distance travelled by the cars
  val totalDistance = sum(routeLength(myVRP.routes, n, v, false, symmetricDistanceMatrix, true))

  // A penalty given to all unrouted nodes to force the optimisation to route them
  val unroutedPenalty = 1000000
  // The objectif function : unroutedNode*penalty + totalDistance ==> To minimize
  val obj = Objective((n-length(myVRP.routes))*unroutedPenalty + totalDistance)

  store.close()


  //////////////////// Pruning and display ////////////////////
  ////////// Display VRP resolution on real map //////////

  val routingDisplay = display(myVRP,geoCoords,routingMapType = RoutingMapTypes.RealRoutingMap, refreshRate = 10)

  ////////// Static Pruning (done once before starting the resolution) //////////

  // Relevant predecessors definition for each node (here any node can be the precessor of another node)
  val relevantPredecessorsOfNodes = (node:Long) => myVRP.nodes
  // Sort them lazily by distance
  val closestRelevantNeighborsByDistance =
    Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistanceMatrix,relevantPredecessorsOfNodes)(_))

  ////////// Dynamic pruning (done each time before evaluation a move) //////////
  // Only condition the new neighbor must be routed
  val routedPostFilter = (node:Long) => (neighbor:Long) => myVRP.isRouted(neighbor)

  //////////////////// Search Procedure ////////////////////
  ////////// Neighborhood definition //////////

  // Takes an unrouted node and insert it at the best position within the 10 closest nodes (inserting it after this node)
  val routeUnroutedPoint =  profile(insertPointUnroutedFirst(myVRP.unrouted,
    ()=>myVRP.kFirst(10,closestRelevantNeighborsByDistance(_),routedPostFilter),
    myVRP,
    neighborhoodName = "InsertUF",
    hotRestart = false,
    selectNodeBehavior = First(), // Select the first unrouted node in myVRP.unrouted
    selectInsertionPointBehavior = Best())) // Inserting after the best node in myVRP.kFirst(10,...)

  // Moves a routed node to a better place (best neighbor within the 10 closest nodes)
  def onePtMove(k:Long) = profile(onePointMove(
    myVRP.routed,
    () => myVRP.kFirst(k,closestRelevantNeighborsByDistance(_),routedPostFilter),
    myVRP,
    selectDestinationBehavior = Best()))

  // Swap two edges based on the 40 closest neighbors of the first segment's head
  val customTwoOpt = profile(twoOpt(myVRP.routed, ()=>myVRP.kFirst(40,closestRelevantNeighborsByDistance(_),routedPostFilter), myVRP))

  ////////// Final search procedure //////////

  // bestSlopeFirst => Perform the best neighborhood in the list (meaning the one that reduces the most the objective function)
  // afterMove => after each move update the routing display
  val searchProcedure = bestSlopeFirst(
    List(routeUnroutedPoint, onePtMove(20),customTwoOpt)
  ).showObjectiveFunction(obj).afterMove(
    routingDisplay.drawRoutes())


  //////////////////// RUN ////////////////////

  searchProcedure.verbose = 1
  searchProcedure.doAllMoves(obj = obj)
  routingDisplay.drawRoutes(true)
  println(myVRP)
  println(obj)
}
