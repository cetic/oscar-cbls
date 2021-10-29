package examples.oscar.cbls.routing

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.WeightedNodesPerVehicle
import oscar.cbls.business.routing.invariants.timeWindow.{DefinedTransferFunction, NaiveTimeWindowConstraint, TimeWindowConstraintWithLogReduction, TransferFunction}
import oscar.cbls.business.routing.model.helpers.DistanceHelper
import oscar.cbls.business.routing.visu.RoutingMapTypes
import oscar.cbls.core.computation.{CBLSIntVar, Domain, Store}
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.core.objective.CascadingObjective
import oscar.cbls.core.search.{First, Neighborhood}
import oscar.cbls.lib.search.combinators.DoOnMove

import scala.util.Random

object VRPTWWithWeightedNodes extends App{
  val n = 11
  val v = 4

  val minLat = 50.404631
  val maxLat = 50.415162
  val minLong = 4.440849
  val maxLong = 4.452595

  new VRPTWWithWeightedNodes(n,v,minLat,maxLat,minLong,maxLong)
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
class VRPTWWithWeightedNodes(n: Int, v: Int, minLat: Double, maxLat: Double, minLong: Double, maxLong: Double) {
  //////////////////// MODEL ////////////////////
  // The Store : used to store all the model of the problem
  val store = Store()

  // The basic VRP problem, containing the basic needed invariant
  val myVRP = new VRP(store, n, v)
  // Generating the nodes of the problem and making it symmetrical
  val (asymetricDistanceMatrix, geoCoords) = RoutingMatrixGenerator.geographicRandom(n, minLong, maxLong, minLat, maxLat)
  val symmetricDistanceMatrix = Array.tabulate(n)({ a =>
    Array.tabulate(n)({ b =>
      asymetricDistanceMatrix(a min b)(a max b).toLong
    })
  })

  // Generating timeMatrix and a time window for each node of the problem
  val timeMatrix = symmetricDistanceMatrix
  //Strong time windows
  val strongSingleNodeTransferFunctions = RoutingMatrixGenerator.generateFeasibleTransferFunctions(n, v, timeMatrix)
  //Weak time windows
  val weakSingleNodeTransferFunctions = Array.tabulate(n)(node =>
    if(node < v) strongSingleNodeTransferFunctions(node)
    else {
      val delta = (strongSingleNodeTransferFunctions(node).la - strongSingleNodeTransferFunctions(node).ea)/5
      DefinedTransferFunction(strongSingleNodeTransferFunctions(node).ea + delta,
        strongSingleNodeTransferFunctions(node).la - delta,
        strongSingleNodeTransferFunctions(node).el + delta, node, node)
    })

  // Generating node weight (0 for depot and 10 to 20 for nodes)
  val random = new Random(1)
  val nodeWeight = Array.tabulate(n)(node => if(node < v)0L else random.nextInt(11)+10L)
  // Vehicles have capacity varying from (n-v)/(2*v) to (2*(n-v))/v
  val vehicleCapacity = Array.fill(v)(15*(random.nextInt((2*(n-v)/v)-((n-v)/(2*v))+1)+(n-v)/(2*v)))


  ////////// INVARIANTS //////////
  // An invariant that store the total distance travelled by the cars
  val totalDistance = sum(routeLength(myVRP.routes, n, v, false, symmetricDistanceMatrix, true))

  // The STRONG timeWindow constraint (vehicleTimeWindowViolations contains the violation of each vehicle)
  val vehicleTimeWindowViolations = Array.fill(v)(new CBLSIntVar(store, 0L, Domain(0L, n)))
  val timeWindowStrongConstraint =
    TimeWindowConstraintWithLogReduction(
      myVRP.routes,
      n, v,
      strongSingleNodeTransferFunctions,
      timeMatrix,
      vehicleTimeWindowViolations
    )

  // The WEAK timeWindow constraint (vehicle can violate the timeWindow constraint but we must minimize this value)
  // If the strong earlyline == weak earlyline ==> no need to define weak earlyline
  // If the strong deadline == weak deadline ==> no need to define weak deadline
  val timeWindowWeakConstraint = NaiveTimeWindowConstraint(myVRP.routes, n, v, weakSingleNodeTransferFunctions, timeMatrix)
  val totalExcessTimeForWeakConstraint = timeWindowWeakConstraint.violation

  // Weighted node constraint
  // The sum of node's weight can't excess the capacity of a vehicle
  val weightPerVehicle = Array.tabulate(v)(_ => CBLSIntVar(store))
  // This invariant maintains the total node's weight encountered by each vehicle
  val weightedNodesConstraint = WeightedNodesPerVehicle(myVRP.routes, n, v, nodeWeight, weightPerVehicle)
  // This invariant maintains the capacity violation of each vehicle (le means lesser or equals)
  val vehicleCapacityViolation = Array.tabulate(v)(vehicle => (weightPerVehicle(vehicle) le vehicleCapacity(vehicle)))
  val constraintSystem = new ConstraintSystem(store)
  vehicleCapacityViolation.foreach(constraintSystem.post(_))


  ////////// OBJECTIVE FUNCTION //////////
  // A penalty given to all unrouted nodes to force the optimisation to route them
  val unroutedPenalty = 1000000
  // The objectif function :
  // If there is no time window violation :
  //    unroutedNode*penalty + totalDistance ==> To minimize
  val obj = CascadingObjective(
    constraintSystem,
    sum(vehicleTimeWindowViolations),
    (n - length(myVRP.routes)) * unroutedPenalty + totalDistance + totalExcessTimeForWeakConstraint)

  store.close()


  //////////////////// Pruning and display ////////////////////
  ////////// Display VRP resolution on real map //////////

  val routingDisplay = display(myVRP, geoCoords, routingMapType = RoutingMapTypes.RealRoutingMap, refreshRate = 10)

  ////////// Static Pruning (done once before starting the resolution) //////////

  // Relevant predecessors definition for each node (here any node can be the precessor of another node)
  val relevantPredecessorsOfNodes = TransferFunction.relevantPredecessorsOfNodes(n,v,strongSingleNodeTransferFunctions,timeMatrix)
  // Sort them lazily by distance
  val closestRelevantNeighborsByDistance =
    Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistanceMatrix, relevantPredecessorsOfNodes)(_))

  ////////// Dynamic pruning (done each time before evaluation a move) //////////
  // Only condition the new neighbor must be routed
  val routedPostFilter = (node: Int) => (neighbor: Int) => myVRP.isRouted(neighbor)

  //////////////////// Search Procedure ////////////////////
  ////////// Neighborhood definition //////////

  // Takes an unrouted node and insert it at the best position within the 10 closest nodes (inserting it after this node)
  val routeUnroutedPoint = profile(insertPointUnroutedFirst(myVRP.unrouted,
    () => myVRP.kFirst(20, closestRelevantNeighborsByDistance(_), routedPostFilter),
    myVRP,
    neighborhoodName = "InsertUF",
    hotRestart = false,
    selectNodeBehavior = First(), // Select the first unrouted node in myVRP.unrouted
    selectInsertionPointBehavior = First())) // Inserting after the first node in myVRP.kFirst(10,...)

  // Moves a routed node to a better place (best neighbor within the 10 closest nodes)
  def onePtMove(k: Int) = profile(onePointMove(
    myVRP.routed,
    () => myVRP.kFirst(k, closestRelevantNeighborsByDistance(_), routedPostFilter),
    myVRP))

  ////////// Final search procedure //////////

  // bestSlopeFirst => Perform the best neighborhood in the list (meaning the one that reduces the most the objective function)
  // afterMove => after each move update the routing display
  val searchProcedure: Neighborhood =
  routeUnroutedPoint.
    exhaust(onePtMove(20)).
    afterMove(routingDisplay.drawRoutes())

  //////////////////// RUN ////////////////////

  searchProcedure.verbose = 3
  searchProcedure.doAllMoves(obj = obj)
  routingDisplay.drawRoutes(true)
  println(myVRP)
  println(searchProcedure.profilingStatistics)
}
