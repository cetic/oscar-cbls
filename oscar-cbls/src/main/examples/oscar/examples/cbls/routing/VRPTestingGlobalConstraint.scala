package oscar.examples.cbls.routing

import oscar.cbls.business.routing.invariants.NbNodes
import oscar.cbls.business.routing.invariants.global.RouteLength
import oscar.cbls.business.routing.model.helpers.DistanceHelper
import oscar.cbls.business.routing.neighborhood.{ThreeOpt, TwoOpt}
import oscar.cbls.business.routing.{VRP, insertPointUnroutedFirst, onePointMove}
import oscar.cbls.core.computation.{CBLSIntVar, Store}
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.core.objective.{CascadingObjective, Objective}
import oscar.cbls.core.search.Best
import oscar.cbls._

object VRPTestingGlobalConstraint extends App {

  val nbNode = 150
  val nbVehicle = 4
  val model = Store() //checker = Some(new ErrorChecker))

  val problem = new VRP(model, nbNode, nbVehicle)

  val (symetricDistanceMatrix, _) = RoutingMatrixGenerator(nbNode)

  val routeLengths: Array[CBLSIntVar] = Array.tabulate(nbVehicle)({ _ => CBLSIntVar(model, 0) })
  val routeLengthPerVehicle = new RouteLength(problem.routes, nbNode, nbVehicle, routeLengths, (from: Int, to: Int) => symetricDistanceMatrix(from)(to))

  val totalRouteLength = sum(routeLengths)

  val nbNodesPerVehicle: Array[CBLSIntVar] = Array.tabulate(nbVehicle)({ _ => CBLSIntVar(model, 0) })
  val nbNodeConstraint = new NbNodes(problem.routes, nbNode, nbVehicle, nbNodesPerVehicle)
  val nbNodesPerVehicle1: Array[CBLSIntVar] = Array.tabulate(nbVehicle)({ _ => CBLSIntVar(model, 0) })
  //val nbNodeConstraint1 = new LogReducedNumberOfNodes(problem.routes,nbVehicle,nbNodesPerVehicle1)
  val nbNodesPerVehicle2: Array[CBLSIntVar] = Array.tabulate(nbVehicle)({ _ => CBLSIntVar(model, 0) })
  //val nbNodeConstraint2 = new LogReducedNumberOfNodesWithExtremes(problem.routes,nbVehicle,nbNodesPerVehicle2)

  val c = ConstraintSystem(model)

  for (vehicle <- 0 until nbVehicle) {
    c.add(nbNodesPerVehicle(vehicle) le 100)
  }

  c.close()

  val obj = new CascadingObjective(c, Objective(totalRouteLength + 10000 * (nbNode - length(problem.routes))))

  model.close()

  val closestRelevantNeighbors = Array.tabulate(nbNode)(DistanceHelper.lazyClosestPredecessorsOfNode(symetricDistanceMatrix, _ => problem.nodes)(_))


  def routeUnroutedPoint =
    profile(insertPointUnroutedFirst(problem.unrouted,
      () => problem.kFirst(10, closestRelevantNeighbors(_), _ => node => problem.isRouted(node)),
      problem,
      selectInsertionPointBehavior = Best(),
      neighborhoodName = "InsertUR 1"))

  val routeUnroutedPointLarger =
    profile(insertPointUnroutedFirst(problem.unrouted,
      () => problem.kFirst(100, closestRelevantNeighbors(_), _ => node => problem.isRouted(node)),
      problem,
      selectInsertionPointBehavior = Best(),
      neighborhoodName = "InsertURLarger 1"))


  /*val routeUnroutedPoint =
    profile(insertPointUnroutedFirst(problem.unrouted,
      () => _ => problem.routes.value,
      problem,
      selectInsertionPointBehavior = Best(),
      neighborhoodName = "InsertUR"))*/

  def onePtMove =
    onePointMove(problem.routed,
      () => problem.kFirst(10, closestRelevantNeighbors(_), _ => node => problem.isRouted(node)),
      problem)

  val twoOpt =
    profile(TwoOpt(problem.routed,
      () => problem.kFirst(10, closestRelevantNeighbors(_), _ => node => problem.isRouted(node)),
      problem))

  def threeOpt =
    ThreeOpt(problem.routed,
      () => problem.kFirst(10, closestRelevantNeighbors(_), _ => node => problem.isRouted(node)),
      problem)

  val search =
    bestSlopeFirst(List(routeUnroutedPoint, twoOpt))

  search.verbose = 1

  search.doAllMoves(obj = obj)

  println(problem)
  println(totalRouteLength)
  println(obj)
  println(search.profilingStatistics)

}
