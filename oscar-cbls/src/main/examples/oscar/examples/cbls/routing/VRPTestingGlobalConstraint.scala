package oscar.examples.cbls.routing

import oscar.cbls._
import oscar.cbls.algo.seq._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.NbNodes
import oscar.cbls.business.routing.invariants.global._
import oscar.cbls.business.routing.neighborhood.{ThreeOpt, ThreeOptMove, TwoOpt}
import oscar.cbls.core.computation.ChangingSeqValue
import oscar.cbls.core.search.Best
import oscar.cbls.lib.constraint.LE



object VRPTestingGlobalConstraint extends App {


  val nbNode = 150
  val nbVehicle = 4
  val model = new Store() //checker = Some(new ErrorChecker))
  //val model = new Store()

  val problem = new VRP(model,nbNode,nbVehicle)

  val (symetricDistanceMatrix,_) = RoutingMatrixGenerator(nbNode)


  val gc = GlobalConstraintCore(problem.routes, nbVehicle)

  val routeLengths : Array[CBLSIntVar] = Array.tabulate(nbVehicle)({_ => CBLSIntVar(model,0)})
  val routeLengthPerVehicle = new RouteLength(gc,nbNode,nbVehicle,routeLengths, (from: Long, to: Long) => symetricDistanceMatrix(from)(to))

  val totalRouteLength = sum(routeLengths)

  val nbNodesPerVehicle : Array[CBLSIntVar] = Array.tabulate(nbVehicle)({_ => CBLSIntVar(model,0)})
  val nbNodeConstraint = new NbNodes(gc,nbNode,nbVehicle,nbNodesPerVehicle)
  val nbNodesPerVehicle1 : Array[CBLSIntVar] = Array.tabulate(nbVehicle)({_ => CBLSIntVar(model,0)})
  //val nbNodeConstraint1 = new LogReducedNumberOfNodes(problem.routes,nbVehicle,nbNodesPerVehicle1)
  val nbNodesPerVehicle2 : Array[CBLSIntVar] = Array.tabulate(nbVehicle)({_ => CBLSIntVar(model,0)})
  //val nbNodeConstraint2 = new LogReducedNumberOfNodesWithExtremes(problem.routes,nbVehicle,nbNodesPerVehicle2)


  val c = new ConstraintSystem(model)

  for(vehicle <- 0 until nbVehicle){
    c.add(nbNodesPerVehicle(vehicle) le 100)
  }

  c.close()

  val obj = new CascadingObjective(c,Objective(totalRouteLength + 10000 * (nbNode - length(problem.routes))))

  model.close()

  val closestRelevantNeighbors = Array.tabulate(nbNode)(DistanceHelper.lazyClosestPredecessorsOfNode(symetricDistanceMatrix,_ => problem.nodes)(_))


  def routeUnroutedPoint =
    profile(insertPointUnroutedFirst(problem.unrouted,
      () => problem.kFirst(10,closestRelevantNeighbors(_),_ => node => problem.isRouted(node)),
      problem,
      selectInsertionPointBehavior = Best(),
      neighborhoodName = "InsertUR 1"))

  val routeUnroutedPointLarger =
    profile(insertPointUnroutedFirst(problem.unrouted,
      () => problem.kFirst(100,closestRelevantNeighbors(_),_ => node => problem.isRouted(node)),
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
      () => problem.kFirst(10,closestRelevantNeighbors(_),_ => node => problem.isRouted(node)),
      problem)

  val twoOpt =
    profile(TwoOpt(problem.routed,
      () => problem.kFirst(10,closestRelevantNeighbors(_),_ => node => problem.isRouted(node)),
      problem))

  def threeOpt =
    ThreeOpt(problem.routed,
      () => problem.kFirst(10,closestRelevantNeighbors(_),_ => node => problem.isRouted(node)),
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