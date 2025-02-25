package oscar.cbls.examples

import oscar.cbls._
import oscar.cbls.algo.generator.RoutingGenerator
import oscar.cbls.lib.neighborhoods.combinator.{BestSlopeFirst, UpdateDisplay}
import oscar.cbls.lib.neighborhoods.routing.{ThreeOpt, TwoOpt}
import oscar.cbls.modeling.{Invariants => Inv, Neighborhoods => Nrs}
import oscar.cbls.visual.cartesian.routing.CartesianRoutingDisplay

import scala.io.StdIn

object VRPModelingExample {
  def main(args: Array[String]): Unit = {
    ///////////////////
    // Model definition
    implicit val m: Model = model("VRP example")

    // Generating random data
    val nbNodes    = 100
    val nbVehicles = 5
    val (nodesCoordinate, distanceMatrix, unroutedPenalty, _) =
      RoutingGenerator.generateRandomRoutingData(nbNodes, 2, 0)

    // Sets up the VRP structure.
    implicit val vrs: VRS = m.vrs(nbNodes, nbVehicles)

    // Creates a RouteLength invariant.
    val totalRouteLength: IntVariable =
      Inv.routing.totalRouteLength(distanceMatrix, matrixIsSymmetrical = true)

    // Creates a penalty for unrouted nodes (otherwise no nodes will be routed).
    val unroutedNodesAndPenalty: IntVariable = vrs.unrouted.size() * unroutedPenalty

    // Creates the objective : minimizing the routeLength and the penalty for unrouted nodes.
    val obj: Objective = m.minimize(totalRouteLength + unroutedNodesAndPenalty)

    // Closes the model. No variable or invariant can be added past that point.
    m.close()

    // Some utility variable and method to optimize insertions and moves.
    val closestNodesOfNode: Array[List[Int]] =
      Array.tabulate(vrs.n)(from => (0 until vrs.n).toList.sortBy(to => distanceMatrix(from)(to)))

    def kFirst(node: Int, k: Int = 20) =
      vrs.kFirst(k, node => closestNodesOfNode(node), node => x => vrs.isRouted(x) && x != node)(
        node
      )

    // Initialization of the problem resolution display.
    val visu = CartesianRoutingDisplay(obj.objValue, vrs, nodesCoordinate)

    // Basic neighborhoods' initialization.
    val n1: Neighborhood =
      Nrs.routing.insertPointUnroutedFirst(() => vrs.unroutedNodes, node => kFirst(node))
    val n2: Neighborhood =
      Nrs.routing.onePointMove(() => vrs.routedWithoutVehicles.value(), node => kFirst(node))
    val n3: Neighborhood = TwoOpt(vrs, () => vrs.routedWithoutVehicles.value())
    val n4: Neighborhood = ThreeOpt.movedSegmentFirst(
      vrs,
      () => vrs.routedWithoutVehicles.value(),
      node => kFirst(node, 10),
      20
    )

    // Defining the search procedure using combinators.
    val search: Neighborhood = UpdateDisplay(BestSlopeFirst(List(n1, n2, n3, n4)), visu)

    // Profile initialization and setting up verbosity.
    search.profileSearch()
    search.verbosityLevel = 1

    search.doAllMoves(obj)

    // Display results and keep the visualization running.
    println(vrs)
    search.displayProfiling()
    StdIn.readLine()
  }
}
