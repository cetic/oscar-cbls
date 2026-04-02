package oscar.cbls.examples

// Imports to model a problem and a search procedure
import oscar.cbls._
import oscar.cbls.modeling.{Invariants => Inv, Neighborhoods => Nrs}

// Object to generate random data
import oscar.cbls.algo.generator.RoutingGenerator

// GUI
import oscar.cbls.visual.cartesian.routing.CartesianRoutingDisplay

import scala.io.StdIn

object VRPAdvancedModelingExample {
  def main(args: Array[String]): Unit = {
    // Model definition
    implicit val m: Model = model("VRP example")

    // Generating random data
    val nbNodes    = 100
    val nbVehicles = 5
    val (nodesCoordinate, distanceMatrix, unroutedPenalty, _) =
      RoutingGenerator.generateRandomRoutingData(nbNodes, 2, 0)

    // Sets up the VRS structure
    implicit val vrs: VRS = m.vrs(nbNodes, nbVehicles)

    // Creates a RouteLength invariant
    val totalRouteLength: IntVariable =
      Inv.routing.totalRouteLength(distanceMatrix, matrixIsSymmetrical = true)

    // Creates a penalty for unrouted nodes (otherwise no nodes will be routed)
    val unroutedNodesAndPenalty: IntVariable = vrs.unrouted.size() * unroutedPenalty

    // Creates the objective : minimizing the routeLength and the penalty for unrouted nodes
    val obj: Objective = m.minimize(totalRouteLength + unroutedNodesAndPenalty)

    // Closes the model. No variable or invariant can be added past that point
    m.close()

    // Some utility variable and method to optimize insertions and moves
    val closestNodesOfNode: Array[List[Int]] =
      Array.tabulate(vrs.n)(from => (0 until vrs.n).toList.sortBy(to => distanceMatrix(from)(to)))

    def kFirst(node: Int, k: Int = 20) =
      vrs.kFirst(k, node => closestNodesOfNode(node), node => x => vrs.isRouted(x) && x != node)(
        node
      )

    // Basic neighborhoods' initialization
    val n1: Neighborhood =
      Nrs.routing.insertPointUnroutedFirst(() => vrs.unroutedNodes, node => kFirst(node))
    val n2: Neighborhood =
      Nrs.routing.onePointMove(() => vrs.routedWithoutVehicles.value(), node => kFirst(node))
    val n3: Neighborhood = Nrs.routing.twoOpt(() => vrs.routedWithoutVehicles.value())
    val n4: Neighborhood = Nrs.routing.threeOptMovedSegmentFirst(
      () => vrs.routedWithoutVehicles.value(),
      node => kFirst(node, 10),
      20
    )

    // Defining the search procedure using combinators
    val search: Neighborhood = Nrs.combinator.bestSlopeFirst(List(n1, n2, n3, n4))

    // Try population-based search
//    val pbs = Nrs.combinator.populationBasedSearch((it, _) =>
//      if (it < 10) Some((List(n1, n2, n3, n4), 4))
//      else None
//    )

    // Initialization of the problem resolution display
    val visu =
      CartesianRoutingDisplay(obj.objValue, vrs, nodesCoordinate, width = 800, height = 800)
    // Defining a search procedure with visualization
    val searchWithVisu = Nrs.combinator.updateDisplay(search, visu)

    // Profile initialization and setting up verbosity
    searchWithVisu.profileSearch()
    searchWithVisu.verbosityLevel = 1

    searchWithVisu.doAllMoves(obj)

    // Displays results
    searchWithVisu.displayProfiling()
    println(vrs)
    println(s"Best objective: ${obj.objValue}\n")

    // May be necessary to keep the display alive depending on your platform
    StdIn.readLine()
  }
}
