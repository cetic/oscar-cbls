// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.examples

// Imports to model a problem and a search procedure
import oscar.cbls._
import oscar.cbls.modeling.{Invariants, Neighborhoods}

// Object to generate random data
import oscar.cbls.algo.generator.RoutingGenerator

object VRPBeginnerModelingExample {
  def main(args: Array[String]): Unit = {

    // Model definition
    val m: Model = model("VRP example")

    // Generating random data
    val nbNodes    = 100
    val nbVehicles = 5
    val (_, distanceMatrix, unroutedPenalty, _) =
      RoutingGenerator.generateRandomRoutingData(nbNodes, 2, 0)

    // Sets up the VRS structure
    val vrs: VRS = m.vrs(nbNodes, nbVehicles)

    // Creates a RouteLength invariant
    val totalRouteLength: IntVariable =
      Invariants.routing.totalRouteLength(distanceMatrix, matrixIsSymmetrical = true)(vrs)

    // Creates a penalty for unrouted nodes (otherwise no nodes will be routed)
    val unroutedNodesAndPenalty: IntVariable =
      vrs.unrouted.size() * long2IntConst(unroutedPenalty)(m)

    // Creates the objective : minimizing the routeLength and the penalty for unrouted nodes
    val obj: Objective = m.minimize(totalRouteLength + unroutedNodesAndPenalty)

    // Closes the model
    // No variable or invariant can be added past that point
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
      Neighborhoods.routing.insertPointUnroutedFirst(() => vrs.unroutedNodes, node => kFirst(node))(
        vrs
      )
    val n2: Neighborhood = Neighborhoods.routing.onePointMove(
      () => vrs.routedWithoutVehicles.value(),
      node => kFirst(node)
    )(vrs)
    val n3: Neighborhood =
      Neighborhoods.routing.twoOpt(() => vrs.routedWithoutVehicles.value())(vrs)
    val n4: Neighborhood = Neighborhoods.routing.threeOptMovedSegmentFirst(
      () => vrs.routedWithoutVehicles.value(),
      node => kFirst(node, 10),
      20
    )(vrs)

    // Defining the search procedure using combinators
    val search: Neighborhood = Neighborhoods.combinator.bestSlopeFirst(List(n1, n2, n3, n4))

    // Profiling initialization and setting up verbosity
    search.profileSearch()
    search.verbosityLevel = 1

    // Performs the search
    search.doAllMoves(obj)

    // Displays results
    search.displayProfiling()
    println(vrs)
    println(s"Best objective: ${obj.objValue}")

  }

}
