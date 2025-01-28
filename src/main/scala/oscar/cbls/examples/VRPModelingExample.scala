package oscar.cbls.examples

import oscar.cbls._
import oscar.cbls.algo.generator.RoutingGenerator
import oscar.cbls.modeling.{Invariants => Inv, Neighborhoods => Nrs}

object VRPModelingExample {
  def main(args: Array[String]): Unit = {
    implicit val m: Model = model("VRP example")

    val nbNodes    = 100
    val nbVehicles = 10
    val (_, distanceMatrix, unroutedPenalty, _) =
      RoutingGenerator.generateGeographicRoutingData(nbNodes, 2, 0)

    val vrp: VRP = m.setVrp(nbNodes, nbVehicles)

    val totalRouteLength: IntVariable =
      Inv.routing.totalRouteLength(distanceMatrix, matrixIsSymmetrical = true)

    val unroutedNodes: IntVariable = vrp.unrouted.size()

    val unroutedNodesAndPenalty: IntVariable = unroutedNodes * unroutedPenalty

    val obj: Objective = m.minimize(totalRouteLength + unroutedNodesAndPenalty)

    m.close()

    val n1: Neighborhood = Nrs.routing.insertPointUnroutedFirst(
      () => vrp.unroutedNodes,
      _ => vrp.routedWithVehicles.value()
    )

    val n2: Neighborhood = Nrs.routing.onePointMove(
      nodesToMove = () => vrp.routedWithoutVehicles.value(),
      relevantDestinationNodes = x => vrp.routedWithVehicles.value().filterNot(_ == x)
    )

    val search: Neighborhood = Nrs.combinator.roundRobin(Seq((n1, 1), (n2, 1)))

    search.verbosityLevel = 1

    search.doAllMoves(obj)

    println(vrp)
  }
}
