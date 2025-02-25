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

package oscar.cbls.test.lib.neighborhoods.routing

import org.scalatest.funsuite.AnyFunSuite
import oscar.cbls.algo.generator.RoutingGenerator
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.Minimize
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.lib.neighborhoods.routing.OnePointMoveNeighborhood
import oscar.cbls.modeling.routing.VRS

import scala.util.Random

class OnePointMoveNeighborhoodTestSuite extends AnyFunSuite {

  private[this] def getBasicModelForTest: (VRS, () => Iterable[Int], Minimize) = {
    val seed: Long = Random.nextLong()
    println(s"\nSeed: $seed")
    val rng = new Random(seed)

    val v = 1
    val n = 5
    val d = 10

    val coordinates = RoutingGenerator.evenlySpacedNodes(n, v, d, RoutingGenerator.centerDepot, rng)
    val distances   = RoutingGenerator.distancesMatrix(coordinates)

    val model    = new Store(debugLevel = 3)
    val vrs      = VRS(model, n, v, debug = true)
    var toInsert = rng.shuffle(List.from(v until n))
    while (toInsert.nonEmpty) {
      val node     = toInsert.head
      val explorer = vrs.routes.pendingValue.explorerAtPosition(0).get
      vrs.routes.insertAfterPosition(node, explorer)
      toInsert = toInsert.tail
    }
    val objValue  = IntVariable(model, 0L)
    val objective = Minimize(objValue)
    new NaiveSumDistancesInvariant(model, vrs.routes, distances, objValue)
    model.close()
    model.propagate()

    val nodesToMove = () => vrs.routedWithoutVehicles.pendingValue

    (vrs, nodesToMove, objective)
  }

  ignore("OnePointMove neighborhood works as expected") {
    val (vrs, nodesToMove, objective) = getBasicModelForTest
    val relevantDestination =
      (x: Int) => vrs.routedWithVehicles.value().filter((n: Int) => n != x && n != x - 1)

    println(s"Initial routing: $vrs")
    val search = OnePointMoveNeighborhood(
      vrs,
      nodesToMove,
      relevantDestination,
      selectDestinationBehavior = LoopBehavior.best()
    )
    search.verbosityLevel = 4
    search.doAllMoves(objective)

    println(s"Routing after search: $vrs")
  }

}
