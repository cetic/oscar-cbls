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
import oscar.cbls.lib.invariant.numeric.Sum2
import oscar.cbls.lib.neighborhoods.routing.{
  InsertPointNeighborhoodInsertionPointFirst,
  InsertPointNeighborhoodUnroutedFirst
}
import oscar.cbls.modeling.routing.VRP

import scala.util.Random

class InsertPointNeighborhoodTestSuite extends AnyFunSuite {

  private[this] def getTestBasicModel: (VRP, Minimize) = {
    val seed: Long = 6553200370069564662L // Random.nextLong()
    println(s"\nSeed: $seed")

    val v = 2
    val n = 10

    val model = new Store(debugLevel = 3)
    val vrp   = VRP(model, n, v, debug = true)
    val (_, dist, unroutedCost, _) =
      RoutingGenerator.generateRandomRoutingData(n, 0L, 0L, seed)
    val sumDist   = IntVariable(model, 0L)
    val sumCost   = IntVariable(model, 0L)
    val objVal    = IntVariable(model, 0L)
    val objective = Minimize(objVal)
    new NaiveSumDistancesInvariant(model, vrp.routes, dist, sumDist)
    new NaiveUnroutedCostInvariant(model, vrp.routes, n, unroutedCost, sumCost)
    Sum2(model, sumDist, sumCost, objVal)
    model.close()

    (vrp, objective)
  }

  ignore("InsertPointNeighborhoodUnroutedFirst works as expected") {
    val (vrp, objective) = getTestBasicModel

    val valuesToInsert      = () => vrp.unroutedNodes
    val relevantInsertPoint = (_: Int) => vrp.routedWithVehicles.value()

    println(s"Initial routing: $vrp")
    val search = InsertPointNeighborhoodUnroutedFirst(
      vrp,
      valuesToInsert,
      relevantInsertPoint,
      selectInsertionAfterPointBehavior = LoopBehavior.best()
    )
    search.verbosityLevel = 4
    search.doAllMoves(objective)

    println(s"Routing after search: $vrp")

  }

  ignore("InsertPointNeighborhoodInsertionPointFirst works as expected") {
    val (vrp, objective) = getTestBasicModel

    val insertionNode  = () => vrp.routedWithVehicles.value()
    val relevantValues = (_: Int) => vrp.unroutedNodes

    println(s"Initial routing: $vrp")
    val search = InsertPointNeighborhoodInsertionPointFirst(
      vrp,
      insertionNode,
      relevantValues,
      selectInsertionPointBehavior = LoopBehavior.best()
    )
    search.verbosityLevel = 4
    search.doAllMoves(objective)

    println(s"Routing after search: $vrp")
  }

}
