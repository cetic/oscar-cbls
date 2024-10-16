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
import org.scalatest.matchers.must.Matchers
import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.Minimize
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.lib.neighborhoods.routing.TwoOpt
import oscar.cbls.modeling.routing.VRP
import oscar.cbls.test.lib.neighborhoods.ToolsForTestingNeighborhood.generateRandomValidRoute

import scala.util.Random

class TwoOptTests extends AnyFunSuite with Matchers {

  private def generateManhattanDistMatrix(points: Array[(Int, Int)]): Array[Array[Long]] = {
    def dist(x1: (Int, Int), x2: (Int, Int)): Long = (x1._1 - x2._1).abs + (x1._2 - x2._2).abs

    Array.tabulate(points.length, points.length)((i: Int, j: Int) => dist(points(i), points(j)))
  }

  private val points: Array[(Int, Int)] =
    Array(
      (0, 0),
      (0, -10),
      (-2, 4),
      (-2, 6),
      (2, 6),
      (2, 4),
      (2, -14),
      (2, -16),
      (-2, -16),
      (-2, -14)
    )

  private val distMatrix: Array[Array[Long]] = generateManhattanDistMatrix(points)

  test("2-opt works as expected") {
    val model  = new Store()
    val vrp    = VRP(model, 10, 2, debug = true)
    val output = IntVariable(model, 0L)
    new NaiveSumDistancesInvariant(model, vrp.routes, distMatrix, output)
    val obj = Minimize(output)
    model.close()
    vrp.routes := IntSequence(List(0, 4, 3, 5, 2, 1, 9, 6, 8, 7))
    model.propagate()

    val routedNodeExceptVehicle = vrp.routed.value().diff((0 until vrp.v).toSet)

    val relevantStartSegment = () => routedNodeExceptVehicle

    val search = TwoOpt(vrp, relevantStartSegment)

    noException mustBe thrownBy(search.doAllMoves(obj))
    val routes = vrp.mapVehicleToRoute

    routes(0) must (contain inOrderOnly (0, 2, 3, 4, 5) or contain inOrderOnly (0, 5, 4, 3, 2))
    routes(1) must (contain inOrderOnly (1, 6, 7, 8, 9) or contain inOrderOnly (1, 9, 8, 7, 6))
  }

  ignore("2-opt verbose mode with first loop") {
    val model  = new Store()
    val vrp    = VRP(model, 10, 2, debug = true)
    val output = IntVariable(model, 0L)
    new NaiveSumDistancesInvariant(model, vrp.routes, distMatrix, output)
    val obj = Minimize(output)
    model.close()

    val seed = Random.nextLong()
    val rng  = new Random(seed)

    vrp.routes := generateRandomValidRoute(vrp.n, vrp.v, rng)
    model.propagate()

    val routedNodeExceptVehicle = vrp.routed.value().diff((0 until vrp.v).toSet)

    val relevantStartSegment = () => routedNodeExceptVehicle
    val search =
      TwoOpt(vrp, relevantStartSegment, selectFlippedSegmentBehavior = LoopBehavior.first())
    search.verbosityLevel = 4

    println(s"Routes before search $vrp")
    withClue(s"Working with seed $seed") {
      noException mustBe thrownBy(search.doAllMoves(obj))
    }
    search.displayProfiling()
    println(s"Routes after search $vrp")
  }

  ignore("2-opt verbose mode with best loop") {
    val model  = new Store()
    val vrp    = VRP(model, 10, 2, debug = true)
    val output = IntVariable(model, 0L)
    new NaiveSumDistancesInvariant(model, vrp.routes, distMatrix, output)
    val obj = Minimize(output)
    model.close()

    val seed = Random.nextLong()
    val rng  = new Random(seed)

    vrp.routes := generateRandomValidRoute(vrp.n, vrp.v, rng)
    model.propagate()

    val routedNodeExceptVehicle = vrp.routed.value().diff((0 until vrp.v).toSet)

    val relevantStartSegment = () => routedNodeExceptVehicle
    val search =
      TwoOpt(vrp, relevantStartSegment, selectFlippedSegmentBehavior = LoopBehavior.best())
    search.verbosityLevel = 4

    println(s"Routes before search $vrp")
    withClue(s"Working with seed $seed") {
      noException mustBe thrownBy(search.doAllMoves(obj))
    }
    search.displayProfiling()
    println(s"Routes after search $vrp")
  }

  ignore("2-opt can only flip segment of length at most 2") {
    val model  = new Store()
    val vrp    = VRP(model, 10, 2, debug = true)
    val output = IntVariable(model, 0L)
    new NaiveSumDistancesInvariant(model, vrp.routes, distMatrix, output)
    val obj = Minimize(output)
    model.close()

    val seed = Random.nextLong()
    val rng  = new Random(seed)

    vrp.routes := generateRandomValidRoute(vrp.n, vrp.v, rng)
    model.propagate()

    val routedNodeExceptVehicle = vrp.routed.value().diff((0 until vrp.v).toSet)

    val relevantStartSegment = () => routedNodeExceptVehicle
    val search =
      TwoOpt(vrp, relevantStartSegment, Some(2), selectFlippedSegmentBehavior = LoopBehavior.best())
    search.verbosityLevel = 4

    println(s"Routes before search $vrp")
    withClue(s"Working with seed $seed") {
      noException mustBe thrownBy(search.doAllMoves(obj))
    }
    search.displayProfiling()
    println(s"Routes after search $vrp")
  }

}
