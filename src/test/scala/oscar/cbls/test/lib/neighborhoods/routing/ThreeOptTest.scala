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
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.Minimize
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.lib.invariant.routing.TotalRouteLength
import oscar.cbls.lib.neighborhoods.routing.ThreeOpt
import oscar.cbls.modeling.routing.VRS
import oscar.cbls.test.lib.neighborhoods.ToolsForTestingNeighborhood.generateRandomValidRoute

import scala.util.Random

class ThreeOptTest extends AnyFunSuite with Matchers {

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

  test("3-Opt insertion point first works as expected") {
    val model  = new Store()
    val vrs    = VRS(model, 10, 2, debug = true)
    val output = TotalRouteLength(vrs, distMatrix).routeLength
    val obj    = Minimize(output)
    model.close()

    val seed = Random.nextLong()
    val rng  = new Random(seed)

    vrs.routes := generateRandomValidRoute(vrs.n, vrs.v, rng)
    model.propagate()

    val relevantInsertPoints = () => vrs.routedWithVehicles.pendingValue
    val relevantStartSeg     = (x: Int) => vrs.routedWithoutVehicles.pendingValue.diff(Set(x))

    val search = ThreeOpt.insertionPointFirst(
      vrs,
      relevantInsertPoints,
      relevantStartSeg,
      vrs.n,
      selectInsertPointBehavior = LoopBehavior.first(),
      selectMovedSegmentBehavior = LoopBehavior.first(),
      selectFlipBehavior = LoopBehavior.best()
    )

    withClue(s"Working with seed $seed\n") {
      noException mustBe thrownBy(search.doAllMoves(obj))
      val routes = vrs.mapVehicleToRoute
      routes(0) must (contain inOrderOnly (0, 2, 3, 4, 5) or contain inOrderOnly (0, 5, 4, 3, 2))
      routes(1) must (contain inOrderOnly (1, 6, 7, 8, 9) or contain inOrderOnly (1, 9, 8, 7, 6))
    }
  }

  test("3-Opt moved segment first works as expected") {
    val model  = new Store()
    val vrs    = VRS(model, 10, 2, debug = true)
    val output = IntVariable(model, 0L)
    new NaiveSumDistancesInvariant(model, vrs.routes, distMatrix, output)
    val obj = Minimize(output)
    model.close()

    val seed = Random.nextLong()
    val rng  = new Random(seed)

    vrs.routes := generateRandomValidRoute(vrs.n, vrs.v, rng)
    model.propagate()

    val relevantInsertPoints = (x: Int) => vrs.routedWithVehicles.pendingValue.diff(Set(x))
    val relevantStartSeg     = () => vrs.routedWithoutVehicles.pendingValue

    val search = ThreeOpt.movedSegmentFirst(
      vrs,
      relevantStartSeg,
      relevantInsertPoints,
      vrs.n,
      selectMovedSegmentBehavior = LoopBehavior.first(),
      selectInsertPointBehavior = LoopBehavior.first(),
      selectFlipBehavior = LoopBehavior.best()
    )

    withClue(s"Working with seed $seed\n") {
      noException mustBe thrownBy(search.doAllMoves(obj))
      val routes = vrs.mapVehicleToRoute
      routes(0) must contain only (0, 2, 3, 4, 5)
      routes(1) must contain only (1, 6, 7, 8, 9)
    }

  }

  ignore("Test 3-Opt insertion first verbose mode breaking symmetries") {
    val model  = new Store()
    val vrs    = VRS(model, 10, 2, debug = true)
    val output = IntVariable(model, 0L)
    new NaiveSumDistancesInvariant(model, vrs.routes, distMatrix, output)
    val obj = Minimize(output)
    model.close()

    val seed = Random.nextLong()
    val rng  = new Random(seed)

    vrs.routes := generateRandomValidRoute(vrs.n, vrs.v, rng)
    model.propagate()
    println(vrs.routes)

    val relevantInsertPoints = () => vrs.routedWithVehicles.pendingValue
    val relevantStartSeg     = (x: Int) => vrs.routedWithoutVehicles.pendingValue.diff(Set(x))

    val search = ThreeOpt.insertionPointFirst(
      vrs,
      relevantInsertPoints,
      relevantStartSeg,
      vrs.n,
      selectInsertPointBehavior = LoopBehavior.first(),
      selectMovedSegmentBehavior = LoopBehavior.first(),
      selectFlipBehavior = LoopBehavior.best()
    )
    search.verbosityLevel = 4

    println(s"Routes before search $vrs")
    withClue(s"Working with seed $seed\n") {
      noException mustBe thrownBy(search.doAllMoves(obj))
    }
    search.displayProfiling()
    println(s"Routes after search $vrs")

  }

  ignore("Test 3-Opt insertion first verbose mode not breaking symmetries") {
    val model  = new Store()
    val vrs    = VRS(model, 10, 2, debug = true)
    val output = IntVariable(model, 0L)
    new NaiveSumDistancesInvariant(model, vrs.routes, distMatrix, output)
    val obj = Minimize(output)
    model.close()

    val seed = Random.nextLong()
    val rng  = new Random(seed)

    vrs.routes := generateRandomValidRoute(vrs.n, vrs.v, rng)
    model.propagate()
    println(vrs.routes)

    val relevantInsertPoints = () => vrs.routedWithVehicles.pendingValue
    val relevantStartSeg     = (x: Int) => vrs.routedWithoutVehicles.pendingValue.diff(Set(x))

    val search = ThreeOpt.insertionPointFirst(
      vrs,
      relevantInsertPoints,
      relevantStartSeg,
      vrs.n,
      selectInsertPointBehavior = LoopBehavior.first(),
      selectMovedSegmentBehavior = LoopBehavior.first(),
      selectFlipBehavior = LoopBehavior.best(),
      breakSymmetry = false
    )
    search.verbosityLevel = 4

    println(s"Routes before search $vrs")
    withClue(s"Working with seed $seed\n") {
      noException mustBe thrownBy(search.doAllMoves(obj))
    }
    search.displayProfiling()
    println(s"Routes after search $vrs")
  }

  ignore("3-Opt moved segment first verbose mode breaking symmetries") {
    val model  = new Store()
    val vrs    = VRS(model, 10, 2, debug = true)
    val output = IntVariable(model, 0L)
    new NaiveSumDistancesInvariant(model, vrs.routes, distMatrix, output)
    val obj = Minimize(output)
    model.close()

    val seed = Random.nextLong()
    val rng  = new Random(seed)

    vrs.routes := generateRandomValidRoute(vrs.n, vrs.v, rng)
    model.propagate()

    val relevantInsertPoints = (x: Int) => vrs.routedWithVehicles.pendingValue.diff(Set(x))
    val relevantStartSeg     = () => vrs.routedWithoutVehicles.pendingValue

    val search = ThreeOpt.movedSegmentFirst(
      vrs,
      relevantStartSeg,
      relevantInsertPoints,
      vrs.n,
      selectMovedSegmentBehavior = LoopBehavior.first(),
      selectInsertPointBehavior = LoopBehavior.first(),
      selectFlipBehavior = LoopBehavior.best()
    )
    search.verbosityLevel = 4

    println(s"Routes before search $vrs")
    withClue(s"Working with seed $seed\n") {
      noException mustBe thrownBy(search.doAllMoves(obj))
    }

    search.displayProfiling()
    println(s"Routes after search $vrs")

  }

  ignore("3-Opt moved segment first verbose mode not breaking symmetries") {
    val model  = new Store()
    val vrs    = VRS(model, 10, 2, debug = true)
    val output = IntVariable(model, 0L)
    new NaiveSumDistancesInvariant(model, vrs.routes, distMatrix, output)
    val obj = Minimize(output)
    model.close()

    val seed = Random.nextLong()
    val rng  = new Random(seed)

    vrs.routes := generateRandomValidRoute(vrs.n, vrs.v, rng)
    model.propagate()

    val relevantInsertPoints = (x: Int) => vrs.routedWithVehicles.pendingValue.diff(Set(x))
    val relevantStartSeg     = () => vrs.routedWithoutVehicles.pendingValue

    val search = ThreeOpt.movedSegmentFirst(
      vrs,
      relevantStartSeg,
      relevantInsertPoints,
      vrs.n,
      selectMovedSegmentBehavior = LoopBehavior.first(),
      selectInsertPointBehavior = LoopBehavior.first(),
      selectFlipBehavior = LoopBehavior.best(),
      breakSymmetry = false
    )
    search.verbosityLevel = 4

    println(s"Routes before search $vrs")
    withClue(s"Working with seed $seed\n") {
      noException mustBe thrownBy(search.doAllMoves(obj))
    }

    search.displayProfiling()
    println(s"Routes after search $vrs")

  }

}
