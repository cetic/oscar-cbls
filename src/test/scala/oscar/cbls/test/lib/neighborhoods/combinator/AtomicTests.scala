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

package oscar.cbls.test.lib.neighborhoods.combinator

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.objective.Minimize
import oscar.cbls.core.search.MoveFound
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.lib.invariant.routing.TotalRouteLength
import oscar.cbls.lib.neighborhoods.combinator.Atomic
import oscar.cbls.lib.neighborhoods.routing.TwoOpt
import oscar.cbls.modeling.routing.VRP

class AtomicTests extends AnyFunSuite with Matchers {

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

  test("Aggregated Atomic combinator works as expected") {
    val model = new Store()
    val vrp   = VRP(model, 10, 2, debug = true)
    val inv   = TotalRouteLength(vrp, distMatrix)
    val obj   = Minimize(inv.routeLength)
    model.close()
    vrp.routes := IntSequence(List(0, 4, 3, 5, 2, 1, 9, 6, 8, 7))
    model.propagate()

    val relevantStartSegment = () => vrp.routedWithoutVehicles.pendingValue

    val twoOpt = TwoOpt(
      vrp,
      relevantStartSegment,
      maxSegmentLength = None,
      selectFlippedSegmentBehavior = LoopBehavior.first(),
      hotRestart = true
    )
    val search = Atomic(twoOpt, aggregateIntoSingleMove = true)
    search.verbosityLevel = 0
    search.profileSearch()

    noException mustBe thrownBy(search.doAllMoves(obj))

    obj.objValue.value() must be(40)
    val routes = vrp.mapVehicleToRoute

    routes(0) must (contain inOrderOnly (0, 2, 3, 4, 5) or contain inOrderOnly (0, 5, 4, 3, 2))
    routes(1) must (contain inOrderOnly (1, 6, 7, 8, 9) or contain inOrderOnly (1, 9, 8, 7, 6))
  }

  test("Aggregated Atomic combinator with shouldStop works as expected") {
    val model = new Store()
    val vrp   = VRP(model, 10, 2, debug = true)
    val inv   = TotalRouteLength(vrp, distMatrix)
    val obj   = Minimize(inv.routeLength)
    model.close()
    vrp.routes := IntSequence(List(0, 4, 3, 5, 2, 1, 9, 6, 8, 7))
    model.propagate()

    val relevantStartSegment = () => vrp.routedWithoutVehicles.pendingValue

    val twoOpt = TwoOpt(
      vrp,
      relevantStartSegment,
      maxSegmentLength = None,
      selectFlippedSegmentBehavior = LoopBehavior.first(),
      hotRestart = true
    )
    val search = Atomic(twoOpt, aggregateIntoSingleMove = true, shouldStop = _ >= 2)
    search.profileSearch()

    val res = search.getMove(obj)
    res match {
      case mf: MoveFound =>
        search.commitMove(obj, mf.move)
      case _ =>
    }

    obj.objValue.value() must be(48)
    val routes = vrp.mapVehicleToRoute

    routes(0) must contain inOrderOnly (0, 4, 3, 2, 5)
    routes(1) must contain inOrderOnly (1, 9, 8, 6, 7)
  }

  test("Not Aggregated Atomic combinator works as expected") {
    val model = new Store()
    val vrp   = VRP(model, 10, 2, debug = true)
    val inv   = TotalRouteLength(vrp, distMatrix)
    val obj   = Minimize(inv.routeLength)
    model.close()
    vrp.routes := IntSequence(List(0, 4, 3, 5, 2, 1, 9, 6, 8, 7))
    model.propagate()

    val relevantStartSegment = () => vrp.routedWithoutVehicles.pendingValue

    val twoOpt = TwoOpt(
      vrp,
      relevantStartSegment,
      maxSegmentLength = None,
      selectFlippedSegmentBehavior = LoopBehavior.first(),
      hotRestart = true
    )
    val search = Atomic(twoOpt, aggregateIntoSingleMove = false)
    search.verbosityLevel = 0
    search.profileSearch()

    noException mustBe thrownBy(search.doAllMoves(obj))

    obj.objValue.value() must be(40)
    val routes = vrp.mapVehicleToRoute

    routes(0) must (contain inOrderOnly (0, 2, 3, 4, 5) or contain inOrderOnly (0, 5, 4, 3, 2))
    routes(1) must (contain inOrderOnly (1, 6, 7, 8, 9) or contain inOrderOnly (1, 9, 8, 7, 6))
  }

  test("Not Aggregated Atomic combinator with shouldStop works as expected") {
    val model = new Store()
    val vrp   = VRP(model, 10, 2, debug = true)
    val inv   = TotalRouteLength(vrp, distMatrix)
    val obj   = Minimize(inv.routeLength)
    model.close()
    vrp.routes := IntSequence(List(0, 4, 3, 5, 2, 1, 9, 6, 8, 7))
    model.propagate()

    val relevantStartSegment = () => vrp.routedWithoutVehicles.pendingValue

    val twoOpt = TwoOpt(
      vrp,
      relevantStartSegment,
      maxSegmentLength = None,
      selectFlippedSegmentBehavior = LoopBehavior.first(),
      hotRestart = true
    )
    val search = Atomic(twoOpt, aggregateIntoSingleMove = false, shouldStop = _ >= 2)
    search.profileSearch()

    val res = search.getMove(obj)
    res match {
      case mf: MoveFound =>
        search.commitMove(obj, mf.move)
      case _ =>
    }

    obj.objValue.value() must be(48)
    val routes = vrp.mapVehicleToRoute

    routes(0) must contain inOrderOnly (0, 4, 3, 2, 5)
    routes(1) must contain inOrderOnly (1, 9, 8, 6, 7)
  }

  ignore("Aggregated Atomic combinator verbose mode") {
    val model = new Store()
    val vrp   = VRP(model, 10, 2, debug = true)
    val inv   = TotalRouteLength(vrp, distMatrix)
    val obj   = Minimize(inv.routeLength)
    model.close()
    vrp.routes := IntSequence(List(0, 4, 3, 5, 2, 1, 9, 6, 8, 7))
    model.propagate()

    val relevantStartSegment = () => vrp.routedWithoutVehicles.pendingValue

    val twoOpt = TwoOpt(
      vrp,
      relevantStartSegment,
      maxSegmentLength = None,
      selectFlippedSegmentBehavior = LoopBehavior.first(),
      hotRestart = true
    )
    twoOpt.verbosityLevel = 2
    val search = Atomic(twoOpt, aggregateIntoSingleMove = true)
    search.verbosityLevel = 4
    search.profileSearch()

    noException mustBe thrownBy(search.doAllMoves(obj))
    search.displayProfiling()

    obj.objValue.value() must be(40)
    val routes = vrp.mapVehicleToRoute

    routes(0) must (contain inOrderOnly (0, 2, 3, 4, 5) or contain inOrderOnly (0, 5, 4, 3, 2))
    routes(1) must (contain inOrderOnly (1, 6, 7, 8, 9) or contain inOrderOnly (1, 9, 8, 7, 6))
  }

  ignore("Not Aggregated Atomic combinator verbose mode") {
    val model = new Store()
    val vrp   = VRP(model, 10, 2, debug = true)
    val inv   = TotalRouteLength(vrp, distMatrix)
    val obj   = Minimize(inv.routeLength)
    model.close()
    vrp.routes := IntSequence(List(0, 4, 3, 5, 2, 1, 9, 6, 8, 7))
    model.propagate()

    val relevantStartSegment = () => vrp.routedWithoutVehicles.pendingValue

    val twoOpt = TwoOpt(
      vrp,
      relevantStartSegment,
      maxSegmentLength = None,
      selectFlippedSegmentBehavior = LoopBehavior.first(),
      hotRestart = true
    )
    val search = Atomic(twoOpt, aggregateIntoSingleMove = false)
    search.verbosityLevel = 4
    search.profileSearch()

    noException mustBe thrownBy(search.doAllMoves(obj))
    search.displayProfiling()

    obj.objValue.value() must be(40)
    val routes = vrp.mapVehicleToRoute

    routes(0) must (contain inOrderOnly (0, 2, 3, 4, 5) or contain inOrderOnly (0, 5, 4, 3, 2))
    routes(1) must (contain inOrderOnly (1, 6, 7, 8, 9) or contain inOrderOnly (1, 9, 8, 7, 6))
  }

}
