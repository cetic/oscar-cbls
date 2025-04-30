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
import oscar.cbls._
import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntConstant
import oscar.cbls.core.computation.objective.Minimize
import oscar.cbls.core.search.Move
import oscar.cbls.lib.invariant.routing.TotalRouteLength
import oscar.cbls.lib.neighborhoods.combinator.{AndThen, BackTrackedEjectionChains, EjectionChains}
import oscar.cbls.lib.neighborhoods.routing._
import oscar.cbls.modeling.routing.VRS

class EjectionChainsTest extends AnyFunSuite with Matchers {

  private val points: Array[(Int, Int)] = Array((0, 0), (-2, 4), (-2, 6), (2, 6), (2, 4))

  private def generateManhattanDistMatrix(points: Array[(Int, Int)]): Array[Array[Long]] = {
    def dist(x1: (Int, Int), x2: (Int, Int)): Long = (x1._1 - x2._1).abs + (x1._2 - x2._2).abs

    Array.tabulate(points.length, points.length)((i: Int, j: Int) => dist(points(i), points(j)))
  }

  test("Ejection chains works as expected") {
    val model: Store                 = new Store()
    val vrs: VRS                     = VRS(model, 5, 1)
    val dist                         = generateManhattanDistMatrix(points)
    val routeLength: IntVariable     = TotalRouteLength(vrs, dist).routeLength
    val unroutedPenalty: IntVariable = vrs.unrouted.size() * IntConstant(model, 10)
    val obj: Objective               = Minimize(routeLength + unroutedPenalty)

    vrs.routes := IntSequence(List(0, 3, 2, 4, 1))
    model.close()

    val nextNeigh: List[Move] => Option[Neighborhood] = (prevMoves: List[Move]) => {
      if (prevMoves.length >= 10)
        None
      else if (prevMoves.length % 2 == 0)
        Some(RemovePoint(vrs, () => vrs.routedWithoutVehicles.pendingValue))
      else {
        Some(
          InsertPointUnroutedFirst(
            vrs,
            () => vrs.unroutedNodes,
            _ => vrs.routedWithVehicles.pendingValue
          )
        )
      }
    }

    val search = EjectionChains(
      nextNeigh,
      relaxedObjective = None,
      relaxedAcceptanceCriterion = Some((oldObj, newObj) => newObj < oldObj + 5),
      stopAtFirstImprovement = false
    )

    noException mustBe thrownBy(search.doAllMoves(obj))

    obj.objValue.value() must be(20)
    vrs.routeOfVehicle(0) must contain inOrderOnly (0, 1, 2, 3, 4)
  }

  test("Ejection chains with early stop works as expected") {
    val model: Store                 = new Store()
    val vrs: VRS                     = VRS(model, 5, 1)
    val dist                         = generateManhattanDistMatrix(points)
    val routeLength: IntVariable     = TotalRouteLength(vrs, dist).routeLength
    val unroutedPenalty: IntVariable = vrs.unrouted.size() * IntConstant(model, 10)
    val obj: Objective               = Minimize(routeLength + unroutedPenalty)

    vrs.routes := IntSequence(List(0, 3, 2, 4, 1))
    model.close()

    val nextNeigh: List[Move] => Option[Neighborhood] = (prevMoves: List[Move]) => {
      if (prevMoves.length >= 10)
        None
      else if (prevMoves.length % 2 == 0)
        Some(RemovePoint(vrs, () => vrs.routedWithoutVehicles.pendingValue))
      else {
        Some(
          InsertPointUnroutedFirst(
            vrs,
            () => vrs.unroutedNodes,
            _ => vrs.routedWithVehicles.pendingValue
          )
        )
      }
    }

    val search = EjectionChains(
      nextNeigh,
      relaxedObjective = None,
      relaxedAcceptanceCriterion = Some((oldObj, newObj) => newObj < oldObj + 5),
      stopAtFirstImprovement = true
    )

    noException mustBe thrownBy(search.doAllMoves(obj))

    obj.objValue.value() must be(24)
    vrs.routeOfVehicle(0) must contain inOrderOnly (0, 2, 3, 4, 1)

  }

  test("Ejection chains with accumulator works as expected") {
    val model: Store                 = new Store()
    val vrs: VRS                     = VRS(model, 5, 1)
    val dist                         = generateManhattanDistMatrix(points)
    val routeLength: IntVariable     = TotalRouteLength(vrs, dist).routeLength
    val unroutedPenalty: IntVariable = vrs.unrouted.size() * IntConstant(model, 10)
    val obj: Objective               = Minimize(routeLength + unroutedPenalty)

    vrs.routes := IntSequence(List(0, 3, 2, 4, 1))
    model.close()

    val nextNeigh: (Int, List[Move]) => Option[(Int, Neighborhood)] =
      (acc: Int, _: List[Move]) => {
        if (acc >= 10)
          None
        else if (acc % 2 == 0)
          Some((acc + 1, RemovePoint(vrs, () => vrs.routedWithoutVehicles.pendingValue)))
        else {
          Some(
            (
              acc + 1,
              InsertPointUnroutedFirst(
                vrs,
                () => vrs.unroutedNodes,
                _ => vrs.routedWithVehicles.pendingValue
              )
            )
          )
        }
      }

    val search = EjectionChains.fold(0)(
      nextNeigh,
      relaxedObjective = None,
      relaxedAcceptanceCriterion = Some((oldObj, newObj) => newObj < oldObj + 5),
      stopAtFirstImprovement = false
    )

    noException mustBe thrownBy(search.doAllMoves(obj))

    obj.objValue.value() must be(20)
    vrs.routeOfVehicle(0) must contain inOrderOnly (0, 1, 2, 3, 4)
  }

  test("Ejection chains as right neighborhood of DynAndThen") {
    val points: Array[(Int, Int)] =
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

    val distMatrix: Array[Array[Long]] = generateManhattanDistMatrix(points)

    val model  = new Store()
    val vrs    = VRS(model, 10, 2, debug = true)
    val output = TotalRouteLength(vrs, distMatrix).routeLength

    val obj = Minimize(output)
    model.close()
    vrs.routes := IntSequence(List(0, 4, 3, 5, 2, 1, 9, 6, 8, 7))
    model.propagate()

    val routedNodeExceptVehicle = vrs.routedWithoutVehicles.pendingValue

    val relevantStartSegment = () => routedNodeExceptVehicle

    val nodesToMove = () => vrs.routedWithoutVehicles.pendingValue
    val relevantInsertPoint = (x: Int) => {
      val xExp = vrs.routes.pendingValue.explorerAtAnyOccurrence(x).get
      val prev = xExp.prev.value
      vrs.routedWithVehicles.pendingValue.diff(Set(x, prev))
    }

    val twoOpt         = TwoOpt(vrs, relevantStartSegment)
    val onePoint       = OnePointMove(vrs, nodesToMove, relevantInsertPoint)
    val ejectionChains = EjectionChains(_ => Some(onePoint))

    val search = AndThen(twoOpt, ejectionChains)
    noException mustBe thrownBy(search.doAllMoves(obj))

    obj.objValue.value() must be(40)
    val routes = vrs.mapVehicleToRoute
    routes(0) must (contain inOrderOnly (0, 2, 3, 4, 5) or contain inOrderOnly (0, 5, 4, 3, 2))
    routes(1) must (contain inOrderOnly (1, 6, 7, 8, 9) or contain inOrderOnly (1, 9, 8, 7, 6))

  }

  test("BackTrackedEjectionChains works as expected") {
    val model: Store                 = new Store()
    val vrs: VRS                     = VRS(model, 5, 1)
    val dist                         = generateManhattanDistMatrix(points)
    val routeLength: IntVariable     = TotalRouteLength(vrs, dist).routeLength
    val unroutedPenalty: IntVariable = vrs.unrouted.size() * IntConstant(model, 10)
    val obj: Objective               = Minimize(routeLength + unroutedPenalty)

    model.close()

    val firstNeigh = InsertPointUnroutedFirst(
      vrs,
      () => vrs.unroutedNodes,
      _ => vrs.routedWithVehicles.pendingValue
    )

    val nextNeigh: Move => List[Move] => Option[Neighborhood] = (first: Move) =>
      (prevMoves: List[Move]) => {
        val prevMove: InsertPointMove =
          if (prevMoves.isEmpty) first.asInstanceOf[InsertPointMove]
          else prevMoves.head.asInstanceOf[InsertPointMove]

        if (vrs.unroutedNodes.isEmpty) None
        else {
          val lastInserted = prevMove.nodeToInsert
          val nextToInsert =
            vrs.unroutedNodes.minBy(dist(lastInserted)(_))
          val next =
            InsertPointInsertionPointFirst(vrs, () => List(lastInserted), _ => List(nextToInsert))
          Some(next)
        }
      }

    val search = BackTrackedEjectionChains(
      firstNeigh,
      nextNeigh,
      relaxedObjective = None,
      relaxedAcceptanceCriterion = None,
      stopAtFirstImprovement = false
    )

    noException mustBe thrownBy(search.doAllMoves(obj))
    obj.objValue.value() must be(20)
    vrs.routeOfVehicle(0) must contain inOrderOnly (0, 1, 2, 3, 4)

  }

  test("BackTrackedEjectionChains with early stop works as expected") {
    val model: Store                 = new Store()
    val vrs: VRS                     = VRS(model, 5, 1)
    val dist                         = generateManhattanDistMatrix(points)
    val routeLength: IntVariable     = TotalRouteLength(vrs, dist).routeLength
    val unroutedPenalty: IntVariable = vrs.unrouted.size() * IntConstant(model, 10)
    val obj: Objective               = Minimize(routeLength + unroutedPenalty)

    model.close()

    val firstNeigh = InsertPointUnroutedFirst(
      vrs,
      () => vrs.unroutedNodes,
      _ => vrs.routedWithVehicles.pendingValue
    )

    val nextNeigh: Move => List[Move] => Option[Neighborhood] = (first: Move) =>
      (prevMoves: List[Move]) => {
        val prevMove: InsertPointMove =
          if (prevMoves.isEmpty) first.asInstanceOf[InsertPointMove]
          else prevMoves.head.asInstanceOf[InsertPointMove]

        if (vrs.unroutedNodes.isEmpty) None
        else {
          val lastInserted = prevMove.nodeToInsert
          val nextToInsert =
            vrs.unroutedNodes.minBy(dist(lastInserted)(_))
          val next =
            InsertPointInsertionPointFirst(vrs, () => List(lastInserted), _ => List(nextToInsert))
          Some(next)
        }
      }

    val search = BackTrackedEjectionChains(
      firstNeigh,
      nextNeigh,
      relaxedObjective = None,
      relaxedAcceptanceCriterion = None,
      stopAtFirstImprovement = true
    )

    noException mustBe thrownBy(search.doAllMoves(obj))
    obj.objValue.value() must be(24)
    vrs.routeOfVehicle(0) must contain inOrderOnly (0, 3, 4, 1, 2)

  }

  test("BackTrackedEjectionChains with accumulator works as expected") {

    val model: Store                 = new Store()
    val vrs: VRS                     = VRS(model, 5, 1)
    val dist                         = generateManhattanDistMatrix(points)
    val routeLength: IntVariable     = TotalRouteLength(vrs, dist).routeLength
    val unroutedPenalty: IntVariable = vrs.unrouted.size() * IntConstant(model, 10)
    val obj: Objective               = Minimize(routeLength + unroutedPenalty)

    model.close()

    val firstNeigh = InsertPointUnroutedFirst(
      vrs,
      () => vrs.unroutedNodes,
      _ => vrs.routedWithVehicles.pendingValue
    )

    val nextNeigh: Move => (Int, List[Move]) => Option[(Int, Neighborhood)] = (first: Move) =>
      (acc: Int, prevMoves: List[Move]) => {
        val prevMove: InsertPointMove =
          if (prevMoves.isEmpty) first.asInstanceOf[InsertPointMove]
          else prevMoves.head.asInstanceOf[InsertPointMove]

        if (acc > 2 || vrs.unroutedNodes.isEmpty) None
        else {
          val lastInserted = prevMove.nodeToInsert
          val nextToInsert =
            vrs.unroutedNodes.minBy(dist(lastInserted)(_))
          val next =
            InsertPointInsertionPointFirst(vrs, () => List(lastInserted), _ => List(nextToInsert))
          Some(acc + 1, next)
        }
      }

    val search = BackTrackedEjectionChains.fold(1)(
      firstNeigh,
      nextNeigh,
      relaxedObjective = None,
      relaxedAcceptanceCriterion = None,
      stopAtFirstImprovement = false
    )

    noException mustBe thrownBy(search.doAllMoves(obj))
    obj.objValue.value() must be(30)
    vrs.routeOfVehicle(0) must contain inOrderOnly (0, 1, 2, 3)

  }

  ignore("EjectionChains profiling") {
    val model: Store                 = new Store()
    val vrs: VRS                     = VRS(model, 5, 1)
    val dist                         = generateManhattanDistMatrix(points)
    val routeLength: IntVariable     = TotalRouteLength(vrs, dist).routeLength
    val unroutedPenalty: IntVariable = vrs.unrouted.size() * IntConstant(model, 10)
    val obj: Objective               = Minimize(routeLength + unroutedPenalty)

    vrs.routes := IntSequence(List(0, 3, 2, 4, 1))
    model.close()

    val nextNeigh: List[Move] => Option[Neighborhood] = (prevMoves: List[Move]) => {
      if (prevMoves.length >= 10)
        None
      else if (prevMoves.length % 2 == 0)
        Some(RemovePoint(vrs, () => vrs.routedWithoutVehicles.pendingValue))
      else {
        Some(
          InsertPointUnroutedFirst(
            vrs,
            () => vrs.unroutedNodes,
            _ => vrs.routedWithVehicles.pendingValue
          )
        )
      }
    }

    val search = EjectionChains(
      nextNeigh,
      relaxedObjective = None,
      relaxedAcceptanceCriterion = Some((oldObj, newObj) => newObj < oldObj + 5),
      stopAtFirstImprovement = false
    )
    search.profileSearch()
    search.verbosityLevel = 3

    search.doAllMoves(obj)
    search.displayProfiling()

    println(vrs)
  }

  ignore("BackTrackedEjectionChains profiling") {
    val model: Store                 = new Store()
    val vrs: VRS                     = VRS(model, 5, 1)
    val dist                         = generateManhattanDistMatrix(points)
    val routeLength: IntVariable     = TotalRouteLength(vrs, dist).routeLength
    val unroutedPenalty: IntVariable = vrs.unrouted.size() * IntConstant(model, 10)
    val obj: Objective               = Minimize(routeLength + unroutedPenalty)

    model.close()

    val firstNeigh = InsertPointUnroutedFirst(
      vrs,
      () => vrs.unroutedNodes,
      _ => vrs.routedWithVehicles.pendingValue
    )

    val nextNeigh: Move => List[Move] => Option[Neighborhood] = (first: Move) =>
      (prevMoves: List[Move]) => {
        val prevMove: InsertPointMove =
          if (prevMoves.isEmpty) first.asInstanceOf[InsertPointMove]
          else prevMoves.head.asInstanceOf[InsertPointMove]

        if (vrs.unroutedNodes.isEmpty) None
        else {
          val lastInserted = prevMove.nodeToInsert
          val nextToInsert =
            vrs.unroutedNodes.minBy(dist(lastInserted)(_))
          val next =
            InsertPointInsertionPointFirst(vrs, () => List(lastInserted), _ => List(nextToInsert))
          Some(next)
        }
      }

    val search = BackTrackedEjectionChains(
      firstNeigh,
      nextNeigh,
      relaxedObjective = None,
      relaxedAcceptanceCriterion = None,
      stopAtFirstImprovement = false
    )

    search.verbosityLevel = 2
    search.profileSearch()

    search.doAllMoves(obj)

    search.displayProfiling()
    println(vrs)
  }

}
