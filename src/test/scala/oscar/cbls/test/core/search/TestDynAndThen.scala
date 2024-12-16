package oscar.cbls.test.core.search

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.Minimize
import oscar.cbls.lib.invariant.routing.TotalRouteLength
import oscar.cbls.lib.neighborhoods.AssignNeighborhood
import oscar.cbls.lib.neighborhoods.combinator.{DynAndThen, Exhaust}
import oscar.cbls.lib.neighborhoods.routing.{OnePointMoveNeighborhood, TwoOpt}
import oscar.cbls.modeling.routing.VRP

import scala.util.Random

class TestDynAndThen extends AnyFunSuite with Matchers {

  test(s"DynAndThen allows to worsen a solution before improving it.") {
    val store               = new Store()
    val a: IntVariable      = IntVariable(store, 20, name = Some("A"))
    val b: IntVariable      = IntVariable(store, 40, name = Some("B"))
    val objective: Minimize = Minimize(a + b)
    store.close()

    val left1            = AssignNeighborhood(Array(a), (_, _) => List(30))
    val right1           = AssignNeighborhood(Array(b), (_, _) => List(20))
    val searchProcedure1 = Exhaust(left1, right1)
    searchProcedure1.doAllMoves(objective)
    // left2 was never applied since it worsen the solution. Only right 2 was applied.
    objective.objValue.value() should be(40)

    val left2            = AssignNeighborhood(Array(a), (_, _) => List(30))
    val right2           = AssignNeighborhood(Array(b), (_, _) => List(5))
    val searchProcedure2 = DynAndThen(left2, _ => right2)
    searchProcedure2.doAllMoves(objective)
    // left is applied as well as right even though left worsen the solution.
    objective.objValue.value() should be(35)
  }

  test(s"AndThen allows to worsen a solution before improving it.") {
    val store               = new Store()
    val a: IntVariable      = IntVariable(store, 20, name = Some("A"))
    val b: IntVariable      = IntVariable(store, 40, name = Some("B"))
    val objective: Minimize = Minimize(a + b)
    store.close()

    val left            = AssignNeighborhood(Array(a), (_, _) => List(30))
    val right           = AssignNeighborhood(Array(b), (_, _) => List(5))
    val searchProcedure = DynAndThen(left, right)
    searchProcedure.doAllMoves(objective)
    // left is applied as well as right even though left worsen the solution.
    objective.objValue.value() should be(35)
  }

  test(s"DynAndThen allows nested DynAndThen as left neighborhoods") {
    val store               = new Store()
    val a: IntVariable      = IntVariable(store, 10, name = Some("A"))
    val b: IntVariable      = IntVariable(store, 10, name = Some("B"))
    val c: IntVariable      = IntVariable(store, 10, name = Some("C"))
    val d: IntVariable      = IntVariable(store, 50, name = Some("D"))
    val objective: Minimize = Minimize(a + b + c + d)
    store.close()

    val left1           = AssignNeighborhood(Array(a), (_, _) => List(20))
    val right1          = AssignNeighborhood(Array(b), (_, _) => List(20))
    val right2          = AssignNeighborhood(Array(d), (_, _) => List(10))
    val searchProcedure = DynAndThen(DynAndThen(left1, _ => right1), _ => right2)
    searchProcedure.doAllMoves(objective)
    // left is applied as well as right even though left worsen the solution.
    objective.objValue.value() should be(60)
  }

  test(s"DynAndThen allows nested DynAndThen as right neighborhoods") {
    val store               = new Store()
    val a: IntVariable      = IntVariable(store, 10, name = Some("A"))
    val b: IntVariable      = IntVariable(store, 10, name = Some("B"))
    val c: IntVariable      = IntVariable(store, 10, name = Some("C"))
    val d: IntVariable      = IntVariable(store, 50, name = Some("D"))
    val objective: Minimize = Minimize(a + b + c + d)
    store.close()

    val left1           = AssignNeighborhood(Array(a), (_, _) => List(20))
    val left2           = AssignNeighborhood(Array(c), (_, _) => List(20))
    val right2          = AssignNeighborhood(Array(d), (_, _) => List(10))
    val searchProcedure = DynAndThen(left1, _ => DynAndThen(left2, _ => right2))
    searchProcedure.doAllMoves(objective)
    // left is applied as well as right even though left worsen the solution.
    objective.objValue.value() should be(60)
  }

  test(s"DynAndThen allows nested DynAndThen as left and right neighborhoods") {
    val store               = new Store()
    val a: IntVariable      = IntVariable(store, 10, name = Some("A"))
    val b: IntVariable      = IntVariable(store, 10, name = Some("B"))
    val c: IntVariable      = IntVariable(store, 10, name = Some("C"))
    val d: IntVariable      = IntVariable(store, 50, name = Some("D"))
    val objective: Minimize = Minimize(a + b + c + d)
    store.close()

    val left1  = AssignNeighborhood(Array(a), (_, _) => List(20))
    val right1 = AssignNeighborhood(Array(b), (_, _) => List(20))
    val left2  = AssignNeighborhood(Array(c), (_, _) => List(20))
    val right2 = AssignNeighborhood(Array(d), (_, _) => List(10))
    val searchProcedure =
      DynAndThen(DynAndThen(left1, _ => right1), _ => DynAndThen(left2, _ => right2))
    searchProcedure.doAllMoves(objective)
    // left is applied as well as right even though left worsen the solution.
    objective.objValue.value() should be(70)
  }

  test(s"Multiple exploration of nested DynAndThen works as expected with profiling (hidden)") {
    val store               = new Store()
    val a: IntVariable      = IntVariable(store, 1000, name = Some("A"))
    val b: IntVariable      = IntVariable(store, 1000, name = Some("B"))
    val c: IntVariable      = IntVariable(store, 1000, name = Some("C"))
    val d: IntVariable      = IntVariable(store, 1000, name = Some("D"))
    val objective: Minimize = Minimize(a + b + c + d)
    store.close()

    val left1  = AssignNeighborhood(Array(a), (_, _) => List.fill(50)(Random.nextInt(1000)))
    val right1 = AssignNeighborhood(Array(b), (_, _) => List.fill(50)(Random.nextInt(1000)))
    val left2  = AssignNeighborhood(Array(c), (_, _) => List.fill(50)(Random.nextInt(1000)))
    val right2 = AssignNeighborhood(Array(d), (_, _) => List.fill(50)(Random.nextInt(1000)))
    val searchProcedure =
      DynAndThen(DynAndThen(left1, _ => right1), _ => DynAndThen(left2, _ => right2))
    searchProcedure.profileSearch()
    searchProcedure.doAllMoves(objective)

    // searchProcedure.displayProfiling()
  }

  test("DynAndThen works with moves on sequences") {
    def generateManhattanDistMatrix(points: Array[(Int, Int)]): Array[Array[Long]] = {
      def dist(x1: (Int, Int), x2: (Int, Int)): Long = (x1._1 - x2._1).abs + (x1._2 - x2._2).abs

      Array.tabulate(points.length, points.length)((i: Int, j: Int) => dist(points(i), points(j)))
    }

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
    val vrp    = VRP(model, 10, 2, debug = true)
    val output = TotalRouteLength(vrp, distMatrix).routeLength

    val obj = Minimize(output)
    model.close()
    vrp.routes := IntSequence(List(0, 4, 3, 5, 2, 1, 9, 6, 8, 7))
    model.propagate()

    val routedNodeExceptVehicle = vrp.routedWithoutVehicles.pendingValue

    val relevantStartSegment = () => routedNodeExceptVehicle

    val nodesToMove = () => vrp.routedWithoutVehicles.pendingValue
    val relevantInsertPoint = (x: Int) => {
      val xExp = vrp.routes.pendingValue.explorerAtAnyOccurrence(x).get
      val prev = xExp.prev.value
      vrp.routedWithVehicles.pendingValue.diff(Set(x, prev))
    }

    val twoOpt   = TwoOpt(vrp, relevantStartSegment)
    val onePoint = OnePointMoveNeighborhood(vrp, nodesToMove, relevantInsertPoint)
    val search   = DynAndThen(twoOpt, onePoint)

    noException mustBe thrownBy(search.doAllMoves(obj))

  }
}
