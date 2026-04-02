package oscar.cbls.test.core.search

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.IntVariable
import oscar.cbls.algo.generator.RoutingGenerator
import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.{IntConstant, IntVariable}
import oscar.cbls.core.computation.objective.{Exploration, Minimize}
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.core.search.{NoMoveFound, SimpleNeighborhood}
import oscar.cbls.lib.invariant.routing.TotalRouteLength
import oscar.cbls.lib.neighborhoods.Assign
import oscar.cbls.lib.neighborhoods.combinator.{AndThen, DynAndThen, Exhaust, LoadSolutionMove}
import oscar.cbls.lib.neighborhoods.routing.{InsertPointUnroutedFirst, OnePointMove, RemovePoint, TwoOpt}
import oscar.cbls.modeling.routing.VRS

import scala.util.Random

class TestDynAndThen extends AnyFunSuite with Matchers {

  test(s"DynAndThen allows to worsen a solution before improving it.") {
    val store               = new Store()
    val a: IntVariable      = IntVariable(store, 20, name = Some("A"))
    val b: IntVariable      = IntVariable(store, 40, name = Some("B"))
    val objective: Minimize = Minimize(a + b)
    store.close()

    val left1            = Assign(Array(a), (_, _) => List(30))
    val right1           = Assign(Array(b), (_, _) => List(20))
    val searchProcedure1 = Exhaust(left1, right1)
    searchProcedure1.doAllMoves(objective)
    // left2 was never applied since it worsen the solution. Only right 2 was applied.
    objective.objValue.value() should be(40)

    val left2            = Assign(Array(a), (_, _) => List(30))
    val right2           = Assign(Array(b), (_, _) => List(5))
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

    val left            = Assign(Array(a), (_, _) => List(30))
    val right           = Assign(Array(b), (_, _) => List(5))
    val searchProcedure = AndThen(left, right)
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

    val left1           = Assign(Array(a), (_, _) => List(20))
    val right1          = Assign(Array(b), (_, _) => List(20))
    val right2          = Assign(Array(d), (_, _) => List(10))
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

    val left1           = Assign(Array(a), (_, _) => List(20))
    val left2           = Assign(Array(c), (_, _) => List(20))
    val right2          = Assign(Array(d), (_, _) => List(10))
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

    val left1  = Assign(Array(a), (_, _) => List(20))
    val right1 = Assign(Array(b), (_, _) => List(20))
    val left2  = Assign(Array(c), (_, _) => List(20))
    val right2 = Assign(Array(d), (_, _) => List(10))
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

    val left1  = Assign(Array(a), (_, _) => List.fill(50)(Random.nextInt(1000)))
    val right1 = Assign(Array(b), (_, _) => List.fill(50)(Random.nextInt(1000)))
    val left2  = Assign(Array(c), (_, _) => List.fill(50)(Random.nextInt(1000)))
    val right2 = Assign(Array(d), (_, _) => List.fill(50)(Random.nextInt(1000)))
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

    val twoOpt   = TwoOpt(vrs, relevantStartSegment)
    val onePoint = OnePointMove(vrs, nodesToMove, relevantInsertPoint)
    val search   = AndThen(twoOpt, onePoint)

    noException mustBe thrownBy(search.doAllMoves(obj))

  }

  test("DynAndThen with best loop in left neighborhood") {
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
    val vrs    = VRS(model, 10, 1, debug = true)
    val output = TotalRouteLength(vrs, distMatrix).routeLength

    val obj = Minimize(output)
    model.close()
    vrs.routes := IntSequence(List(0, 1, 2, 3))
    model.propagate()

    val insert = InsertPointUnroutedFirst(
      vrs,
      () => vrs.unroutedNodes,
      _ => List(0),
      selectNodeBehavior = LoopBehavior.best()
    )
    val remove = RemovePoint(
      vrs,
      () => vrs.routedWithoutVehicles.value(),
      selectNodesToRemoveBehavior = LoopBehavior.first()
    )

    val search = AndThen(insert, remove)

    noException must be thrownBy search.doAllMoves(obj)
  }

  test("DynAndThen with best loop in right neighborhood") {
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
    val vrs    = VRS(model, 10, 1, debug = true)
    val output = TotalRouteLength(vrs, distMatrix).routeLength

    val obj = Minimize(output)
    model.close()
    vrs.routes := IntSequence(List(0, 1, 2, 3))
    model.propagate()
    val insert = InsertPointUnroutedFirst(
      vrs,
      () => vrs.unroutedNodes,
      _ => List(0),
      selectNodeBehavior = LoopBehavior.best()
    )
    val remove = RemovePoint(
      vrs,
      () => vrs.routedWithoutVehicles.value(),
      selectNodesToRemoveBehavior = LoopBehavior.first()
    )

    val search = AndThen(insert, remove)

    noException must be thrownBy search.doAllMoves(obj)
  }

  test("DynAndThen with best loop in left and right neighborhood") {
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
    val vrs    = VRS(model, 10, 1, debug = true)
    val output = TotalRouteLength(vrs, distMatrix).routeLength

    val obj = Minimize(output)
    model.close()
    vrs.routes := IntSequence(List(0, 1, 2, 3))
    model.propagate()
    val insert = InsertPointUnroutedFirst(
      vrs,
      () => vrs.unroutedNodes,
      _ => List(0),
      selectNodeBehavior = LoopBehavior.best()
    )
    val remove = RemovePoint(
      vrs,
      () => vrs.routedWithoutVehicles.value(),
      selectNodesToRemoveBehavior = LoopBehavior.best()
    )

    val search = AndThen(insert, remove)

    noException must be thrownBy search.doAllMoves(obj)
  }

  test("AndThen with left neighborhood choosing the best solution to load") {
    val nbNodes    = 100
    val nbVehicles = 1
    val (_, distanceMatrix, unroutedPenalty, _) =
      RoutingGenerator.generateRandomRoutingData(nbNodes, 2, 0, seed = 42)

    val model  = new Store()
    val vrs    = VRS(model, nbNodes, nbVehicles, debug = true)
    val output = TotalRouteLength(vrs, distanceMatrix).routeLength

    // Creates a penalty for unrouted nodes (otherwise no nodes will be routed)
    val unroutedNodesAndPenalty: IntVariable =
      vrs.unrouted.size() * IntConstant(model, unroutedPenalty)
    val obj = Minimize(output + unroutedNodesAndPenalty)
    model.close()

    val left = new LoadingNeighborhood(vrs)
    val right = RemovePoint(
      vrs,
      () => vrs.routedWithoutVehicles.value(),
      selectNodesToRemoveBehavior = LoopBehavior.best()
    )

    val search = AndThen(left, right)
    noException must be thrownBy search.doAllMoves(obj)

  }

  private class LoadingNeighborhood(vrs: VRS)
      extends SimpleNeighborhood[LoadSolutionMove]("Loading Neighborhood for test") {

    override protected def exploreNeighborhood(exploration: Exploration[LoadSolutionMove]): Unit = {
      val initialSolution             = vrs.store.extractSolution()
      val solutions: Array[List[Int]] = Array(List(1, 2, 3, 4, 5), List(44, 46, 42))

      val (solIterator, stopSol) = LoopBehavior.best().toIterator(solutions.indices)

      for (i <- solIterator) {
        val current = 0 :: solutions(i)
        vrs.routes := IntSequence(current)
        val currentSolution = vrs.routes.model.extractSolution()
        searchProfiler().foreach(x => x.neighborSelected())
        exploration.checkNeighborWP(objValue => LoadSolutionMove(currentSolution, objValue, name))
        initialSolution.restoreSolution()

        if (exploration.toReturn != NoMoveFound) stopSol()
      }
    }

    override def doMove(move: LoadSolutionMove): Unit = move.commit()

    override def reset(): Unit = {}
  }

}
