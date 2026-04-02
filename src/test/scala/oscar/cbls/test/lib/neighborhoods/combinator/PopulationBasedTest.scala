package oscar.cbls.test.lib.neighborhoods.combinator

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import oscar.cbls.algo.generator.RoutingGenerator
import oscar.cbls.api.{ApiModel, Cbls}
import oscar.cbls.core.computation.integer.{IntConstant, IntVariable}
import oscar.cbls.core.computation.objective.Minimize
import oscar.cbls.core.computation.{Solution, Store}
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.lib.invariant.set.Cardinality
import oscar.cbls.lib.neighborhoods.combinator._
import oscar.cbls.lib.neighborhoods.routing.{InsertPointInsertionPointFirst, OnePointMove, RemovePoint}
import oscar.cbls.modeling.routing.VRS
import oscar.cbls.test.lib.neighborhoods.routing.NaiveSumDistancesInvariant
import oscar.cbls.{Neighborhood, Objective}

import java.io.{OutputStream, PrintStream}
import scala.util.Random

class PopulationBasedTest extends AnyFunSuite with Matchers {
  private[this] def getBasicModelForTest(random: Random): (VRS, Minimize) = {

    val v = 2
    val n = 50
    val d = 100

    val coordinates =
      RoutingGenerator.evenlySpacedNodes(n, v, d, RoutingGenerator.centerDepot, random)
    val distances = RoutingGenerator.distancesMatrix(coordinates)

    val model    = new Store()
    val vrs      = VRS(model, n, v)
    var toInsert = random.shuffle(List.from(v until n)).take((n - v) / 2)
    while (toInsert.nonEmpty) {
      val node     = toInsert.head
      val explorer = vrs.routes.pendingValue.explorerAtPosition(0).get
      vrs.routes.insertAfterPosition(node, explorer)
      toInsert = toInsert.tail
    }
    val routeLength = IntVariable(model, 0L)
    new NaiveSumDistancesInvariant(model, vrs.routes, distances, routeLength)
    val nbUnrouted = IntVariable(model, 0L)
    new Cardinality(model, vrs.unrouted, nbUnrouted, None)
    val objective = Minimize(routeLength + (nbUnrouted * IntConstant(model, 1000)))

    model.close()
    model.propagate()

    (vrs, objective)
  }

  test("PopulationBasedSearch works as expected") {
    val seed             = 396144803746428278L
    val random           = new Random(seed)
    val (vrs, objective) = getBasicModelForTest(random)

    def search = Exhaust(
      MaxMoves(
        AcceptAll(
          RemovePoint(
            vrs,
            relevantNodesToRemove = () => {
              // remove just one, randomly
              val allPoints = vrs.routedWithoutVehicles.value().toArray
              Some(allPoints(random.nextInt(allPoints.length)))
            }
          )
        ),
        1
      ),
      RoundRobin(
        Array(
          (
            OnePointMove(
              vrs,
              () => vrs.routedWithoutVehicles.pendingValue,
              (x: Int) => vrs.routedWithVehicles.value().filter((n: Int) => n != x && n != x - 1),
              selectDestinationBehavior = LoopBehavior.best()
            ),
            1
          ),
          (
            InsertPointInsertionPointFirst(
              vrs,
              () => vrs.routedWithVehicles.value(),
              (_: Int) => vrs.unroutedNodes,
              selectInsertionPointBehavior = LoopBehavior.best()
            ),
            1
          )
        )
      )
    )

    val populationBasedSearch = PopulationBasedSearch[Int](
      () => 0,
      (it, _: List[Int]) =>
        if (it < 10) Some((_, _) => (false, List((search, 0), (search, 0), (search, 0))), 4, None)
        else None,
      store = vrs.store,
      saveAnytimeBest = false
    )

    populationBasedSearch.verbosityLevel = 0

    withClue(s"Seed: $seed") {
      noException mustBe thrownBy(
        populationBasedSearch.doAllMoves(objective, _ >= 1)
      ) // only one move
    }
    // println(s"Routing after search: $vrs")
  }

  test("PopulationBased verbose mode works well while using anytimeBest") {
    val seed             = 396144803746428278L
    val random           = new Random(seed)
    val (vrs, objective) = getBasicModelForTest(random)

    def search = Exhaust(
      MaxMoves(
        AcceptAll(
          RemovePoint(
            vrs,
            relevantNodesToRemove = () => {
              // remove just one, randomly
              val allPoints = vrs.routedWithoutVehicles.value().toArray
              Some(allPoints(random.nextInt(allPoints.length)))
            }
          )
        ),
        1
      ),
      RoundRobin(
        Array(
          (
            OnePointMove(
              vrs,
              () => vrs.routedWithoutVehicles.pendingValue,
              (x: Int) => vrs.routedWithVehicles.value().filter((n: Int) => n != x && n != x - 1),
              selectDestinationBehavior = LoopBehavior.best()
            ),
            1
          ),
          (
            InsertPointInsertionPointFirst(
              vrs,
              () => vrs.routedWithVehicles.value(),
              (_: Int) => vrs.unroutedNodes,
              selectInsertionPointBehavior = LoopBehavior.best()
            ),
            1
          )
        )
      )
    )

    val populationBasedSearch = PopulationBasedSearch[Int](
      () => 0,
      (it, _: List[Int]) =>
        if (it < 10) Some((_, _) => (false, List((search, 0), (search, 0), (search, 0))), 4, None)
        else None,
      store = vrs.store,
      saveAnytimeBest = true
    )

    val nullStream = new PrintStream(new OutputStream {

      override def write(b: Int): Unit = {}

      override def write(b: Array[Byte], off: Int, len: Int): Unit = {}
    })

    populationBasedSearch.verbosityLevel = 2

    withClue(s"Seed: $seed") {
      noException mustBe thrownBy {
        Console.withOut(nullStream) {
          // Does not display the verbosity but construct all the needed string

          populationBasedSearch.doAllMoves(objective, _ >= 1)
        }
      }
    }
  }

  test("PopulationBasedSearch exploits data of individual while minimizing") {
    val model: ApiModel = Cbls.model("Test population based")
    val a               = model.intVar(100, 0, 100, "A")
    val b               = model.intVar(100, 0, 100, "B")
    val obj             = model.minimize(a + b)
    obj.name = "a + b"
    model.close()

    import model.search.{andThen, assign, maxMoves, populationBased}

    val setVar: (IntVariable, Long) => Neighborhood = (x, v) => assign(Array(x), (_, _) => Some(v))

    val add: (IntVariable, Long) => Neighborhood =
      (x, v) => maxMoves(assign(Array(x), (y, _) => Some(y.value() + v)), 1)

    val initVar: (Int, Int) => Neighborhood = (x, y) => andThen(setVar(a, x), setVar(b, y))

    val step: (Int, List[Boolean]) => Option[
      ((Solution, Boolean) => (Boolean, List[(Neighborhood, Boolean)]), Int, Option[Objective])
    ] =
      (it, _) => {
        it match {
          case 0 =>
            val initialGen = List(
              (initVar(55, 20), true),
              (initVar(58, 20), true),
              (initVar(10, 66), false),
              (initVar(10, 69), false)
            )
            Some(((_: Solution, _: Boolean) => (false, initialGen), 4, None))
          case 1 =>
            val genChildren: (Solution, Boolean) => (Boolean, List[(Neighborhood, Boolean)]) =
              (_, data) => {
                val updatedVar = if (data) a else b
                val nextGen = List(
                  (add(updatedVar, -23), data),
                  (add(updatedVar, -5), data),
                  (add(updatedVar, -1), data)
                )
                (false, nextGen)
              }
            Some((genChildren, 4, None))
          case 2 =>
            val genChildren: (Solution, Boolean) => (Boolean, List[(Neighborhood, Boolean)]) =
              (_, data) => {
                val updatedVar = if (data) a else b
                val nextGen = List(
                  (add(updatedVar, -10), data),
                  (add(updatedVar, -2), data),
                  (add(updatedVar, -1), data)
                )
                (false, nextGen)
              }
            Some((genChildren, 4, None))

          case _ => None
        }
      }

    val search = populationBased(
      () => true,
      step,
      saveAnytimeBest = false,
      filterRedundantElements = false,
      dropIfNoMoveFound = false
    )
    search.verbosityLevel = 0

    noException mustBe thrownBy(search.doAllMoves(obj, _ >= 1))

    obj.objValue.value() must be(42)
    a.value() must be(22)
    b.value() must be(20)
  }

  test("PopulationBasedSearch exploits data of individual while maximizing") {
    val model: ApiModel = Cbls.model("Test population based")
    val a               = model.intVar(0, 0, 100, "A")
    val b               = model.intVar(0, 0, 100, "B")
    val obj             = model.maximize(a + b)
    obj.name = "a + b"
    model.close()

    import model.search.{andThen, assign, maxMoves, populationBased}

    val setVar: (IntVariable, Long) => Neighborhood = (x, v) => assign(Array(x), (_, _) => Some(v))

    val add: (IntVariable, Long) => Neighborhood =
      (x, v) => maxMoves(assign(Array(x), (y, _) => Some(y.value() + v)), 1)

    val initVar: (Int, Int) => Neighborhood = (x, y) => andThen(setVar(a, x), setVar(b, y))

    val step: (Int, List[Boolean]) => Option[
      ((Solution, Boolean) => (Boolean, List[(Neighborhood, Boolean)]), Int, Option[Objective])
    ] =
      (it, _) => {
        it match {
          case 0 =>
            val initialGen = List(
              (initVar(10, 2), true),
              (initVar(8, 2), true),
              (initVar(2, 9), false),
              (initVar(2, 7), false)
            )
            Some(((_: Solution, _: Boolean) => (false, initialGen), 4, None))
          case 1 =>
            val genChildren: (Solution, Boolean) => (Boolean, List[(Neighborhood, Boolean)]) =
              (_, data) => {
                val updatedVar = if (data) a else b
                val nextGen = List(
                  (add(updatedVar, 20), data),
                  (add(updatedVar, 5), data),
                  (add(updatedVar, 1), data)
                )
                (false, nextGen)
              }
            Some((genChildren, 4, None))
          case 2 =>
            val genChildren: (Solution, Boolean) => (Boolean, List[(Neighborhood, Boolean)]) =
              (_, data) => {
                val updatedVar = if (data) a else b
                val nextGen = List(
                  (add(updatedVar, 10), data),
                  (add(updatedVar, 5), data),
                  (add(updatedVar, 2), data)
                )
                (false, nextGen)
              }
            Some((genChildren, 4, None))

          case _ => None
        }
      }

    val search = populationBased(
      () => true,
      step,
      saveAnytimeBest = false,
      filterRedundantElements = false,
      dropIfNoMoveFound = false
    )
    search.verbosityLevel = 0

    noException mustBe thrownBy(search.doAllMoves(obj, _ >= 1))

    obj.objValue.value() must be(42)
    a.value() must be(40)
    b.value() must be(2)
  }

  test(
    "PopulationBasedSearch: using another objective for generation and selection works as expected"
  ) {
    val model: ApiModel = Cbls.model("Test population based")
    val a               = model.intVar(100, 0, 100, "A")
    val b               = model.intVar(100, 0, 100, "B")
    val c: IntVariable  = a + b
    c.name = "a + b"
    val mainObj = model.minimize(c)
    val d       = (a - b).abs
    d.name = "|a - b|"
    val alternateObj = model.minimize(d)
    model.close()

    import model.search.{andThen, assign, maxMoves, populationBased}

    val setVar: (IntVariable, Long) => Neighborhood = (x, v) => assign(Array(x), (_, _) => Some(v))

    val add: (IntVariable, Long) => Neighborhood =
      (x, v) => maxMoves(assign(Array(x), (y, _) => Some(y.value() + v)), 1)

    val initVar: (Int, Int) => Neighborhood = (x, y) => andThen(setVar(a, x), setVar(b, y))

    val step: (Int, List[Boolean]) => Option[
      ((Solution, Boolean) => (Boolean, List[(Neighborhood, Boolean)]), Int, Option[Objective])
    ] =
      (it, _) => {
        it match {
          case 0 =>
            val initialGen = List(
              (initVar(80, 30), true),
              (initVar(82, 30), true),
              (initVar(22, 89), false),
              (initVar(22, 91), false)
            )
            Some(((_: Solution, _: Boolean) => (false, initialGen), 4, None))
          case 1 =>
            val genChildren: (Solution, Boolean) => (Boolean, List[(Neighborhood, Boolean)]) =
              (_, data) => {
                val updatedVar = if (data) a else b
                val nextGen = List(
                  (add(updatedVar, -30), data),
                  (add(updatedVar, -10), data),
                  (add(updatedVar, -5), data)
                )
                (false, nextGen)
              }
            Some((genChildren, 4, None))
          case 2 =>
            val genChildren: (Solution, Boolean) => (Boolean, List[(Neighborhood, Boolean)]) =
              (_, data) => {
                val updatedVar = if (data) a else b
                val nextGen = List(
                  (add(updatedVar, if (data) -20 else -36), data),
                  (add(updatedVar, -10), data),
                  (add(updatedVar, -5), data)
                )
                (false, nextGen)
              }
            Some((genChildren, 4, Some(alternateObj)))

          case 3 =>
            val genChildren: (Solution, Boolean) => (Boolean, List[(Neighborhood, Boolean)]) =
              (_, data) => {
                val updatedVar = if (data) a else b
                val nextGen = List(
                  (add(updatedVar, -3), data),
                  (add(updatedVar, -2), data),
                  (add(updatedVar, -1), data)
                )
                (false, nextGen)
              }
            Some((genChildren, 4, None))

          case _ => None
        }
      }

    val search =
      populationBased(
        () => true,
        step,
        saveAnytimeBest = false,
        filterRedundantElements = false,
        dropIfNoMoveFound = false
      )
    search.verbosityLevel = 0

    noException mustBe thrownBy(search.doAllMoves(mainObj, _ >= 1))

    mainObj.objValue.value() must be(42)
    a.value() must be(22)
    b.value() must be(20)
  }

  test("PopulationBasedSearch: skip some generation") {
    val model: ApiModel = Cbls.model("Test population based")
    val a               = model.intVar(100, 0, 100, "A")
    val b               = model.intVar(100, 0, 100, "B")
    val obj             = model.minimize(a + b)
    obj.name = "a + b"
    model.close()

    import model.search._

    val setVar: (IntVariable, Long) => Neighborhood = (x, v) => assign(Array(x), (_, _) => Some(v))

    val add: (IntVariable, Long) => Neighborhood =
      (x, v) => maxMoves(assign(Array(x), (y, _) => Some(y.value() + v)), 1)

    val initVar: (Int, Int) => Neighborhood = (x, y) => andThen(setVar(a, x), setVar(b, y))

    val step: (Int, List[Boolean]) => Option[
      ((Solution, Boolean) => (Boolean, List[(Neighborhood, Boolean)]), Int, Option[Objective])
    ] =
      (it, _) => {
        it match {
          case 0 =>
            val initialGen = List(
              (initVar(52, 20), true),
              (initVar(55, 20), true),
              (initVar(10, 64), false),
              (initVar(10, 68), false)
            )
            Some(((_: Solution, _: Boolean) => (false, initialGen), 4, None))
          case 1 =>
            val genChildren: (Solution, Boolean) => (Boolean, List[(Neighborhood, Boolean)]) =
              (_, data) => {
                val nextGen = {
                  if (data) List((add(a, -20), true), (add(a, -10), true), (add(a, -5), true))
                  else List((add(b, -10), false), (add(b, -5), false), (add(b, -2), false))
                }
                (false, nextGen)
              }
            Some((genChildren, 4, None))
          case 2 =>
            val genChildren: (Solution, Boolean) => (Boolean, List[(Neighborhood, Boolean)]) =
              (_, data) => {
                val nextGen = {
                  if (data) List((doNothing()(model), true))
                  else List((add(b, -15), false), (add(b, -10), false), (add(b, -2), false))
                }
                (false, nextGen)
              }
            Some((genChildren, 4, None))
          case 3 =>
            val genChildren: (Solution, Boolean) => (Boolean, List[(Neighborhood, Boolean)]) =
              (_, data) => {
                val nextGen = {
                  if (data) List((add(a, -10), true), (add(a, -5), true), (add(a, -2), true))
                  else List((add(b, -6), false), (add(b, -3), false), (add(b, -1), false))
                }
                (false, nextGen)
              }
            Some((genChildren, 4, None))

          case _ => None
        }
      }

    val search =
      populationBased(
        () => true,
        step,
        saveAnytimeBest = false,
        filterRedundantElements = false,
        dropIfNoMoveFound = false
      )
    search.verbosityLevel = 0

    noException mustBe thrownBy(search.doAllMoves(obj, _ >= 1))

    obj.objValue.value() must be(42)
    a.value() must be(22)
    b.value() must be(20)
  }

}
