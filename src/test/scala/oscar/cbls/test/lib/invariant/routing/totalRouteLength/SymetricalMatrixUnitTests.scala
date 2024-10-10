package oscar.cbls.test.lib.invariant.routing.totalRouteLength

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.test.invBench.InvTestBenchWithConstGen
import oscar.cbls.model.routing.VRP
import oscar.cbls.core.computation.Store
import oscar.cbls.lib.invariant.routing.TotalRouteLength
import org.scalacheck.Gen
import oscar.cbls.test.invBench.TestBenchSut
import oscar.cbls.algo.sequence.IntSequence

class SymetricalMatrixUnitTests extends AnyFunSuite with Matchers {

  test("TotalRouteLength do not accept when the matrix is not symetric") {
    val model = new Store()
    val vrp   = VRP(model, 4, 1)
    val distanceMatrix: Array[Array[Long]] =
      Array(Array(1, 0, 0, 1), Array(0, 1, 0, 0), Array(1, 2, 3, 4), Array(1, 4, 3, 3))

    an[IllegalArgumentException] should be thrownBy TotalRouteLength(vrp, distanceMatrix, true)

  }

  private val distanceMatrix: Array[Array[Long]] =
    Array(
      Array(73958, 61888, 62709, 49501, 7372, 83661, 57116, 29586, 86889, 15972),
      Array(61888, 29786, 1715, 67873, 28914, 2286, 65043, 57427, 64017, 22215),
      Array(62709, 1715, 78297, 42365, 51558, 50378, 44208, 3945, 78389, 47882),
      Array(49501, 67873, 42365, 52630, 17086, 10979, 8371, 99874, 81103, 71514),
      Array(7372, 28914, 51558, 17086, 37972, 47127, 53086, 7918, 12736, 45578),
      Array(83661, 2286, 50378, 10979, 47127, 86076, 23040, 61991, 15452, 48222),
      Array(57116, 65043, 44208, 8371, 53086, 23040, 73145, 76191, 12643, 84914),
      Array(29586, 57427, 3945, 99874, 7918, 61991, 76191, 8327, 31970, 48749),
      Array(86889, 64017, 78389, 81103, 12736, 15452, 12643, 31970, 54203, 74279),
      Array(15972, 22215, 47882, 71514, 45578, 48222, 84914, 48749, 74279, 27270)
    )

  test("Total Route Length works when inserting points in an empty route") {
    val model = new Store(debugLevel = 3)
    val vrp   = VRP(model, 10, 2)
    val _     = TotalRouteLength(vrp, distanceMatrix)
    model.close()
    val routes = vrp.routes
    val exp    = routes.value().explorerAtPosition(1).get
    routes.insertAfterPosition(6, exp)
    vrp.model.propagate()
  }

  test("Total Route Length works when inserting points in a non empty route") {
    val model = new Store(debugLevel = 3)
    val vrp   = VRP(model, 10, 2)
    val _     = TotalRouteLength(vrp, distanceMatrix)
    model.close()

    val routes = vrp.routes
    routes := IntSequence(List(0, 8, 3, 4, 1, 5))
    val exp = routes.value().explorerAtPosition(3).get
    routes.insertAfterPosition(2, exp)

    vrp.model.propagate()
  }

  test("Total Route Length works when removing points giving an empty route") {
    val model = new Store(debugLevel = 3)
    val vrp   = VRP(model, 10, 2)
    val _     = TotalRouteLength(vrp, distanceMatrix)
    model.close()

    val routes = vrp.routes
    routes := IntSequence(List(0, 8, 1, 5))
    val exp = routes.value().explorerAtPosition(1).get
    routes.remove(exp)

    vrp.model.propagate()
  }

  test("Total Route Length works when removing point giving a non empty route") {
    val model = new Store(debugLevel = 3)
    val vrp   = VRP(model, 10, 2)
    val _     = TotalRouteLength(vrp, distanceMatrix)
    model.close()

    val routes = vrp.routes
    routes := IntSequence(List(0, 8, 3, 4, 1, 5))
    val exp = routes.value().explorerAtPosition(3).get
    routes.remove(exp)

    vrp.model.propagate()
  }

  test("Total Route Length works when moving segment, emptying a vehicle") {
    val model = new Store(debugLevel = 3)
    val vrp   = VRP(model, 10, 2)
    val _     = TotalRouteLength(vrp, distanceMatrix)
    model.close()

    val routes = vrp.routes
    routes := IntSequence(List(0, 8, 3, 4, 1, 5))
    val fromExp  = routes.value().explorerAtPosition(1).get
    val toExp    = routes.value().explorerAtPosition(3).get
    val afterExp = routes.value().explorerAtPosition(5).get
    routes.move(fromExp, toExp, afterExp, flip = false)

    vrp.model.propagate()
  }

  test("Total Route Length works when moving segment") {
    val model = new Store(debugLevel = 3)
    val vrp   = VRP(model, 10, 2)
    val _     = TotalRouteLength(vrp, distanceMatrix)
    model.close()

    val routes = vrp.routes
    routes := IntSequence(List(0, 8, 3, 4, 1, 5))
    val fromExp  = routes.value().explorerAtPosition(2).get
    val toExp    = routes.value().explorerAtPosition(3).get
    val afterExp = routes.value().explorerAtPosition(5).get
    routes.move(fromExp, toExp, afterExp, flip = false)

    vrp.model.propagate()
  }

  test("Total Route Length works when moving and flipping segment") {
    val model = new Store(debugLevel = 3)
    val vrp   = VRP(model, 10, 2)
    val _     = TotalRouteLength(vrp, distanceMatrix)
    model.close()

    val routes = vrp.routes
    routes := IntSequence(List(0, 8, 3, 4, 1, 5))
    val fromExp  = routes.value().explorerAtPosition(2).get
    val toExp    = routes.value().explorerAtPosition(3).get
    val afterExp = routes.value().explorerAtPosition(5).get
    routes.move(fromExp, toExp, afterExp, flip = true)

    vrp.model.propagate()
  }

  test("Total Route Length works when moving segment in an empty routes") {
    val model = new Store(debugLevel = 3)
    val vrp   = VRP(model, 10, 2)
    val _     = TotalRouteLength(vrp, distanceMatrix)
    model.close()
    val routes = vrp.routes
    routes := IntSequence(List(0, 1, 2, 4))
    vrp.model.propagate()

    val expStartFrom = routes.value().explorerAtPosition(2).get
    val expEndFrom   = routes.value().explorerAtPosition(3).get
    val expTo        = routes.value().explorerAtPosition(0).get
    routes.move(expStartFrom, expEndFrom, expTo, flip = false)

    vrp.model.propagate()
  }

  test("Total Route Length works when flipping segment") {
    val model = new Store(debugLevel = 3)
    val vrp   = VRP(model, 10, 2)
    val _     = TotalRouteLength(vrp, distanceMatrix)
    model.close()

    val routes = vrp.routes
    routes := IntSequence(List(0, 8, 3, 4, 1, 5))
    val fromExp  = routes.value().explorerAtPosition(2).get
    val toExp    = routes.value().explorerAtPosition(3).get
    val afterExp = routes.value().explorerAtPosition(1).get
    routes.flip(fromExp, toExp)

    vrp.model.propagate()
  }

  test("Total route length works when rolling back to checkpoints") {
    val model = new Store(debugLevel = 3)
    val vrp   = VRP(model, 10, 2)
    val _     = TotalRouteLength(vrp, distanceMatrix)
    model.close()

    val routes = vrp.routes
    routes := IntSequence(List(0, 8, 3, 4, 1, 5))
    routes.defineCurrentValueAsCheckpoint()
    val startExp = routes.value().explorerAtPosition(3).get
    val endExp   = routes.value().explorerAtPosition(3).get
    val afterExp = routes.value().explorerAtPosition(4).get
    routes.move(startExp, endExp, afterExp, flip = true)

    vrp.model.propagate()
    routes.rollbackToTopCheckpoint()
    vrp.model.propagate()

  }

  test("Total route length works when playing with checkpoints") {
    val model = new Store(debugLevel = 3)
    val vrp   = VRP(model, 10, 2)
    val _     = TotalRouteLength(vrp, distanceMatrix)
    model.close()
    val routes = vrp.routes
    routes := IntSequence(List(0, 8, 3, 4, 1, 5))
    vrp.model.propagate() // Propagate

    // Define some moves
    routes.defineCurrentValueAsCheckpoint()
    val insertExp1 = routes.value().explorerAtPosition(3).get
    routes.insertAfterPosition(9, insertExp1)
    routes.defineCurrentValueAsCheckpoint()
    val insertExp2 = routes.value().explorerAtPosition(5).get
    routes.insertAfterPosition(6, insertExp2)
    // Propagate
    vrp.model.propagate()

    // Define some moves
    routes.rollbackToTopCheckpoint()
    routes.releaseTopCheckpoint()
    val insertExp3 = routes.value().explorerAtPosition(6).get
    routes.insertAfterPosition(6, insertExp3)
    // Propagate
    vrp.model.propagate()
  }

}
