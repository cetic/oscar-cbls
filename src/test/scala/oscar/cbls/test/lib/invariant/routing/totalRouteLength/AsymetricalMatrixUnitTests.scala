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

class AsymetricalMatrixUnitTests extends AnyFunSuite with Matchers {

  test("TotalRouteLength do not accept when the matrix is not symetric") {
    val model = new Store()
    val vrp   = VRP(model, 4, 1)
    val distanceMatrix: Array[Array[Long]] =
      Array(Array(1, 0, 0, 1), Array(0, 1, 0, 0), Array(1, 2, 3, 4), Array(1, 4, 3, 3))

    an[IllegalArgumentException] should be thrownBy TotalRouteLength(
      vrp,
      distanceMatrix,
      true
    )

  }

  private val distanceMatrix : Array[Array[Long]] = Array(
      Array(42254, 85511, 75508, 21913, 32574, 45017, 76208, 27770, 79010, 52963),
      Array(48628, 2052, 8290, 6497, 60336, 22092, 55622, 9817, 37527, 90359),
      Array(83151, 11681, 67679, 96475, 65283, 89826, 22609, 65312, 43588, 90218),
      Array(21875, 59899, 31392, 65749, 24230, 59082, 13590, 35604, 75020, 74112),
      Array(24809, 2765, 5438, 69177, 82460, 42344, 48903, 24547, 31428, 21111),
      Array(48178, 44045, 59083, 73924, 19766, 25412, 99659, 38911, 83625, 26062),
      Array(32563, 48270, 88174, 12127, 39223, 39452, 78325, 68441, 77241, 31966),
      Array(70985, 64895, 46455, 70039, 2177, 68676, 53872, 34678, 95774, 63907),
      Array(57321, 6442, 31093, 11418, 36598, 9763, 65435, 57716, 24683, 49216),
      Array(77648, 74604, 14510, 85149, 28449, 68360, 52716, 48001, 69236, 97947)
    )


  test("Total Route Length works when inserting points") {
    val model       = new Store(debugLevel = 3)
    val vrp         = VRP(model, 10, 2)
    val routeLength = TotalRouteLength(vrp, distanceMatrix)
    model.close()
    vrp.model.propagate()
    val routes = vrp.routes
    routes := IntSequence(List(0, 8, 3, 4, 1, 5))
    val exp = routes.value().explorerAtPosition(3).get
    routes.insertAfterPosition(2, exp)
    vrp.model.propagate()
  }

  test("Total Route Length works when moving segment") {
    val model       = new Store(debugLevel = 3)
    val vrp         = VRP(model, 10, 2)
    val routeLength = TotalRouteLength(vrp, distanceMatrix)
    model.close()
    vrp.model.propagate()
    val routes = vrp.routes
    routes := IntSequence(List(0, 8, 3, 4, 1, 5))
    val fromExp  = routes.value().explorerAtPosition(2).get
    val toExp    = routes.value().explorerAtPosition(3).get
    val afterExp = routes.value().explorerAtPosition(5).get
    routes.move(fromExp, toExp, afterExp, false)
    vrp.model.propagate()
  }

  test("Total Route Length works when moving and flipping segment") {
    val model       = new Store(debugLevel = 3)
    val vrp         = VRP(model, 10, 2)
    val routeLength = TotalRouteLength(vrp, distanceMatrix)
    model.close()
    vrp.model.propagate()
    val routes = vrp.routes
    routes := IntSequence(List(0, 8, 3, 4, 1, 5))
    val fromExp  = routes.value().explorerAtPosition(2).get
    val toExp    = routes.value().explorerAtPosition(3).get
    val afterExp = routes.value().explorerAtPosition(5).get
    routes.move(fromExp, toExp, afterExp, true)
    vrp.model.propagate()
  }

  test("Total Route Length works when flipping segment") {
    val model       = new Store(debugLevel = 3)
    val vrp         = VRP(model, 10, 2)
    val routeLength = TotalRouteLength(vrp, distanceMatrix)
    model.close()
    vrp.model.propagate()
    val routes = vrp.routes
    routes := IntSequence(List(0, 8, 3, 4, 1, 5))
    val fromExp  = routes.value().explorerAtPosition(2).get
    val toExp    = routes.value().explorerAtPosition(3).get
    val afterExp = routes.value().explorerAtPosition(1).get
    routes.flip(fromExp,toExp)
    vrp.model.propagate()
  }

  test("Total route length works when rolling back to checkpoints") {
    val model       = new Store(debugLevel = 3)
    val vrp         = VRP(model, 10, 2)
    val routeLength = TotalRouteLength(vrp, distanceMatrix)
    model.close()
    vrp.model.propagate()
    val routes = vrp.routes
    routes := IntSequence(List(0, 8, 3, 4, 1, 5))
    routes.defineCurrentValueAsCheckpoint()
    val startExp = routes.value().explorerAtPosition(3).get
    val endExp   = routes.value().explorerAtPosition(3).get
    val afterExp = routes.value().explorerAtPosition(4).get
    routes.move(startExp, endExp, afterExp, true)

    vrp.model.propagate()
    routes.rollbackToTopCheckpoint()
    vrp.model.propagate()

  }

  test("Total route length works when playing with checkpoints") {
    val model       = new Store(debugLevel = 3)
    val vrp         = VRP(model, 10, 2)
    val routeLength = TotalRouteLength(vrp, distanceMatrix)
    model.close()
    val routes = vrp.routes
    routes := IntSequence(List(0, 8, 3, 4, 1, 5))
    vrp.model.propagate() // Propagate

    // Define some moves
    routes.defineCurrentValueAsCheckpoint()
    val insertExp1 = routes.value().explorerAtPosition(3).get
    routes.insertAfterPosition(9,insertExp1)
    routes.defineCurrentValueAsCheckpoint()
    val insertExp2 = routes.value().explorerAtPosition(5).get
    routes.insertAfterPosition(6,insertExp2)
    // Propagate
    vrp.model.propagate()

    // Define some moves
    routes.rollbackToTopCheckpoint()
    routes.releaseTopCheckpoint()
    val insertExp3 = routes.value().explorerAtPosition(6).get
    routes.insertAfterPosition(6,insertExp3)
    // Propagate
    vrp.model.propagate()
  }


}
