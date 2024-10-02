package oscar.cbls.test.lib.invariant.routing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.test.invBench.InvTestBenchWithConstGen
import oscar.cbls.model.routing.VRP
import oscar.cbls.core.computation.Store
import oscar.cbls.lib.invariant.routing.TotalRouteLength
import org.scalacheck.Gen
import oscar.cbls.test.invBench.TestBenchSut

class TotalRouteLengthTestSuite extends AnyFunSuite with Matchers {

  test("TotalRouteLength do not accept when the matrix has not enough line") {
    val model = new Store()
    val vrp   = VRP(model, 4, 1)
    val distanceMatrix : Array[Array[Long]] =
      Array(Array(1, 0, 0, 1), Array(0, 1, 0, 0), Array(1, 2, 3, 4))

    an[IllegalArgumentException] should be thrownBy TotalRouteLength(
      vrp,
      distanceMatrix
    )

  }

  test("TotalRouteLength do not accept when the matrix has not enough column") {
    val model = new Store()
    val vrp   = VRP(model, 4, 1)
    val distanceMatrix : Array[Array[Long]] =
      Array(Array(1, 0, 0, 1), Array(0, 1, 0, 0), Array(1, 2, 3, 4),Array(0,0,1))

    an[IllegalArgumentException] should be thrownBy TotalRouteLength(
      vrp,
      distanceMatrix
    )

  }

  test("TotalRouteLength do not accept when the matrix is not symetric") {
    val model = new Store()
    val vrp   = VRP(model, 4, 1)
    val distanceMatrix : Array[Array[Long]] =
      Array(Array(1, 0, 0, 1), Array(0, 1, 0, 0), Array(1, 2, 3, 4), Array(1,4,3,3))

    an[IllegalArgumentException] should be thrownBy TotalRouteLength(
      vrp,
      Array(Array(0, 1, 0, 1), Array(1, 1, 1, 1))
    )

  }

  test("TotalRouteLength invariant is working in test bench") {
    val nbNodes = 10
    val nbVehicles = 2

    class TotalRouteLengthTestBench extends InvTestBenchWithConstGen[Array[Array[Long]]]("TotalRouteLength Test Bench",List("WGzuXArD7EGtes8tpwXL5dkJTjVkDkjf8ji1ZtWraTM=")) {

      private def makeItSymetric(m : Array[Array[Long]]) : Array[Array[Long]] = {
        for (i <- 0 until m.length) {
          for (j <- i until m.length) {
            m(j)(i) = m(i)(j)
          }
        }
        m
      }


      override def genConst(): Gen[Array[Array[Long]]] = {
        val arrayGen : Gen[Array[Long]] = Gen.sequence[Array[Long],Long](Array.fill(nbNodes)(Gen.choose(0L,100000L)))
        val matrixGen : Gen[Array[Array[Long]]] = Gen.sequence[Array[Array[Long]],Array[Long]](Array.fill(nbNodes)(arrayGen))
        for (
          m <- matrixGen
        ) yield makeItSymetric(m)
        Gen.const(Array.fill(nbNodes)(Array.fill(nbNodes)(0)))
      }

      override def createTestBenchSut(model: Store, inputData: Array[Array[Long]]): TestBenchSut = {
        val vrp = VRP(model,nbNodes,nbVehicles)
        val routeLength = TotalRouteLength(vrp,inputData)
        TestBenchSut(routeLength,Array(vrp.routes),Array(routeLength.routeLength))
      }

      override def typeTToString(elem: Array[Array[Long]]) = {
        elem.map(l => l.mkString(" ; ")).mkString("\n")
      }

    }

    val bench = new TotalRouteLengthTestBench

    bench.test()

  }

}
