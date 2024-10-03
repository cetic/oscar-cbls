package oscar.cbls.test.lib.invariant.routing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.test.invBench.InvTestBenchWithConstGen
import oscar.cbls.model.routing.VRP
import oscar.cbls.core.computation.Store
import oscar.cbls.lib.invariant.routing.TotalRouteLength
import org.scalacheck.Gen
import oscar.cbls.test.invBench.TestBenchSut
import oscar.cbls.algo.sequence.IntSequence

class TotalRouteLengthTestsWithBench extends AnyFunSuite with Matchers {

  test("TotalRouteLength invariant is working in test bench") {
    val nbNodes    = 10
    val nbVehicles = 2

    class TotalRouteLengthTestBench
        extends InvTestBenchWithConstGen[Array[Array[Long]]]("TotalRouteLength Test Bench") {

      private def makeItSymetric(m: Array[Array[Long]]): Array[Array[Long]] = {
        for (i <- 0 until m.length) {
          for (j <- i until m.length) {
            m(j)(i) = m(i)(j)
          }
        }
        m
      }

      override def genConst(): Gen[Array[Array[Long]]] = {
        val arrayGen: Gen[Array[Long]] =
          Gen.sequence[Array[Long], Long](Array.fill(nbNodes)(Gen.choose(0L, 100000L)))
        val matrixGen: Gen[Array[Array[Long]]] =
          Gen.sequence[Array[Array[Long]], Array[Long]](Array.fill(nbNodes)(arrayGen))
        for (m <- matrixGen) yield makeItSymetric(m)
      }

      override def createTestBenchSut(model: Store, inputData: Array[Array[Long]]): TestBenchSut = {

        val vrp         = VRP(model, nbNodes, nbVehicles)
        val routeLength = TotalRouteLength(vrp, inputData)
        TestBenchSut(routeLength, Array(vrp.routes), Array(routeLength.routeLength), Some(vrp))
      }

      override def typeTToString(elem: Array[Array[Long]]) = {
        s"Array(Array(${elem.map(l => l.mkString(",")).mkString("),\n Array(")})"
      }

    }

    val bench = new TotalRouteLengthTestBench

    bench.test()

  }

  test("TotalRouteLength invariant is working in test bench with 0 on the diagonal") {
    val nbNodes    = 10
    val nbVehicles = 2

    class TotalRouteLengthTestBench
        extends InvTestBenchWithConstGen[Array[Array[Long]]]("TotalRouteLength Test Bench") {

      private def makeItSymetric(m: Array[Array[Long]]): Array[Array[Long]] = {
        for (i <- 0 until m.length) {
          for (j <- i until m.length) {
            if (i == j)
              m(i)(i) == 0
            else
              m(j)(i) = m(i)(j)
          }
        }
        m
      }

      override def genConst(): Gen[Array[Array[Long]]] = {
        val arrayGen: Gen[Array[Long]] =
          Gen.sequence[Array[Long], Long](Array.fill(nbNodes)(Gen.choose(0L, 100000L)))
        val matrixGen: Gen[Array[Array[Long]]] =
          Gen.sequence[Array[Array[Long]], Array[Long]](Array.fill(nbNodes)(arrayGen))
        for (m <- matrixGen) yield makeItSymetric(m)
      }

      override def createTestBenchSut(model: Store, inputData: Array[Array[Long]]): TestBenchSut = {

        val vrp         = VRP(model, nbNodes, nbVehicles)
        val routeLength = TotalRouteLength(vrp, inputData)
        TestBenchSut(routeLength, Array(vrp.routes), Array(routeLength.routeLength), Some(vrp))
      }

      override def typeTToString(elem: Array[Array[Long]]) = {
        s"Array(Array(${elem.map(l => l.mkString(",")).mkString("),\n Array(")})"
      }

    }

    val bench = new TotalRouteLengthTestBench

    bench.test()

  }

}
