package oscar.cbls.test.lib.invariant.routing.totalRouteLength

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.core.computation.Store
import oscar.cbls.lib.invariant.routing.TotalRouteLength
import oscar.cbls.modeling.routing.VRS
import oscar.cbls.test.invBench.{InvTestBenchWithConstGen, TestBenchSut}

class TotalRouteLengthTestsWithBench extends AnyFunSuite with Matchers {

  private val nbNodes    = 10
  private val nbVehicles = 2

  /** Creates a distance matrix with given properties.
    *
    * @param m
    *   The original matrix randomly generated
    * @param symmetrical
    *   A flag saying if the output matrix should be symmetrical
    * @param zeroDiag
    *   A flag saying if the output matrix should have 0 in the diagonal
    * @return
    */
  private def mkMatrix(
    m: Array[Array[Long]],
    symmetrical: Boolean,
    zeroDiag: Boolean
  ): Array[Array[Long]] = {
    for (i <- m.indices) {
      for (j <- i until m.length) {
        if (zeroDiag) {
          if (i == j)
            m(i)(j) = 0
        }
        if (symmetrical)
          m(j)(i) = m(i)(j)
      }
    }
    m
  }

  test("TotalRouteLength working in bench, symmetrical matrix") {
    class TotalRouteLengthTestBench
        extends InvTestBenchWithConstGen[Array[Array[Long]]]("TotalRouteLength Symmetrical Matrix") {

      override def genConst(): Gen[Array[Array[Long]]] = {
        val arrayGen: Gen[Array[Long]] =
          Gen.sequence[Array[Long], Long](Array.fill(nbNodes)(Gen.choose(0L, 100000L)))
        val matrixGen: Gen[Array[Array[Long]]] =
          Gen.sequence[Array[Array[Long]], Array[Long]](Array.fill(nbNodes)(arrayGen))
        for (m <- matrixGen) yield mkMatrix(m, symmetrical = true, zeroDiag = false)
      }

      override def createTestBenchSut(model: Store, inputData: Array[Array[Long]]): TestBenchSut = {

        val vrs         = VRS(model, nbNodes, nbVehicles)
        val routeLength = TotalRouteLength(vrs, inputData)
        TestBenchSut(routeLength, Array(vrs.routes), Array(routeLength.routeLength), Some(vrs))
      }

      override def typeTToString(elem: Array[Array[Long]]): String = {
        s"Array(Array(${elem.map(l => l.mkString(",")).mkString("),\n Array(")})"
      }

    }

    val bench = new TotalRouteLengthTestBench

    bench.changePropagationMoveFrequency(1, 7)

    bench.test()

  }

  test("TotalRouteLength working in bench, symmetrical matrix, 0 on the diagonal") {
    class TotalRouteLengthTestBench
        extends InvTestBenchWithConstGen[Array[Array[Long]]](
          "TotalRouteLength Symmetrical Matrix, 0 diagonal"
        ) {

      override def genConst(): Gen[Array[Array[Long]]] = {
        val arrayGen: Gen[Array[Long]] =
          Gen.sequence[Array[Long], Long](Array.fill(nbNodes)(Gen.choose(0L, 100000L)))
        val matrixGen: Gen[Array[Array[Long]]] =
          Gen.sequence[Array[Array[Long]], Array[Long]](Array.fill(nbNodes)(arrayGen))
        for (m <- matrixGen) yield mkMatrix(m, symmetrical = true, zeroDiag = true)
      }

      override def createTestBenchSut(model: Store, inputData: Array[Array[Long]]): TestBenchSut = {

        val vrs         = VRS(model, nbNodes, nbVehicles)
        val routeLength = TotalRouteLength(vrs, inputData)
        TestBenchSut(routeLength, Array(vrs.routes), Array(routeLength.routeLength), Some(vrs))
      }

      override def typeTToString(elem: Array[Array[Long]]): String = {
        s"Array(Array(${elem.map(l => l.mkString(",")).mkString("),\n Array(")})"
      }

    }

    val bench = new TotalRouteLengthTestBench

    bench.changePropagationMoveFrequency(1, 7)

    bench.test()

  }

  test("TotalRouteLength working in bench, asymmetrical matrix") {
    class TotalRouteLengthTestBench
        extends InvTestBenchWithConstGen[Array[Array[Long]]](
          "TotalRouteLength Asymmetrical Matrix"
        ) {

      override def genConst(): Gen[Array[Array[Long]]] = {
        val arrayGen: Gen[Array[Long]] =
          Gen.sequence[Array[Long], Long](Array.fill(nbNodes)(Gen.choose(0L, 100000L)))
        Gen.sequence[Array[Array[Long]], Array[Long]](Array.fill(nbNodes)(arrayGen))
      }

      override def createTestBenchSut(model: Store, inputData: Array[Array[Long]]): TestBenchSut = {

        val vrs         = VRS(model, nbNodes, nbVehicles)
        val routeLength = TotalRouteLength(vrs, inputData)
        TestBenchSut(routeLength, Array(vrs.routes), Array(routeLength.routeLength), Some(vrs))
      }

      override def typeTToString(elem: Array[Array[Long]]): String = {
        s"Array(Array(${elem.map(l => l.mkString(",")).mkString("),\n Array(")})"
      }

    }

    val bench = new TotalRouteLengthTestBench

    bench.changePropagationMoveFrequency(1, 7)

    bench.test()

  }

  test("TotalRouteLength working in bench, asymmetrical matrix, 0 on the diagonal") {
    class TotalRouteLengthTestBench
        extends InvTestBenchWithConstGen[Array[Array[Long]]](
          "TotalRouteLength Asymmetrical Matrix, 0 diagonal"
        ) {

      override def genConst(): Gen[Array[Array[Long]]] = {
        val arrayGen: Gen[Array[Long]] =
          Gen.sequence[Array[Long], Long](Array.fill(nbNodes)(Gen.choose(0L, 100000L)))
        val matrixGen: Gen[Array[Array[Long]]] =
          Gen.sequence[Array[Array[Long]], Array[Long]](Array.fill(nbNodes)(arrayGen))
        for (m <- matrixGen) yield mkMatrix(m, symmetrical = false, zeroDiag = true)
      }

      override def createTestBenchSut(model: Store, inputData: Array[Array[Long]]): TestBenchSut = {

        val vrs         = VRS(model, nbNodes, nbVehicles)
        val routeLength = TotalRouteLength(vrs, inputData)
        TestBenchSut(routeLength, Array(vrs.routes), Array(routeLength.routeLength), Some(vrs))
      }

      override def typeTToString(elem: Array[Array[Long]]): String = {
        s"Array(Array(${elem.map(l => l.mkString(",")).mkString("),\n Array(")})"
      }

    }

    val bench = new TotalRouteLengthTestBench

    bench.changePropagationMoveFrequency(1, 7)

    bench.test()

  }

}
