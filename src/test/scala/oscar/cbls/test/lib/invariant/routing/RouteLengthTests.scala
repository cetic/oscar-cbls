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

package oscar.cbls.test.lib.invariant.routing

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.{Store, Variable}
import oscar.cbls.lib.invariant.routing.RouteLength
import oscar.cbls.modeling.routing.VRP
import oscar.cbls.test.invBench.{InvTestBenchWithConstGen, TestBenchSut}

class RouteLengthTests extends AnyFunSuite with Matchers {

  private val distanceMatrix: Array[Array[Long]] =
    Array(
      Array(0, 2, 3, 5, 7, 9),
      Array(11, 0, 13, 17, 19, 23),
      Array(29, 31, 0, 37, 41, 43),
      Array(47, 53, 59, 0, 61, 67),
      Array(71, 73, 79, 83, 0, 89),
      Array(97, 101, 103, 107, 109, 0)
    )

  private val sparseMatrix: Array[Array[Long]] =
    Array(
      Array(0, 2, 3, 5, 7, 9),
      Array(0, 13, 17, 19, 23),
      Array(0, 37, 41, 43),
      Array(0, 61, 67),
      Array(0, 89),
      Array(0)
    )

  test("RouteLength: initialization works as expected") {
    val model = new Store(debugLevel = 3)
    val vrp   = VRP(model, 6, 2)
    val inv   = RouteLength(vrp, distanceMatrix, matrixIsTriangular = false)
    model.close()

    inv(0).value() must be(0L)
    inv(1).value() must be(0L)

    inv.precomputedValues must contain only RouteLength.PrecomputedDistance(0L, 0L)

  }

  test("RouteLength: initialization with sparse matrix works as expected") {
    val model = new Store(debugLevel = 3)
    val vrp   = VRP(model, 6, 2)
    val inv   = RouteLength(vrp, sparseMatrix, matrixIsTriangular = true)
    model.close()

    inv(0).value() must be(0L)
    inv(1).value() must be(0L)

    inv.precomputedValues must contain only RouteLength.PrecomputedDistance(0L, 0L)
  }

  test("RouteLength: Assign works matrix") {
    val model = new Store(debugLevel = 3)
    val vrp   = VRP(model, 6, 2)
    val inv   = RouteLength(vrp, distanceMatrix, matrixIsTriangular = false)
    model.close()

    vrp.routes := IntSequence(List.from(0 until 6 by 2) ::: List.from(1 until 6 by 2))
    model.propagate()

    inv(0).value() must be(115)
    inv(1).value() must be(185)
  }

  test("RouteLength: Assign works with sparse matrix") {
    val model = new Store(debugLevel = 3)
    val vrp   = VRP(model, 6, 2)
    val inv   = RouteLength(vrp, sparseMatrix, matrixIsTriangular = true)
    model.close()

    vrp.routes := IntSequence(List.from(0 until 6 by 2) ::: List.from(1 until 6 by 2))
    model.propagate()

    inv(0).value() must be(51)
    inv(1).value() must be(107)
  }

  test("RouteLength: precomputations are correct") {
    val model = new Store(debugLevel = 3)
    val vrp   = VRP(model, 6, 2)
    val inv   = RouteLength(vrp, distanceMatrix, matrixIsTriangular = false)
    model.close()

    vrp.routes := IntSequence(List.from(0 until 6 by 2) ::: List.from(1 until 6 by 2))
    vrp.routes.defineCurrentValueAsCheckpoint()
    model.propagate()

    inv(0).value() must be(115)
    inv(1).value() must be(185)

    val precompute = inv.precomputedValues
    precompute(0) must equal(RouteLength.PrecomputedDistance(0L, 0L))
    precompute(2) must equal(RouteLength.PrecomputedDistance(3L, 29L))
    precompute(4) must equal(RouteLength.PrecomputedDistance(44L, 108L))
    precompute(1) must equal(RouteLength.PrecomputedDistance(0L, 0L))
    precompute(3) must equal(RouteLength.PrecomputedDistance(17L, 53L))
    precompute(5) must equal(RouteLength.PrecomputedDistance(84L, 160L))
  }

  private class RouteLengthTestBench(
    n: Int,
    v: Int,
    triangular: Boolean,
    additionalSeed: List[String] = List()
  ) extends InvTestBenchWithConstGen[Array[Array[Long]]](
        s"RouteLength ${if (triangular) "with sparse matrix"}",
        additionalSeed
      ) {

    override def genConst(): Gen[Array[Array[Long]]] = {
      val arrayGen: Gen[Array[Long]] =
        Gen.sequence[Array[Long], Long](Array.fill(n)(Gen.choose(0L, 100000L)))
      val matrixGen: Gen[Array[Array[Long]]] =
        Gen.sequence[Array[Array[Long]], Array[Long]](Array.fill(n)(arrayGen))

      if (triangular) {
        for (m <- matrixGen) yield makeItTriangular(m)
      } else matrixGen
    }

    override def createTestBenchSut(model: Store, inputData: Array[Array[Long]]): TestBenchSut = {
      val vrp                     = VRP(model, n, v)
      val inv                     = RouteLength(vrp, inputData, triangular)
      val output: Array[Variable] = Array.from(inv())
      TestBenchSut(inv, Array(vrp.routes), output, Some(vrp))
    }

    override def typeTToString(elem: Array[Array[Long]]): String = {
      s"Array(Array(${elem.map(l => l.mkString(",")).mkString("),\n Array(")})"
    }

    private def makeItTriangular(m: Array[Array[Long]]): Array[Array[Long]] = {
      val toReturn: Array[Array[Long]] = new Array[Array[Long]](m.length)
      for (i <- m.indices) toReturn(i) = m(i).drop(i)
      toReturn
    }
  }

  test("RouteLength: test bench") {
    val n = 25
    val v = 5

    val bench = new RouteLengthTestBench(n, v, triangular = false)
    bench.changePropagationMoveFrequency(1, 7)
    bench.test()
  }

  test("RouteLength: test bench with sparse matrix") {
    val n = 25
    val v = 5

    val bench = new RouteLengthTestBench(n, v, triangular = true)
    bench.changePropagationMoveFrequency(1, 7)
    bench.test()
  }
}
