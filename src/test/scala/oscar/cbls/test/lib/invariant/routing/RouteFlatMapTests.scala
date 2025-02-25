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
import oscar.cbls.core.computation.Store
import oscar.cbls.lib.invariant.routing.RouteFlatMap
import oscar.cbls.modeling.routing.VRS
import oscar.cbls.test.invBench.{InvTestBenchWithConstGen, TestBenchSut}

class RouteFlatMapTests extends AnyFunSuite with Matchers {

  test("RouteFlatMap: asymmetric function are forbidden") {
    val model = new Store(debugLevel = 3)
    val vrs   = VRS(model, 5, 2)
    val _ =
      an[IllegalArgumentException] must be thrownBy RouteFlatMap(
        vrs,
        (x: Int, y: Int) => Set.from(x - y to x + y)
      )
  }

  test("RouteFlatMap: checkInternals fails with wrong output") {
    val model = new Store(debugLevel = 3)
    val vrs   = VRS(model, 5, 2)
    val inv   = RouteFlatMap(vrs, (_, _) => Set(0, 1, 2))
    model.close()
    vrs.routes := IntSequence(List(0, 2, 3, 1, 4))
    model.propagate()
    inv.output := Set(0, 9, 5, 7)
    an[IllegalArgumentException] must be thrownBy inv.checkInternals()

  }

  test("RouteFlatMap: checkInternals fails with wrong number of duplicates") {
    val model = new Store(debugLevel = 3)
    val vrs   = VRS(model, 5, 2)
    val inv   = RouteFlatMap(vrs, (_, _) => Set(0, 1, 2))
    model.close()
    vrs.routes := IntSequence(List(0, 2, 3, 1, 4))
    model.propagate()
    inv.numDuplicates := 10
    an[IllegalArgumentException] must be thrownBy inv.checkInternals()
  }

  test("RouteFlatMap: nbOccurrence works as expected") {
    val model = new Store(debugLevel = 3)
    val vrs   = VRS(model, 3, 1)
    val mapData: Array[Array[Set[Int]]] =
      Array(Array(Set.empty, Set(0, 1), Set(1, 2)), Array(Set.empty, Set(1, 2, 3)), Array(Set(5)))
    val map = (i: Int, j: Int) => if (i <= j) mapData(i)(j - i) else mapData(j)(i - j)
    val inv = RouteFlatMap(vrs, map)
    model.close()
    vrs.routes := IntSequence(List(0, 1, 2))
    model.propagate()

    inv.nbOccurrence(0) must be(1)
    inv.nbOccurrence(1) must be(3)
    inv.nbOccurrence(2) must be(2)
    inv.nbOccurrence(3) must be(1)
    inv.nbOccurrence(4) must be(0)
    inv.nbOccurrence(5) must be(1)
  }

  test("RouteFlatMap test bench") {
    val n = 25
    val v = 5

    class RouteFlatMapTestBench(additionalSeed: List[String] = List())
        extends InvTestBenchWithConstGen[Array[Array[Set[Int]]]](
          "RouteFlatMap test bench",
          additionalSeed
        ) {

      override def genConst(): Gen[Array[Array[Set[Int]]]] = {
        val setGen: Gen[Set[Int]] =
          for {
            size <- Gen.choose(1, 10)
            set  <- Gen.sequence[Set[Int], Int](Set.fill(size)(Gen.choose(0, 50)))
          } yield set

        val rowGen: Gen[Array[Set[Int]]] =
          Gen.sequence[Array[Set[Int]], Set[Int]](Array.fill(n)(setGen))
        val genMatrix: Gen[Array[Array[Set[Int]]]] =
          Gen.sequence[Array[Array[Set[Int]]], Array[Set[Int]]](Array.fill(n)(rowGen))
        for {
          m <- genMatrix
        } yield makeItSymmetric(m)
      }

      override def createTestBenchSut(
        model: Store,
        inputData: Array[Array[Set[Int]]]
      ): TestBenchSut = {
        val vrs: VRS = VRS(model, n, v)
        val inv      = RouteFlatMap(vrs, (i, j) => inputData(i)(j))
        TestBenchSut(inv, Array(vrs.routes), Array(inv.output), Some(vrs))
      }

      override def typeTToString(elem: Array[Array[Set[Int]]]): String = {
        s"Data:\nArray(Array(${elem.map(l => l.mkString(",")).mkString("),\n Array(")})"
      }

      private def makeItSymmetric(m: Array[Array[Set[Int]]]): Array[Array[Set[Int]]] = {
        for (i <- m.indices) {
          for (j <- i until m.length) m(j)(i) = m(i)(j)
        }
        m
      }
    }

    val bench = new RouteFlatMapTestBench()
    bench.test()
  }
}
