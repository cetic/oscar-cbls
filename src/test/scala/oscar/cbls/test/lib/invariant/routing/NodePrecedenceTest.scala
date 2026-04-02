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
import oscar.cbls.lib.invariant.routing.NodePrecedence
import oscar.cbls.modeling.Model
import oscar.cbls.modeling.routing.VRS
import oscar.cbls.util.invBench.{InvTestBenchWithConstGen, TestBenchSut}

import scala.collection.immutable.HashSet

class NodePrecedenceTest extends AnyFunSuite with Matchers {

  test("NodePrecedence: initialization works as expected") {
    val model       = new Store(debugLevel = 3)
    val vrs         = VRS(model, 11, 2)
    val precedences = Array((2, 4), (5, 10))
    val inv         = NodePrecedence(vrs, precedences)
    model.close()

    inv().value() must be(0L)
  }

  test("NodePrecedence: assigning route with respected precedence") {
    val model       = Model(debugLevel = 3)
    val vrs         = model.vrs(11, 2)
    val precedences = Array((2, 4), (5, 10))
    val inv         = NodePrecedence(vrs, precedences)
    model.close()

    vrs.routes := IntSequence(List(0, 2, 4, 1))
    model.store.propagate()

    inv().value() must be(0L)
  }

  test("NodePrecedence: assigning only the before node") {
    val model       = Model(debugLevel = 3)
    val vrs         = model.vrs(11, 2)
    val precedences = Array((2, 4), (5, 10))
    val inv         = NodePrecedence(vrs, precedences)
    model.close()

    vrs.routes := IntSequence(List(0, 2, 1))
    model.store.propagate()

    inv().value() must be(1L)
    inv.violatedPrecedences must contain only ((2, 4))
  }

  test("NodePrecedence: assigning only the after node") {
    val model       = Model(debugLevel = 3)
    val vrs         = model.vrs(11, 2)
    val precedences = Array((2, 4), (5, 10))
    val inv         = NodePrecedence(vrs, precedences)
    model.close()

    vrs.routes := IntSequence(List(0, 10, 1))
    model.store.propagate()

    inv().value() must be(1L)
    inv.violatedPrecedences must contain only ((5, 10))
  }

  test("NodePrecedence: assigning route with precedence in the wrong order") {
    val model       = Model(debugLevel = 3)
    val vrs         = model.vrs(11, 2)
    val precedences = Array((2, 4), (5, 10))
    val inv         = NodePrecedence(vrs, precedences)
    model.close()

    vrs.routes := IntSequence(List(0, 4, 2, 1))
    model.store.propagate()

    inv().value() must be(1L)
    inv.violatedPrecedences must contain only ((2, 4))
  }

  test("NodePrecedence: assigning route with respected precedence but on different vehicles") {
    val model       = Model(debugLevel = 3)
    val vrs         = model.vrs(11, 2)
    val precedences = Array((2, 4), (5, 10))

    val inv = NodePrecedence(vrs, precedences)
    model.close()

    vrs.routes := IntSequence(List(0, 5, 1, 10))
    model.store.propagate()

    inv().value() must be(1L)
    inv.violatedPrecedences must contain only ((5, 10))
  }

  test("NodePrecedence: the count of violated precedence is ok") {
    val model       = Model(debugLevel = 3)
    val vrs         = model.vrs(11, 2)
    val precedences = Array((2, 4), (2, 6), (2, 8), (2, 10), (3, 6), (3, 9), (4, 8), (5, 10))
    val inv         = NodePrecedence(vrs, precedences)
    model.close()

    vrs.routes := IntSequence(List(0, 2, 4, 5, 1, 9, 3, 6, 10))
    model.store.propagate()

    inv().value() must be(6L)
    inv.violatedPrecedences must contain only ((2, 6), (2, 8), (2, 10), (3, 9), (4, 8), (5, 10))
  }

  test("NodePrecedence: performs some move and define checkpoint") {
    val prec = Array(
      (2, 9),
      (9, 3),
      (7, 5),
      (2, 5),
      (2, 6),
      (9, 7),
      (1, 6),
      (9, 6),
      (3, 9),
      (4, 3),
      (1, 7),
      (0, 2),
      (5, 4),
      (4, 8),
      (6, 2),
      (1, 2),
      (1, 4),
      (4, 5),
      (6, 7),
      (4, 9),
      (2, 7),
      (9, 4),
      (1, 9),
      (2, 4),
      (3, 7),
      (6, 3),
      (3, 6),
      (3, 5),
      (7, 3),
      (0, 6),
      (0, 3)
    )

    val model = Model(debugLevel = 3)
    val vrs   = model.vrs(10, 2)
    NodePrecedence(vrs, prec)
    val route = vrs.routes
    model.close()

    val exp = route.pendingValue.explorerAtPosition(0).get
    route.insertAfterPosition(7, exp)
    model.store.propagate()

    route := IntSequence(List(0, 1, 8, 4))
    route.defineCurrentValueAsCheckpoint()

    val fromExp = route.pendingValue.explorerAtPosition(2).get
    val toExp   = route.pendingValue.explorerAtPosition(3).get

    route.flip(fromExp, toExp)
    model.store.propagate()

    route.rollbackToTopCheckpoint()

    noException mustBe thrownBy(model.store.propagate())
  }

  test("NodePrecedence: TestBench") {
    val n = 100
    val v = 10

    val bench = new NodePrecedenceTestBench(n, v)
    bench.test()

  }

  private class NodePrecedenceTestBench(n: Int, v: Int, additionalSeeds: List[String] = List())
      extends InvTestBenchWithConstGen[Array[(Int, Int)]](
        "NodePrecedence test bench",
        additionalSeeds
      ) {

    private val allNodesToRoute: Set[Int] = (v until n).toSet

    override def genConst(): Gen[Array[(Int, Int)]] = {
      val size: Int = 2 * n
      for {
        precedence <- Gen.sequence[HashSet[(Int, Int)], (Int, Int)](
          HashSet.fill(size)(genPrecedence())
        )
      } yield precedence.toArray
    }

    override def createTestBenchSut(model: Model, inputData: Array[(Int, Int)]): TestBenchSut = {
      val vrs = model.vrs(n, v)
      val inv = NodePrecedence(vrs, inputData)

      TestBenchSut(inv, Array(vrs.routes), Array(inv()), Some(vrs))
    }

    override def typeTToString(elem: Array[(Int, Int)]): String =
      elem.mkString("Array(", ", ", ")\n")

    private def genPrecedence(): Gen[(Int, Int)] = {
      for {
        before <- Gen.choose(0, n - 1)
        after  <- Gen.oneOf(allNodesToRoute - before)
      } yield (before, after)
    }
  }

}
