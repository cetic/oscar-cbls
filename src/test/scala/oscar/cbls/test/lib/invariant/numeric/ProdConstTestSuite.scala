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

package oscar.cbls.test.lib.invariant.numeric

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.lib.invariant.numeric.ProdConst
import oscar.cbls.test.invBench.{InvTestBenchWithConstGen, TestBenchSut}

class ProdConstTestSuite extends AnyFunSuite with Matchers {

  private def testProdConst(set: Set[Int]): (Store, SetVariable, IntVariable, ProdConst) = {
    val store: Store                          = new Store(debugLevel = 3)
    val input: Array[Long]                    = Array.from((0L to 6L).map(i => i % 6))
    val listenedVariablesIndices: SetVariable = SetVariable(store, set)
    val output                                = IntVariable(store, 0)
    val inv = ProdConst(store, input, listenedVariablesIndices, output)
    store.close()

    (store, listenedVariablesIndices, output, inv)
  }

  test("Prod: adding a non zero element value to a non zero element product. Expected result: 30") {
    val (_, lvi, output, _) = testProdConst(Set(2, 3))
    lvi :+= 5

    output.value() should be(30)
  }

  test("Prod: adding a value to a zero element product. Expected result: 0") {
    val (_, lvi, output, _) = testProdConst(Set(0, 2, 3))
    lvi :+= 5

    output.value() should be(0)
  }

  test("Prod: adding a zero to a non zero element product. Expected result: 0") {
    val (_, lvi, output, _) = testProdConst(Set(2, 3))
    lvi :+= 0

    output.value() should be(0)
  }

  test(
    "Prod: removing a non zero element value from a non zero element product. Expected " +
      "result: 8"
  ) {
    val (_, lvi, output, _) = testProdConst(Set(2, 3, 4))
    lvi :-= 3

    output.value() should be(8)
  }

  test("Prod: removing a non zero element value from a zero element product. Expected result: 0") {
    val (_, lvi, output, _) = testProdConst(Set(0, 2, 3))
    lvi :-= 2

    output.value() should be(0)
  }

  test("Prod: removing the only 0 factor from a zero element product. Expected result: 6") {
    val (_, lvi, output, _) = testProdConst(Set(0, 2, 3))
    lvi :-= 0

    output.value() should be(6)
  }

  test(
    "Prod: removing one of the zero element factors from a zero element product.  Expected " +
      "result: 0"
  ) {
    val (_, lvi, output, _) = testProdConst(Set.range(0, 7))
    lvi :-= 0

    output.value() should be(0)
  }

  test("Prod: checkInternal doesn't fail") {
    val (store, lvi, _, inv) = testProdConst(Set.range(2, 5))
    lvi :+= 5
    lvi :+= 6
    lvi :-= 3
    store.propagate()

    noException should be thrownBy inv.checkInternals()
  }

  test("ProdConst: test bench") {
    class ProdConstTestBench extends InvTestBenchWithConstGen[Array[Long]]("ProdConst Test Bench") {

      override def genConst(): Gen[Array[Long]] = {
        for {
          size  <- Gen.choose(1, 10)
          array <- Gen.listOfN(size, Gen.choose(0L, 75L))
        } yield array.toArray // Values such that their product does not cause overflow
      }

      override def createTestBenchSut(model: Store, inputData: Array[Long]): TestBenchSut = {
        val listened: SetVariable = SetVariable(model, Set.empty[Int])
        listened.setDomain(0, inputData.length - 1)
        val output: IntVariable = IntVariable(model, 0)
        val inv: ProdConst      = ProdConst(model, inputData, listened, output)

        TestBenchSut(inv, Array(listened), Array(output))
      }

      override def typeTToString(elem: Array[Long]): String = {
        s"""(${Array.tabulate(elem.length)(i => s"$i -> ${elem(i)}").mkString("; ")})
           |Values: ${elem.mkString(", ")}
           |""".stripMargin
      }
    }

    val bench = new ProdConstTestBench
    bench.test()
  }
}
