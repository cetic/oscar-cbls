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

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.lib.invariant.numeric.Prod
import oscar.cbls.test.invBench.{InvTestBench, TestBenchSut}

class ProdTestSuite extends AnyFunSuite with Matchers {

  private def testProd(
    set: Set[Int]
  ): (Store, Array[IntVariable], SetVariable, IntVariable, Prod) = {
    val store: Store              = new Store(debugLevel = 3)
    val input: Array[IntVariable] = Array.range(0, 7).map(i => IntVariable(store, i % 6))
    val listenedVariablesIndices: SetVariable = SetVariable(store, set)
    val output                                = IntVariable(store, 0)
    val inv                                   = Prod(store, input, listenedVariablesIndices, output)
    store.close()

    (store, input, listenedVariablesIndices, output, inv)
  }

  test("Prod: initialization without zero element. Expected result: 120") {
    val (_, _, _, output, _) = testProd(Set.range(1, 6))

    output.value() should be(120)
  }

  test("Prod: initialization with zero element. Expected result: 0") {
    val (_, _, _, output, _) = testProd(Set.range(0, 6))

    output.value() should be(0)
  }

  test(
    "Prod: changing non zero element listened value to another non zero element value. " +
      "Expected result: 42"
  ) {
    val (_, input, _, output, _) = testProd(Set(2, 3, 4))
    input(4) :+= 3

    output.value() should be(42)
  }

  test("Prod: changing a non zero element value to zero element. Expected result: 0") {
    val (_, input, _, output, _) = testProd(Set(2, 3, 4))
    input(4) := 0

    output.value() should be(0)
  }

  test("Prod changing a zero element to a non zero element value. Expected result: 240") {
    val (_, input, _, output, _) = testProd(Set(0, 2, 3, 4))
    input(0) := 10

    output.value() should be(240)
  }

  test("Prod: adding a non zero element value to a non zero element product. Expected result: 30") {
    val (_, _, lvi, output, _) = testProd(Set(2, 3))
    lvi :+= 5

    output.value() should be(30)
  }

  test("Prod: adding a value to a zero element product. Expected result: 0") {
    val (_, _, lvi, output, _) = testProd(Set(0, 2, 3))
    lvi :+= 5

    output.value() should be(0)
  }

  test("Prod: adding a zero to a non zero element product. Expected result: 0") {
    val (_, _, lvi, output, _) = testProd(Set(2, 3))
    lvi :+= 0

    output.value() should be(0)
  }

  test(
    "Prod: removing a non zero element value from a non zero element product. Expected " +
      "result: 8"
  ) {
    val (_, _, lvi, output, _) = testProd(Set(2, 3, 4))
    lvi :-= 3

    output.value() should be(8)
  }

  test("Prod: removing a non zero element value from a zero element product. Expected result: 0") {
    val (_, _, lvi, output, _) = testProd(Set(0, 2, 3))
    lvi :-= 2

    output.value() should be(0)
  }

  test("Prod: removing the only 0 factor from a zero element product. Expected result: 6") {
    val (_, _, lvi, output, _) = testProd(Set(0, 2, 3))
    lvi :-= 0

    output.value() should be(6)
  }

  test(
    "Prod: removing one of the zero element factors from a zero element product. Expected " +
      "result: 0"
  ) {
    val (_, _, lvi, output, _) = testProd(Set.range(0, 7))
    lvi :-= 0

    output.value() should be(0)
  }

  test("Prod: checkInternal doesn't fail") {
    val (store, input, lvi, _, inv) = testProd(Set.range(2, 5))
    lvi :+= 5
    lvi :+= 6
    input(2) := 10
    input(6) := -7
    lvi :-= 3
    store.propagate()

    inv.checkInternals()
  }

  test("Prod: test bench") {
    def createProd(model: Store): TestBenchSut = {
      val nbValues = 1000
      val input    = Array.fill(nbValues)(IntVariable(model, 0))
      val listened = SetVariable(model, Set.empty[Int])
      listened.setDomain(0, nbValues - 1)
      val output = IntVariable(model, 0)
      val inv    = Prod(model, input, listened, output)
      TestBenchSut(inv, listened +: input, Array(output))
    }

    val bench = InvTestBench(createProd, "Test Prod Invariant")
    bench.test()
  }
}
