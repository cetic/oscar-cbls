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

package oscar.cbls.test.lib.invariant.logic

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.lib.invariant.logic.MultiElements
import oscar.cbls.test.invBench.{InvTestBench, TestBenchSut}

class MultiElementsTestSuite extends AnyFunSuite with Matchers {

  private def testMultiElements()
    : (Store, Array[IntVariable], SetVariable, SetVariable, MultiElements) = {
    val store                                 = new Store(debugLevel = 3)
    val values: Array[Long]                   = Array(42, 7, 14, 21, 28, 35, 42)
    val input: Array[IntVariable]             = values.map(x => IntVariable(store, x))
    val listenedVariablesIndices: SetVariable = SetVariable(store, Set(0, 2, 4))
    val output: SetVariable                   = SetVariable(store, Set.empty)
    val inv: MultiElements = MultiElements(store, input, listenedVariablesIndices, output)
    store.close()

    (store, input, listenedVariablesIndices, output, inv)
  }

  test("MultiElements: initialization works as expected.") {
    val (_, _, _, output, _) = testMultiElements()

    output.value() should contain only (42, 14, 28)
  }

  test("MultiElements: changing a listened variable to a new value") {
    val (_, input, _, output, _) = testMultiElements()
    input(2) := -7

    output.value() should contain only (-7, 28, 42)
  }

  test("MultiElements: changing a listened variable to an already returned value") {
    val (_, input, _, output, _) = testMultiElements()
    input(2) := 42

    output.value() should contain only (28, 42)
  }

  test("MultiElements: changing a not listened value") {
    val (_, input, _, output, _) = testMultiElements()
    input(3) := -7

    output.value() should contain only (14, 28, 42)
  }

  test("MultiElements: adding a new value") {
    val (_, _, lvi, output, _) = testMultiElements()
    lvi :+= 5

    output.value() should contain only (14, 28, 35, 42)
  }

  test("MultiElements: removing a value") {
    val (_, _, lvi, output, _) = testMultiElements()
    lvi :-= 4

    output.value() should contain only (14, 42)
  }

  test("MultiElements: adding a duplicated value") {
    val (_, _, lvi, output, _) = testMultiElements()
    lvi :+= 6

    output.value() should contain only (14, 28, 42)
  }

  test("MultiElements: removing a duplicated value") {
    val (store, _, lvi, output, _) = testMultiElements()
    lvi :+= 6
    store.propagate()
    lvi :-= 0

    output.value() should contain only (14, 28, 42)
  }

  test("MultiElements: checkInternal doesn't fail") {
    val (store, input, lvi, _, inv) = testMultiElements()
    input(2) := 42
    lvi :+= 3
    lvi :-= 0
    input(5) :*= 2
    lvi :+= 5
    store.propagate()

    noException should be thrownBy inv.checkInternals()
  }

  test("MultiElements: checkInternal should fail") {
    val (store, _, _, output, inv) = testMultiElements()
    store.propagate()

    output :+= 24

    an[IllegalArgumentException] should be thrownBy inv.checkInternals()
  }

  test("MultiElements: test bench") {
    def createMultiElements(model: Store): TestBenchSut = {
      val nbValues = 1000
      val input    = Array.fill(nbValues)(IntVariable(model, 0))
      val listened = SetVariable(model, Set.empty)
      listened.setDomain(0, nbValues - 1)
      val output = SetVariable(model, Set.empty)
      val inv    = MultiElements(model, input, listened, output)

      TestBenchSut(inv, listened +: input, Array(output))
    }

    val bench = InvTestBench(createMultiElements, "Test MultiElements Invariant")
    bench.test()
  }

}
