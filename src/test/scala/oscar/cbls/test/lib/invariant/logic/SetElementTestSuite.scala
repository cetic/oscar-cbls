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
import oscar.cbls.lib.invariant.logic.SetElement
import oscar.cbls.test.invBench.{InvTestBench, TestBenchSut}

class SetElementTestSuite extends AnyFunSuite with Matchers {

  private def testSetElement()
    : (Store, Array[SetVariable], IntVariable, SetVariable, SetElement) = {
    val store                     = new Store(debugLevel = 3)
    val values: Array[Set[Int]]   = Array(Set(2, 4, 8, 16), Set(2, 3, 5, 7, 11), Set(3, 14, 15, 9))
    val input: Array[SetVariable] = values.map(s => SetVariable(store, s))
    val index: IntVariable        = IntVariable(store, 2)
    val output: SetVariable       = SetVariable(store, Set.empty)
    val inv: SetElement           = SetElement(store, input, index, output)
    store.close()

    (store, input, index, output, inv)
  }

  test("SetElement: initialization works as expected") {
    val (_, _, _, output, _) = testSetElement()

    output.value() should be(Set(3, 14, 15, 9))
  }

  test("SetElement: changing the index") {
    val (_, _, index, output, inv) = testSetElement()
    index := 0

    output.value() should be(Set(2, 4, 8, 16))
    noException should be thrownBy inv.checkInternals()
  }

  test("SetElement: changing the listened set") {
    val (_, input, _, output, inv) = testSetElement()
    input(2) :-= 9

    output.value() should be(Set(3, 14, 15))
    noException should be thrownBy inv.checkInternals()
  }

  test("SetElement: changing a non listened set") {
    val (_, input, _, output, inv) = testSetElement()
    input(0) :+= 32

    output.value() should be(Set(3, 14, 15, 9))
    noException should be thrownBy inv.checkInternals()
  }

  test("SetElement: changing the listened value and changing its set") {
    val (_, input, index, output, _) = testSetElement()
    index    := 0
    input(0) := Set(42, 84, 126)

    output.value() should contain only (42, 84, 126)
  }

  test("SetElement: checkInternals should fail") {
    val (store, _, _, output, inv) = testSetElement()
    store.propagate()
    output := Set(1, 3, 5, 7)
    an[IllegalArgumentException] should be thrownBy inv.checkInternals()
  }

  test("SetElement: test bench") {
    def createSetElement(model: Store): TestBenchSut = {
      val nbValues = 1000
      val input    = Array.fill(nbValues)(SetVariable(model, Set.empty))
      val index    = IntVariable(model, 0)
      index.setDomain(0, nbValues - 1)
      val output = SetVariable(model, Set.empty)
      val inv    = SetElement(model, input, index, output)

      TestBenchSut(inv, index +: input, Array(output))
    }

    val bench = InvTestBench(createSetElement, "Test SetElement Invariant")
    bench.test()
  }

}
