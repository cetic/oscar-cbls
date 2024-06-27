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
import oscar.cbls.lib.invariant.numeric.Sum

class SumTestSuite extends AnyFunSuite with Matchers {

  private def testSum(set: Set[Int]): (Store, Array[IntVariable], SetVariable, IntVariable, Sum) = {
    val store: Store                          = new Store(debugLevel = 3)
    val input: Array[IntVariable]             = Array.range(0, 6).map(i => IntVariable(store, i))
    val listenedVariablesIndices: SetVariable = SetVariable(store, set)
    val output                                = IntVariable(store, 0)
    val inv                                   = Sum(store, input, listenedVariablesIndices, output)
    store.close()

    (store, input, listenedVariablesIndices, output, inv)
  }

  test("Sum: initialization. Expected result: 15") {
    val (_, _, _, output, _) = testSum(Set(0, 1, 2, 3, 4, 5))

    output.value() should be(15)
  }

  test("Sum: increase listened variable. Expected result: 42") {
    val (_, input, _, output, _) = testSum(Set(2, 3, 4))
    input(3) :*= 12

    output.value() should be(42)
  }

  test("Sum: decrease listened variable. Expected result: -42") {
    val (_, input, _, output, _) = testSum(Set(2, 3, 4))
    input(3) :-= 51

    output.value() should be(-42)
  }

  test("Sum: add new listened variable. Expected result: 10") {
    val (_, _, lvi, output, _) = testSum(Set(2, 3, 4))
    lvi :+= 1

    output.value() should be(10)
  }

  test("Sum: remove listened variable. Expected result: 5") {
    val (_, _, lvi, output, _) = testSum(Set(2, 3, 4))
    lvi :-= 4

    output.value() should be(5)
  }

  test("Sum: checkInternals doesn't fail") {
    val (store, input, lvi, _, inv) = testSum(Set(2, 4))
    lvi :+= 5
    lvi :+= 1
    lvi :-= 2
    input(4) :/= 2
    store.propagate()

    noException should be thrownBy inv.checkInternals()
  }
}
