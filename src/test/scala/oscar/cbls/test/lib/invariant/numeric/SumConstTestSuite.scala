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
import oscar.cbls.core.computation.integer.{IntConstant, IntVariable}
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.lib.invariant.numeric.{Sum, SumConst}

class SumConstTestSuite extends AnyFunSuite with Matchers {

  private def testSumConst(set: Set[Int]): (Store, SetVariable, IntVariable, SumConst) = {
    val store: Store              = new Store(debugLevel = 3)
    val input: Array[IntConstant] = Array.range(0, 6).map(i => new IntConstant(store, i))
    val listenedVariablesIndices: SetVariable = SetVariable(store, set)
    val output                                = IntVariable(store, 0)
    val inv = SumConst(store, input, listenedVariablesIndices, output)
    store.close()

    (store, listenedVariablesIndices, output, inv)
  }

  test("Sum: add new listened variable") {
    val (_, lvi, output, _) = testSumConst(Set(2, 3, 4))
    lvi :+= 1

    output.value() should be(10)
  }

  test("Sum: remove listened variable") {
    val (_, lvi, output, _) = testSumConst(Set(2, 3, 4))
    lvi :-= 4

    output.value() should be(5)
  }

  test("Sum: checkInternals doesn't fail") {
    val (store, lvi, _, inv) = testSumConst(Set(2, 4))
    lvi :+= 5
    lvi :+= 1
    lvi :-= 2
    store.propagate()

    noException should be thrownBy inv.checkInternals()
  }

}
