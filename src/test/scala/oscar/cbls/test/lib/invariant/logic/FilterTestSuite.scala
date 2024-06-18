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
import oscar.cbls.lib.invariant.logic.Filter

class FilterTestSuite extends AnyFunSuite with Matchers {

  private def testFilter(
    predicate: Long => Boolean
  ): (Store, Array[IntVariable], SetVariable, Filter) = {
    val store  = new Store(debugLevel = 3)
    val values = Array.range(0, 11)
    val input  = values.map(v => IntVariable(store, v))
    val output = SetVariable(store, Set.empty)
    val inv    = Filter(store, input, output, predicate)
    store.close()

    (store, input, output, inv)
  }

  test("Filter: initialization works as expected") {
    val (_, _, output, _) = testFilter(_ <= 2)

    output.value() should contain only (0, 1, 2)
  }

  test("Filter: a new variable respects the predicate") {
    val (_, input, output, _) = testFilter(_ % 2 == 0)
    input(1) := 42

    output.value() should contain only (0, 1, 2, 4, 6, 8, 10)
  }

  test("Filter: a variable doesn't respect the predicate anymore") {
    val (_, input, output, _) = testFilter(_ % 3 == 0)
    input(6) := 17

    output.value() should contain only (0, 3, 9)
  }

  test("Filter: changing a selected variable so that it always respects the predicate") {
    val (_, input, output, _) = testFilter(_ % 6 == 0)
    input(0) := 42

    output.value() should contain only (0, 6)
  }

  test("Filter: checkInternals doesn't fail"){
    val (store, input, _, inv) = testFilter(_ % 2 == 1)
    input(0) := 11
    input(3) := 4
    input(7) := 5
    store.propagate()

    noException should be thrownBy inv.checkInternals()
  }

  test("Filter: checkInternal should fail"){
    val (store, _, output, inv) = testFilter(_ % 2 == 0)
    output :+= 25

    an[IllegalArgumentException] should be thrownBy inv.checkInternals()
    inv.checkInternals()
  }

}
