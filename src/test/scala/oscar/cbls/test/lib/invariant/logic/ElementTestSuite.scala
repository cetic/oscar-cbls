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
import oscar.cbls.lib.invariant.logic.Element

class ElementTestSuite extends AnyFunSuite with Matchers {

  private def testElement(): (Store, Array[IntVariable], IntVariable, IntVariable, Element) = {
    val store                     = new Store(debugLevel = 3)
    val values: Array[Long]       = Array(2, 3, 5, 7, 11, 13)
    val input: Array[IntVariable] = values.map(x => IntVariable(store, x))
    val index: IntVariable        = IntVariable(store, 3)
    val output: IntVariable       = IntVariable(store, 0)
    val inv: Element              = Element(store, input, index, output)
    store.close()

    (store, input, index, output, inv)
  }

  test("Element: initialization works as expected"){
    val (_, _, _, output, _) = testElement()

    output.value() should be(7)
  }

  test("Element: change the index"){
    val (_, _, index, output, inv) = testElement()
    index := 1

    output.value() should be(3)
    noException should be thrownBy inv.checkInternals()

  }

  test("Element: changing the listened variable"){
    val (_, input, _, output, inv) = testElement()
    input(3) := 42

    output.value() should be(42)
    noException should be thrownBy inv.checkInternals()
  }

  test("Element: changing a not listened variable"){
    val (_, input, _, output, _) = testElement()
    input(0) := 17

    output.value() should be(7)
  }

  test("Element: changing the listened variable and changing ots value"){
    val (_, input, index, output, _) = testElement()
    index := 0
    input(0) := 42

    output.value() should be(42)
  }

  test("Element: checkInternal should fail"){
    val (store, _, _, output, inv) = testElement()
    store.propagate()

    output := 24
    an[IllegalArgumentException] should be thrownBy inv.checkInternals()
  }


}
