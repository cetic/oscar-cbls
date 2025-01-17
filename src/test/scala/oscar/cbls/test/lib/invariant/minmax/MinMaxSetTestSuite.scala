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

package oscar.cbls.test.lib.invariant.minmax

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.lib.invariant.minmax.{ExtremumSet, MaxSet, MinSet}

class MinMaxSetTestSuite extends AnyFunSuite with Matchers {

  private def testMinMaxSet(isMin: Boolean): (Store, SetVariable, IntVariable, ExtremumSet) = {
    val store: Store        = new Store(debugLevel = 3)
    val input: SetVariable  = SetVariable(store, Set[Int](3, 4))
    val output: IntVariable = IntVariable(store, 42)
    val inv =
      if (isMin) MinSet(store, input, output)
      else MaxSet(store, input, output)
    store.close()
    (store, input, output, inv)
  }

  test("MinSet: initialization") {
    val (_, _, output, _) = testMinMaxSet(isMin = true)
    output.value() should be(3)
  }

  test("MaxSet: initialization") {
    val (_, _, output, _) = testMinMaxSet(isMin = false)
    output.value() should be(4)
  }

  test("MinSet: insert no impacting value") {
    val (_, input, output, _) = testMinMaxSet(isMin = true)
    input :+= 10
    output.value() should be(3)
  }

  test("MaxSet: insert no impacting value") {
    val (_, input, output, _) = testMinMaxSet(isMin = false)
    input :+= 0
    output.value() should be(4)
  }

  test("MinSet: remove no impacting value") {
    val (_, input, output, _) = testMinMaxSet(isMin = true)
    input :-= 4
    output.value() should be(3)
  }

  test("MaxSet: remove no impacting value") {
    val (_, input, output, _) = testMinMaxSet(isMin = false)
    input :-= 3
    output.value() should be(4)
  }

  test("MinSet: insert new min") {
    val (_, input, output, _) = testMinMaxSet(isMin = true)
    input :+= 0
    output.value() should be(0)
  }

  test("MaxSet: insert new max") {
    val (_, input, output, _) = testMinMaxSet(isMin = false)
    input :+= 10
    output.value() should be(10)
  }

  test("MinSet: remove the min") {
    val (_, input, output, _) = testMinMaxSet(isMin = true)
    input :-= 3
    output.isScheduled should be(true)
    output.value() should be(4)
  }

  test("MaxSet: remove the max") {
    val (_, input, output, _) = testMinMaxSet(isMin = false)
    input :-= 4
    output.isScheduled should be(true)
    output.value() should be(3)
  }

  test("MinSet: checkInternals should fail") {
    val (_, _, output, minInv) = testMinMaxSet(isMin = true)
    output := 42
    an[IllegalArgumentException] should be thrownBy minInv.checkInternals()
  }

  test("MaxSet: checkInternals should fail") {
    val (_, _, output, maxInv) = testMinMaxSet(isMin = false)
    output := -42
    an[IllegalArgumentException] should be thrownBy maxInv.checkInternals()
  }

  test("MinSet: checkInternals doesn't fail") {
    val (store, input, _, inv) = testMinMaxSet(isMin = true)
    input :+= 10
    input :+= 2
    input :+= 1
    input :-= 4
    input :-= 1
    store.propagate()
    noException should be thrownBy inv.checkInternals()
  }

  test("MaxSet: checkInternals doesn't fail") {
    val (store, input, _, inv) = testMinMaxSet(isMin = false)
    input :+= 1
    input :+= 7
    input :+= 8
    input :-= 3
    input :-= 8
    store.propagate()
    noException should be thrownBy inv.checkInternals()
  }

  test("MinSet: checkInternals doesn't fail on empty set") {
    val (_, input, output, inv) = testMinMaxSet(isMin = true)
    input :-= 4
    input :-= 3

    output.value() should be(Int.MaxValue)
    noException should be thrownBy inv.checkInternals()
  }

  test("MaxSet: checkInternals doesn't fail on empty set") {
    val (_, input, output, inv) = testMinMaxSet(isMin = false)
    input :-= 4
    input :-= 3

    output.value() should be(Int.MinValue)
    noException should be thrownBy inv.checkInternals()
  }
}
