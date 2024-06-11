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
import oscar.cbls.lib.invariant.minmax._

class MinMaxTestSuite extends AnyFunSuite with Matchers {

  // Create and return objets we use to make tests
  private def testMinMax(
    isMin: Boolean,
    set: Set[Int]
  ): (Store, Array[IntVariable], SetVariable, IntVariable, Extremum) = {
    val store                                 = new Store(debugLevel = 3)
    val input: Array[IntVariable]             = Array.range(0, 6).map(i => IntVariable(store, i))
    val listenedVariablesIndices: SetVariable = SetVariable(store, set)
    val output: IntVariable                   = IntVariable(store, 42)
    val inv: Extremum =
      if (isMin) Min(store, input, listenedVariablesIndices, output)
      else Max(store, input, listenedVariablesIndices, output)
    store.close()

    (store, input, listenedVariablesIndices, output, inv)
  }

  test("Min invariant initialisation works as expected.") {
    val (_, _, _, output, _) = testMinMax(isMin = true, Set(0, 1, 2, 3, 4, 5))
    output.value() should be(0)
  }

  test("Max invariant initialisation works as expected.") {
    val (_, _, _, output, _) = testMinMax(isMin = false, Set(0, 1, 2, 3, 4, 5))
    output.value() should be(5)
  }

  test("Min that listen to an empty set of variables") {
    val (_, _, _, output, _) = testMinMax(isMin = true, Set.empty)
    output.value() should be(Long.MaxValue)
  }

  test("Max that listen to an empty set of variables") {
    val (_, _, _, output, _) = testMinMax(isMin = false, Set.empty)
    output.value() should be(Long.MinValue)
  }

  test("Min: a variable is changed but the min doesn't change") {
    val (_, input, _, output, _) = testMinMax(isMin = true, Set(0, 1, 2, 3, 4, 5))
    input(2) := 10
    output.value() should be(0)
  }

  test("Min: the min is changed and is no more the smaller value") {
    val (_, input, _, output, _) = testMinMax(isMin = true, Set(0, 1, 2, 3, 4, 5))
    input(0) := 11
    output.value() should be(1)
  }

  test("Min: another value becomes the min") {
    val (_, input, _, output, _) = testMinMax(isMin = true, Set(0, 1, 2, 3, 4, 5))
    input(4) := -2
    output.value() should be(-2)
  }

  test("Min: the min become smaller") {
    val (_, input, _, output, _) = testMinMax(isMin = true, Set(0, 1, 2, 3, 4, 5))
    input(0) :-= 4
    output.value() should be(-4)
  }

  test("Max: a variable is changed but the max doesn't change") {
    val (_, input, _, output, _) = testMinMax(isMin = false, Set(0, 1, 2, 3, 4, 5))
    input(3) := 2
    output.value() should be(5)
  }

  test("Max: the max is changed and is no more the bigger value") {
    val (_, input, _, output, _) = testMinMax(isMin = false, Set(0, 1, 2, 3, 4, 5))
    input(5) := -1
    output.value() should be(4)
  }

  test("Max: another variable becomes the max") {
    val (_, input, _, output, _) = testMinMax(isMin = false, Set(0, 1, 2, 3, 4, 5))
    input(0) := 10
    output.value() should be(10)
  }

  test("Max: the max become bigger") {
    val (_, input, _, output, _) = testMinMax(isMin = false, Set(0, 1, 2, 3, 4, 5))
    input(5) :*= 3
    output.value() should be(15)
  }

  test("Min: adding a variable that doesn't change the min") {
    val (_, _, listenedVariablesIndices, output, _) = testMinMax(isMin = true, Set(1, 3, 5))
    listenedVariablesIndices :+= 4
    output.value() should be(1)
  }

  test("Min: removing the min") {
    val (_, _, listenedVariablesIndices, output, _) = testMinMax(isMin = true, Set(1, 3, 5))
    listenedVariablesIndices :-= 1
    output.value() should be(3)
  }

  test("Min: adding a smaller value") {
    val (_, _, listenedVariablesIndices, output, _) = testMinMax(isMin = true, Set(1, 3, 5))
    listenedVariablesIndices :+= 0
    output.value() should be(0)
  }

  test("Min: removing a value other than the min") {
    val (_, _, listenedVariablesIndices, output, _) = testMinMax(isMin = true, Set(1, 3, 5))
    listenedVariablesIndices :-= 3
    output.value() should be(1)
  }

  test("Min: removing all value") {
    val (_, _, listenedVariablesIndices, output, _) = testMinMax(isMin = true, Set(1, 3, 5))
    listenedVariablesIndices :-= 1
    listenedVariablesIndices :-= 3
    listenedVariablesIndices :-= 5
    output.value() should be(Long.MaxValue)
  }

  test("Max: adding a variable that doesn't change the max") {
    val (_, _, listenedVariablesIndices, output, _) = testMinMax(isMin = false, Set(0, 2, 4))
    listenedVariablesIndices :+= 1
    output.value() should be(4)
  }

  test("Max: removing the max") {
    val (_, _, listenedVariablesIndices, output, _) = testMinMax(isMin = false, Set(0, 2, 4))
    listenedVariablesIndices :-= 4
    output.value() should be(2)
  }

  test("Max: adding a bigger value") {
    val (_, _, listenedVariablesIndices, output, _) = testMinMax(isMin = false, Set(0, 2, 4))
    listenedVariablesIndices :+= 5
    output.value() should be(5)
  }

  test("Max: removing a value other than the max") {
    val (_, _, listenedVariablesIndices, output, _) = testMinMax(isMin = false, Set(0, 2, 4))
    listenedVariablesIndices :-= 2
    output.value() should be(4)
  }

  test("Max: removing all the value") {
    val (_, _, listenedVariablesIndices, output, _) = testMinMax(isMin = false, Set(0, 2, 4))
    listenedVariablesIndices :-= 0
    listenedVariablesIndices :-= 2
    listenedVariablesIndices :-= 4
    output.value() should be(Long.MinValue)
  }

  test("Min: checkInternals doesn't fail") {
    val (store, input, listenedVariablesIndices, _, minInv) = testMinMax(isMin = true, Set(2, 3))
    listenedVariablesIndices :+= 1
    listenedVariablesIndices :+= 4
    input(2) := -5
    input(3) := -1
    listenedVariablesIndices :-= 2
    store.propagate()

    noException should be thrownBy minInv.checkInternals()
  }

  test("Min: checkInternals should fail") {
    val (store, _, _, output, minInv) = testMinMax(isMin = true, Set(0, 1, 2, 3, 4, 5))
    output := 42
    store.propagate()

    an[IllegalArgumentException] should be thrownBy minInv.checkInternals()
  }

  test("Max: checkInternals doesn't fail") {
    val (store, input, listenedVariablesIndices, _, maxInv) = testMinMax(isMin = false, Set(2, 3))
    listenedVariablesIndices :+= 1
    listenedVariablesIndices :+= 4
    input(2) := 15
    input(3) := 12
    listenedVariablesIndices :-= 2
    store.propagate()

    noException should be thrownBy maxInv.checkInternals()
  }

  test("Max: checkInternals should fail") {
    val (store, _, _, output, maxInv) = testMinMax(isMin = false, Set(0, 1, 2, 3, 4, 5))
    output := 42
    store.propagate()

    an[IllegalArgumentException] should be thrownBy maxInv.checkInternals()
  }

  test("Min: checkInternals doesn't fail with empty listenedVariablesIndices") {
    val (_, _, _, _, minInv) = testMinMax(isMin = true, Set.empty)
    noException should be thrownBy minInv.checkInternals()
  }

  test("Max: checkInternals doesn't fail with empty listenedVariablesIndices") {
    val (_, _, _, _, maxInv) = testMinMax(isMin = false, Set.empty)
    noException should be thrownBy maxInv.checkInternals()
  }
}
