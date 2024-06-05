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
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.{an, convertToAnyShouldWrapper}
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.{IntConstant, IntVariable}
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.lib.invariant.minmax.{ExtremumConst, MaxConst, MinConst}

class MinMaxConstTestSuite extends AnyFunSuite {
  private def testMinMaxConst(
    isMin: Boolean,
    set: Option[Set[Int]] = None
  ): (Array[IntConstant], SetVariable, IntVariable, ExtremumConst) = {
    val store = new Store(debugLevel = 3)
    val input: Array[IntConstant] =
      (for (i: Long <- 0L to 5L) yield new IntConstant(store, i)).toArray
    val output: IntVariable = IntVariable(store, 42)
    val cond: SetVariable = set match {
      case None =>
        SetVariable(store, (for (i: Int <- input.indices) yield i).toSet)
      case Some(s) =>
        SetVariable(store, s)
    }
    val inv: ExtremumConst =
      if (isMin) MinConst(store, input, cond, output)
      else MaxConst(store, input, cond, output)
    store.close()

    (input, cond, output, inv)
  }

  test("MinConst invariant initialisation works as expected.") {
    val (_, _, output, _) = testMinMaxConst(isMin = true)
    output.value() should be(0L)
  }

  test("MaxConst invariant initialisation works as expected.") {
    val (_, _, output, _) = testMinMaxConst(isMin = false)
    output.value() should be(5L)
  }

  test("MinConst that listen to an empty set of variables") {
    val (_, _, output, _) = testMinMaxConst(isMin = true, Some(Set.empty))
    output.value() should be(Long.MaxValue)
  }

  test("MaxConst that listen to an empty set of variables") {
    val (_, _, output, _) = testMinMaxConst(isMin = false, Some(Set.empty))
    output.value() should be(Long.MinValue)
  }

  test("MinConst: adding a variable that doesn't change the min") {
    val (_, cond, output, _) = testMinMaxConst(isMin = true, Some(Set(1, 3, 5)))
    cond :+= 4
    output.value() should be(1)
  }

  test("MinConst: removing the min") {
    val (input, cond, output, _) = testMinMaxConst(isMin = true, Some(Set(1, 3, 5)))
    cond :-= 1
    output.value() should be(3)
  }

  test("MinConst: adding a smaller value") {
    val (_, cond, output, _) = testMinMaxConst(isMin = true, Some(Set(1, 3, 5)))
    cond :+= 0
    output.value() should be(0)
  }

  test("MinConst: removing a value other than the min") {
    val (_, cond, output, _) = testMinMaxConst(isMin = true, Some(Set(1, 3, 5)))
    cond :-= 3
    output.value() should be(1)
  }

  test("MinConst: removing all value") {
    val (_, cond, output, _) = testMinMaxConst(isMin = true, Some(Set(1, 3, 5)))
    cond :-= 1
    cond :-= 3
    cond :-= 5
    output.value() should be(Long.MaxValue)
  }

  test("MaxConst: adding a variable that doesn't change the max") {
    val (_, cond, output, _) = testMinMaxConst(isMin = false, Some(Set(0, 2, 4)))
    cond :+= 1
    output.value() should be(4)
  }

  test("MaxConst: removing the max") {
    val (_, cond, output, _) = testMinMaxConst(isMin = false, Some(Set(0, 2, 4)))
    cond :-= 4
    output.value() should be(2)
  }

  test("MaxConst: adding a bigger value") {
    val (_, cond, output, _) = testMinMaxConst(isMin = false, Some(Set(0, 2, 4)))
    cond :+= 5
    output.value() should be(5)
  }

  test("MaxConst: removing a value other than the max") {
    val (_, cond, output, _) = testMinMaxConst(isMin = false, Some(Set(0, 2, 4)))
    cond :-= 2
    output.value() should be(4)
  }

  test("MaxConst: removing all the value") {
    val (_, cond, output, _) = testMinMaxConst(isMin = false, Some(Set(0, 2, 4)))
    cond :-= 0
    cond :-= 2
    cond :-= 4
    output.value() should be(Long.MinValue)
  }

  test("MinConst: checkInternals doesn't fail") {
    val (input, cond, _, minInv) = testMinMaxConst(isMin = true, Some(Set(2, 3)))
    cond :+= 1
    cond :+= 4
    cond :-= 2
    minInv.checkInternals()
  }

  test("MinConst: checkInternals should fail"){
    val (_, _, output, minInv) = testMinMaxConst(isMin = true)
    output := 42
    an [IllegalArgumentException] should be thrownBy minInv.checkInternals()
  }

  test("MaxConst: checkInternals doesn't fail") {
    val (_, cond, _, maxInv) = testMinMaxConst(isMin = false, Some(Set(2, 3)))
    cond :+= 1
    cond :+= 4
    cond :-= 2
    maxInv.checkInternals()
  }

  test("MaxConst: checkInternals should fail"){
    val (_, _, output, maxInv) = testMinMaxConst(isMin = false)
    output := 42
    an [IllegalArgumentException] should be thrownBy maxInv.checkInternals()
  }

  test("MinConst: checkInternals doesn't fail with empty cond") {
    val (_, _, _, minInv) = testMinMaxConst(isMin = true, Some(Set.empty))
    minInv.checkInternals()
  }

  test("MaxConst: checkInternals doesn't fail with empty cond") {
    val (_, _, _, maxInv) = testMinMaxConst(isMin = false, Some(Set.empty))
    maxInv.checkInternals()
  }
}
