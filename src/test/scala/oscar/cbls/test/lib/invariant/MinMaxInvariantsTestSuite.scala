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

package oscar.cbls.test.lib.invariant

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.lib.invariant.minmax._

import scala.util.Random
class MinMaxInvariantsTestSuite extends AnyFunSuite {

  // Create and return objets we use to make tests
  private def testMinMax(isMin: Boolean, set: Option[Set[Int]] = None) = {
    val store                     = new Store()
    val input: Array[IntVariable] = (for (i: Long <- 0L to 5L) yield IntVariable(store, i)).toArray
    val output: IntVariable       = IntVariable(store, 0L)
    val cond: SetVariable = set match {
      case None =>
        SetVariable(store, (for (i: Int <- input.indices) yield i).toSet)
      case Some(s) =>
        SetVariable(store, s)
    }
    val inv: Extremum =
      if (isMin) Min(store, input, cond, output)
      else Max(store, input, cond, output)
    store.close()

    (input, cond, output, inv)
  }

  test("Min invariant initialisation works as expected.") {
    val (_, _, output, _) = testMinMax(isMin = true)
    output.value() should be(0L)
  }

  test("Max invariant initialisation works as expected.") {
    val (_, _, output, _) = testMinMax(isMin = false)
    output.value() should be(5L)
  }

  test("Min that listen to an empty set of variables") {
    val (_, _, output, _) = testMinMax(isMin = true, Some(Set.empty))
    output.value() should be(Long.MaxValue)
  }

  test("Max that listen to an empty set of variables") {
    val (_, _, output, _) = testMinMax(isMin = false, Some(Set.empty))
    output.value() should be(Long.MinValue)
  }

  test("Min: a variable is changed but the min doesn't change") {
    val (input, _, output, _) = testMinMax(isMin = true)
    input(2) := 10
    output.value() should be(0)
  }

  test("Min: the min is changed and is no more the smaller value") {
    val (input, _, output, _) = testMinMax(isMin = true)
    input(0) := 11
    output.value() should be(1)
  }

  test("Min: another value becomes the min") {
    val (input, _, output, _) = testMinMax(isMin = true)
    input(4) := -2
    output.value() should be(-2)
  }

  test("Min: the min become smaller") {
    val (input, _, output, _) = testMinMax(isMin = true)
    input(0) :-= 4
    output.value() should be(-4)
  }

  test("Max: a variable is changed but the max doesn't change") {
    val (input, _, output, _) = testMinMax(isMin = false)
    input(3) := 2
    output.value() should be(5)
  }

  test("Max: the max is changed and is no more the bigger value") {
    val (input, _, output, _) = testMinMax(isMin = false)
    input(5) := -1
    output.value() should be(4)
  }

  test("Max: another variable becomes the max") {
    val (input, _, output, _) = testMinMax(isMin = false)
    input(0) := 10
    output.value() should be(10)
  }

  test("Max: the max become bigger") {
    val (input, _, output, _) = testMinMax(isMin = false)
    input(5) :*= 3
    output.value() should be(15)
  }

  test("Min: adding a variable that doesn't change the min") {
    val (_, cond, output, _) = testMinMax(isMin = true, Some(Set(1, 3, 5)))
    cond :+= 4
    output.value() should be(1)
  }

  test("Min: removing the min") {
    val (_, cond, output, _) = testMinMax(isMin = true, Some(Set(1, 3, 5)))
    cond :-= 1
    output.value() should be(3)
  }

  test("Min: adding a smaller value") {
    val (_, cond, output, _) = testMinMax(isMin = true, Some(Set(1, 3, 5)))
    cond :+= 0
    output.value() should be(0)
  }

  test("Min: removing a value other than the min") {
    val (_, cond, output, _) = testMinMax(isMin = true, Some(Set(1, 3, 5)))
    cond :-= 3
    output.value() should be(1)
  }

  test("Min: removing all value") {
    val (_, cond, output, _) = testMinMax(isMin = true, Some(Set(1, 3, 5)))
    cond :-= 1
    cond :-= 3
    cond :-= 5
    output.value() should be(Long.MaxValue)
  }

  test("Max: adding a variable that doesn't change the max") {
    val (_, cond, output, _) = testMinMax(isMin = false, Some(Set(0, 2, 4)))
    cond :+= 1
    output.value() should be(4)
  }

  test("Max: removing the max") {
    val (_, cond, output, _) = testMinMax(isMin = false, Some(Set(0, 2, 4)))
    cond :-= 4
    output.value() should be(2)
  }

  test("Max: adding a bigger value") {
    val (_, cond, output, _) = testMinMax(isMin = false, Some(Set(0, 2, 4)))
    cond :+= 5
    output.value() should be(5)
  }

  test("Max: removing a value other than the max") {
    val (_, cond, output, _) = testMinMax(isMin = false, Some(Set(0, 2, 4)))
    cond :-= 2
    output.value() should be(4)
  }

  test("Max: removing all the value") {
    val (_, cond, output, _) = testMinMax(isMin = false, Some(Set(0, 2, 4)))
    cond :-= 0
    cond :-= 2
    cond :-= 4

    output.value() should be(Long.MinValue)
  }

}
