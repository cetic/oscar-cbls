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

import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.algo.heap.BinaryHeapWithMoveIntItem
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.{IntConstant, IntVariable}
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.lib.invariant.minmax.{ExtremumConst, MaxConst, MinConst}

import scala.collection.mutable

class MinMaxConstTestSuite extends Suites(new MinMaxConstBacklogTests, new MinMaxConstTests)

/** Suite to test if the backlog mechanism works as expected */
class MinMaxConstBacklogTests extends AnyFunSuite with Matchers {
  // MinConst invariant with public access to private variables
  private class TestMinMaxConst(
    model: Store,
    input: Array[IntConstant],
    listenedValuesIndices: SetVariable,
    output: IntVariable
  ) extends MinConst(model, input, listenedValuesIndices, output, 3) {
    override def currentBacklogStates()
      : (BinaryHeapWithMoveIntItem, mutable.Queue[Int], Array[Boolean], Array[Boolean]) =
      super.currentBacklogStates()
  }

  private def testForMinMaxConstBacklogFields(): (Store, SetVariable, TestMinMaxConst) = {
    val store                     = new Store(debugLevel = 3)
    val a: Array[Long]            = Array(0, 1, 2, 3, 2, 5)
    val input: Array[IntConstant] = for (x <- a) yield new IntConstant(store, x)
    val listenedValuesIndices: SetVariable         = SetVariable(store, Set(2, 3))
    val output: IntVariable       = IntVariable(store, 42)
    val inv                       = new TestMinMaxConst(store, input, listenedValuesIndices, output)
    store.close()

    (store, listenedValuesIndices, inv)
  }

  test("Empty backlog at initialization") {
    val (store, _, inv) = testForMinMaxConstBacklogFields()
    store.propagate()
    val (h, backlog, isBacklogged, consideredValue) = inv.currentBacklogStates()
    backlog shouldBe empty
    isBacklogged should equal(Array.fill(6)(false))
    consideredValue should equal(Array(false, false, true, true, false, false))
    h should contain only (2, 3)
  }

  test("Insert non impacting values") {
    val (store, listenedValuesIndices, inv) = testForMinMaxConstBacklogFields()
    listenedValuesIndices :+= 5
    listenedValuesIndices :+= 4
    store.propagate()
    val (h, backlog, isBacklogged, consideredValue) = inv.currentBacklogStates()

    backlog should contain(5)
    backlog should contain(4)
    isBacklogged should equal(Array(false, false, false, false, true, true))
    consideredValue should equal(Array(false, false, true, true, false, false))
    h should not contain 5
    h should not contain 4
  }

  test("Insert and remove a non impacting value") {
    val (store, listenedValuesIndices, inv) = testForMinMaxConstBacklogFields()
    listenedValuesIndices :+= 5
    store.propagate()
    listenedValuesIndices :-= 5
    store.propagate()
    val (h, backlog, isBacklogged, consideredValue) = inv.currentBacklogStates()

    backlog should contain(5)
    isBacklogged(5) should be(false)
    consideredValue(5) should be(false)
    h should not contain 5
  }

  test("Insert impacting value") {
    val (store, listenedValuesIndices, inv) = testForMinMaxConstBacklogFields()
    listenedValuesIndices :+= 0
    store.propagate()
    val (h, backlog, isBacklogged, consideredValue) = inv.currentBacklogStates()

    backlog shouldBe empty
    backlog shouldBe empty
    isBacklogged should equal(Array.fill(6)(false))
    consideredValue should equal(Array(true, false, true, true, false, false))
    h should contain(0)
  }

  test("Removing considered non impacting value") {
    val (store, listenedValuesIndices, inv) = testForMinMaxConstBacklogFields()
    listenedValuesIndices :-= 3
    store.propagate()
    val (_, backlog, isBacklogged, consideredValue) = inv.currentBacklogStates()
    backlog should contain(3)
    isBacklogged(3) should be(true)
    consideredValue(3) should be(true)
  }

  test("Removing and re-adding a considered non impacting value") {
    val (store, listenedValuesIndices, inv) = testForMinMaxConstBacklogFields()
    listenedValuesIndices :-= 3
    store.propagate()
    listenedValuesIndices :+= 3
    store.propagate()
    val (_, backlog, isBacklogged, consideredValue) = inv.currentBacklogStates()
    backlog should contain(3)
    isBacklogged(3) should be(false)
    consideredValue(3) should be(true)
  }

  test("Fill the backlog") {
    val (store, listenedValuesIndices, inv) = testForMinMaxConstBacklogFields()
    listenedValuesIndices :+= 0
    listenedValuesIndices :+= 1
    listenedValuesIndices :-= 3
    listenedValuesIndices :-= 2
    store.propagate()
    listenedValuesIndices :+= 3
    listenedValuesIndices :+= 2
    listenedValuesIndices :+= 5 // After that the backlog should be [1, 3, 2, 5]
    listenedValuesIndices :+= 4 // The backlog should be trimmed before 4 is put in it
    store.propagate()
    val (h, backlog, isBacklogged, consideredValue) = inv.currentBacklogStates()

    backlog should contain only (5, 4)
    isBacklogged should equal(Array(false, false, false, false, true, true))
    consideredValue should equal(Array(true, true, true, true, false, false))
    h should contain only (0, 1, 2, 3)
  }

  test("Removing the min") {
    val (store, listenedValuesIndices, inv) = testForMinMaxConstBacklogFields()
    listenedValuesIndices :+= 0
    listenedValuesIndices :+= 1
    listenedValuesIndices :-= 3
    listenedValuesIndices :-= 2
    store.propagate()
    listenedValuesIndices :+= 3
    listenedValuesIndices :+= 2
    listenedValuesIndices :+= 5
    listenedValuesIndices :-= 0 // The min is removed
    store.propagate()
    val (h, backlog, isBacklogged, consideredValue) = inv.currentBacklogStates()

    backlog shouldBe empty
    isBacklogged should equal(Array.fill(6)(false))
    consideredValue should equal(Array(false, true, true, true, false, true))
    h should contain only (1, 2, 3, 5)
  }
}

/** Suite to test if the output of [[MinConst]] and [[MaxConst]] is correct */
class MinMaxConstTests extends AnyFunSuite with Matchers {

  // Create and return objets we use to make tests
  private def testMinMaxConst(
    isMin: Boolean,
    set: Set[Int]
  ): (Store, SetVariable, IntVariable, ExtremumConst) = {
    val store                     = new Store(debugLevel = 3)
    val input: Array[IntConstant] = Array.range(0, 6).map(i => new IntConstant(store, i))
    val output: IntVariable       = IntVariable(store, 42)
    val listenedValuesIndices: SetVariable         = SetVariable(store, set)
    val inv: ExtremumConst =
      if (isMin) MinConst(store, input, listenedValuesIndices, output)
      else MaxConst(store, input, listenedValuesIndices, output)
    store.close()

    (store, listenedValuesIndices, output, inv)
  }

  test("MinConst invariant initialisation works as expected.") {
    val (_, _, output, _) = testMinMaxConst(isMin = true, Set(0, 1, 2, 3, 4, 5))
    output.value() should be(0)
  }

  test("MaxConst invariant initialisation works as expected.") {
    val (_, _, output, _) = testMinMaxConst(isMin = false, Set(0, 1, 2, 3, 4, 5))
    output.value() should be(5)
  }

  test("MinConst that listen to an empty set of variables") {
    val (_, _, output, _) = testMinMaxConst(isMin = true, Set.empty)
    output.value() should be(Long.MaxValue)
  }

  test("MaxConst that listen to an empty set of variables") {
    val (_, _, output, _) = testMinMaxConst(isMin = false, Set.empty)
    output.value() should be(Long.MinValue)
  }

  test("MinConst: adding a variable that doesn't change the min") {
    val (_, listenedValuesIndices, output, _) = testMinMaxConst(isMin = true, Set(1, 3, 5))
    listenedValuesIndices :+= 4
    output.value() should be(1)
  }

  test("MinConst: removing the min") {
    val (_, listenedValuesIndices, output, _) = testMinMaxConst(isMin = true, Set(1, 3, 5))
    listenedValuesIndices :-= 1
    output.value() should be(3)
  }

  test("MinConst: adding a smaller value") {
    val (_, listenedValuesIndices, output, _) = testMinMaxConst(isMin = true, Set(1, 3, 5))
    listenedValuesIndices :+= 0
    output.value() should be(0)
  }

  test("MinConst: removing a value other than the min") {
    val (_, listenedValuesIndices, output, _) = testMinMaxConst(isMin = true, Set(1, 3, 5))
    listenedValuesIndices :-= 3
    output.value() should be(1)
  }

  test("MinConst: removing all value") {
    val (_, listenedValuesIndices, output, _) = testMinMaxConst(isMin = true, Set(1, 3, 5))
    listenedValuesIndices :-= 1
    listenedValuesIndices :-= 3
    listenedValuesIndices :-= 5
    output.value() should be(Long.MaxValue)
  }

  test("MaxConst: adding a variable that doesn't change the max") {
    val (_, listenedValuesIndices, output, _) = testMinMaxConst(isMin = false, Set(0, 2, 4))
    listenedValuesIndices :+= 1
    output.value() should be(4)
  }

  test("MaxConst: removing the max") {
    val (_, listenedValuesIndices, output, _) = testMinMaxConst(isMin = false, Set(0, 2, 4))
    listenedValuesIndices :-= 4
    output.value() should be(2)
  }

  test("MaxConst: adding a bigger value") {
    val (_, listenedValuesIndices, output, _) = testMinMaxConst(isMin = false, Set(0, 2, 4))
    listenedValuesIndices :+= 5
    output.value() should be(5)
  }

  test("MaxConst: removing a value other than the max") {
    val (_, listenedValuesIndices, output, _) = testMinMaxConst(isMin = false, Set(0, 2, 4))
    listenedValuesIndices :-= 2
    output.value() should be(4)
  }

  test("MaxConst: removing all the value") {
    val (_, listenedValuesIndices, output, _) = testMinMaxConst(isMin = false, Set(0, 2, 4))
    listenedValuesIndices :-= 0
    listenedValuesIndices :-= 2
    listenedValuesIndices :-= 4
    output.value() should be(Long.MinValue)
  }

  test("MinConst: checkInternals doesn't fail") {
    val (store, listenedValuesIndices, _, minInv) = testMinMaxConst(isMin = true, Set(2, 3))
    listenedValuesIndices :+= 1
    listenedValuesIndices :+= 4
    listenedValuesIndices :-= 2
    store.propagate()

    noException should be thrownBy minInv.checkInternals()
  }

  test("MinConst: checkInternals should fail") {
    val (store, _, output, minInv) = testMinMaxConst(isMin = true, Set(0, 1, 2, 3, 4, 5))
    output := 42
    store.propagate()

    an[IllegalArgumentException] should be thrownBy minInv.checkInternals()
  }

  test("MaxConst: checkInternals doesn't fail") {
    val (store, listenedValuesIndices, _, maxInv) = testMinMaxConst(isMin = false, Set(2, 3))
    listenedValuesIndices :+= 1
    listenedValuesIndices :+= 4
    listenedValuesIndices :-= 2
    store.propagate()

    noException should be thrownBy maxInv.checkInternals()
  }

  test("MaxConst: checkInternals should fail") {
    val (store, _, output, maxInv) = testMinMaxConst(isMin = false, Set(0, 1, 2, 3, 4, 5))
    output := 42
    store.propagate()

    an[IllegalArgumentException] should be thrownBy maxInv.checkInternals()
  }

  test("MinConst: checkInternals doesn't fail with empty listenedValuesIndices") {
    val (_, _, _, minInv) = testMinMaxConst(isMin = true, Set.empty)
    noException should be thrownBy minInv.checkInternals()
  }

  test("MaxConst: checkInternals doesn't fail with empty listenedValuesIndices") {
    val (_, _, _, maxInv) = testMinMaxConst(isMin = false, Set.empty)
    noException should be thrownBy maxInv.checkInternals()
  }
}
