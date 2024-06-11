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
import oscar.cbls.lib.invariant.numeric.Prod

class ProdTestSuite extends AnyFunSuite with Matchers {

  private def testProd(
    set: Set[Int]
  ): (Store, Array[IntVariable], SetVariable, IntVariable, Prod) = {
    val store: Store              = new Store(debugLevel = 3)
    val input: Array[IntVariable] = Array.range(0, 7).map(i => IntVariable(store, i % 6))
    val listenedVariablesIndices: SetVariable = SetVariable(store, set)
    val output                                = IntVariable(store, 0)
    val inv                                   = Prod(store, input, listenedVariablesIndices, output)
    store.close()

    (store, input, listenedVariablesIndices, output, inv)
  }

  test("Prod: initialization without null") {
    val (_, _, _, output, _) = testProd(Set.range(1, 6))

    output.value() should be(120)
  }

  test("Prod: initialization with null") {
    val (_, _, _, output, _) = testProd(Set.range(0, 6))

    output.value() should be(0)
  }

  test("Prod: changing non null listened value to another non null value") {
    val (_, input, _, output, _) = testProd(Set(2, 3, 4))
    input(4) :+= 3

    output.value() should be(42)
  }

  test("Prod: changing a non null value to null") {
    val (_, input, _, output, _) = testProd(Set(2, 3, 4))
    input(4) := 0

    output.value() should be(0)
  }

  test("Prod changing a null to a non null value") {
    val (_, input, _, output, _) = testProd(Set(0, 2, 3, 4))
    input(0) := 10

    output.value() should be(240)
  }

  test("Prod: adding a non null value to a non null product") {
    val (_, _, lvi, output, _) = testProd(Set(2, 3))
    lvi :+= 5

    output.value() should be(30)
  }

  test("Prod: adding a value to a null product") {
    val (_, _, lvi, output, _) = testProd(Set(0, 2, 3))
    lvi :+= 5

    output.value() should be(0)
  }

  test("Prod: adding a zero to a non null product") {
    val (_, _, lvi, output, _) = testProd(Set(2, 3))
    lvi :+= 0

    output.value() should be(0)
  }

  test("Prod: removing a non null value from a non null product") {
    val (_, _, lvi, output, _) = testProd(Set(2, 3, 4))
    lvi :-= 3

    output.value() should be(8)
  }

  test("Prod: removing a non null value from a null product") {
    val (_, _, lvi, output, _) = testProd(Set(0, 2, 3))
    lvi :-= 2

    output.value() should be(0)
  }

  test("Prod: removing the only 0 factor from a null product") {
    val (_, _, lvi, output, _) = testProd(Set(0, 2, 3))
    lvi :-= 0

    output.value() should be(6)
  }

  test("Prod: removing one of the null factors from a null product") {
    val (_, _, lvi, output, _) = testProd(Set.range(0, 7))
    lvi :-= 0

    output.value() should be(0)
  }

  test("Prod: checkInternal doesn't fail") {
    val (store, input, lvi, _, inv) = testProd(Set.range(2, 5))
    lvi :+= 5
    lvi :+= 6
    input(2) := 10
    input(6) := -7
    lvi :-= 3
    store.propagate()

    inv.checkInternals()
  }
}
