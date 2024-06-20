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
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.lib.invariant.logic.DenseRef

class DenseRefTestSuite extends AnyFunSuite with Matchers {

  private def testDenseRef(): (Store, Array[SetVariable], Array[SetVariable], DenseRef) = {
    val store                   = new Store(debugLevel = 3)
    val values: Array[Set[Int]] = Array(Set(0, 1, 2, 3), Set(2, 3), Set(0, 3), Set(1, 2, 3), Set(4))
    val input: Array[SetVariable] = values.map(s => SetVariable(store, s))
    val inv: DenseRef             = DenseRef.makeDenseRef(store, input, 10)
    store.close()

    (store, input, inv.output, inv)
  }

  test("DenseRef: initialization works as expected") {
    val (_, _, output, _) = testDenseRef()

    output(0).value() should contain only (0, 2)
    output(1).value() should contain only (0, 3)
    output(2).value() should contain only (0, 1, 3)
    output(3).value() should contain only (0, 1, 2, 3)
    output(4).value() should contain only 4
    for (i <- 5 until 10) {
      withClue(s"For ref $i") {
        output(i).value() shouldBe empty
      }
    }
  }

  test("DenseRef: adding a new value") {
    val (_, input, output, _) = testDenseRef()
    input(4) :+= 5

    output(0).value() should contain only (0, 2)
    output(1).value() should contain only (0, 3)
    output(2).value() should contain only (0, 1, 3)
    output(3).value() should contain only (0, 1, 2, 3)
    output(4).value() should contain only 4
    output(5).value() should contain only 4
    for (i <- 6 until 10) {
      withClue(s"For ref $i") {
        output(i).value() shouldBe empty
      }
    }
  }

  test("DenseRef: remove a value from an input set") {
    val (_, input, output, _) = testDenseRef()
    input(0) :-= 3

    output(0).value() should contain only (0, 2)
    output(1).value() should contain only (0, 3)
    output(2).value() should contain only (0, 1, 3)
    output(3).value() should contain only (1, 2, 3)
    output(4).value() should contain only 4
  }

  test("DenseRef: move a value from an input set to another") {
    val (_, input, output, _) = testDenseRef()
    input(0) :-= 3
    input(4) :+= 3

    output(0).value() should contain only (0, 2)
    output(1).value() should contain only (0, 3)
    output(2).value() should contain only (0, 1, 3)
    output(3).value() should contain only (1, 2, 3, 4)
    output(4).value() should contain only 4
  }

  test("DenseRef: checkInternals doesn't fail") {
    val (store, input, _, inv) = testDenseRef()
    input(0) :-= 3
    input(4) :+= 3
    input(3) :+= 7
    input(1) :-= 2
    store.propagate()

    noException should be thrownBy inv.checkInternals()
  }

  test("DenseRef: checkInternal should fail") {
    val (store, _, output, inv) = testDenseRef()
    store.propagate()

    output(0) :+= 4
    an[IllegalArgumentException] should be thrownBy inv.checkInternals()
  }

}
