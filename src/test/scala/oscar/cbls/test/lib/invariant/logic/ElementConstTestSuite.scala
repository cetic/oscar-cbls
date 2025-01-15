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

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.lib.invariant.logic.ElementConst
import oscar.cbls.test.invBench.{InvTestBenchWithConstGen, TestBenchSut}

class ElementConstTestSuite extends AnyFunSuite with Matchers {

  private def testElement(): (Store, Array[Long], IntVariable, IntVariable, ElementConst) = {
    val store               = new Store(debugLevel = 3)
    val input: Array[Long]  = Array(2, 3, 5, 7, 11, 13)
    val index: IntVariable  = IntVariable(store, 3)
    val output: IntVariable = IntVariable(store, 0)
    val inv: ElementConst   = ElementConst(store, input, index, output)
    store.close()

    (store, input, index, output, inv)
  }

  test("ElementConst: initialization works as expected") {
    val (_, _, _, output, _) = testElement()

    output.value() should be(7)
  }

  test("ElementConst: change the index") {
    val (_, _, index, output, inv) = testElement()
    index := 1

    output.value() should be(3)
    noException should be thrownBy inv.checkInternals()

  }

  test("ElementConst: checkInternal should fail") {
    val (store, _, _, output, inv) = testElement()
    store.propagate()

    output := 24
    an[IllegalArgumentException] should be thrownBy inv.checkInternals()
  }

  test("ElementConst: TestBench") {
    class ElementConstTestBench
        extends InvTestBenchWithConstGen[Array[Long]]("ElementConst Test Bench") {

      override def genConst(): Gen[Array[Long]] = {
        for {
          size  <- Gen.choose(1, 100)
          array <- Gen.listOfN(size, Arbitrary.arbitrary[Long])
        } yield array.toArray
      }

      override def createTestBenchSut(model: Store, inputData: Array[Long]): TestBenchSut = {
        val index = IntVariable(model, 0)
        index.setDomain(0, inputData.length - 1)
        val output = IntVariable(model, 0)
        val inv    = ElementConst(model, inputData, index, output)

        TestBenchSut(inv, Array(index), Array(output))
      }

      override def typeTToString(elem: Array[Long]) =
        s"(${Array.tabulate(elem.length)(i => s"$i: ${elem(i)}").mkString("; ")})"
    }

    val bench = new ElementConstTestBench
    bench.test()
  }

}
