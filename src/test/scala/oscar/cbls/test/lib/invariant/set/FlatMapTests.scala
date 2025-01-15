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

package oscar.cbls.test.lib.invariant.set

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.lib.invariant.set.FlatMap
import oscar.cbls.test.invBench.{InvTestBenchWithConstGen, TestBenchSut}

class FlatMapTests extends AnyFunSuite with Matchers {

  test("FlatMap: checkInternals should fail if the output is set to wrong value") {
    val model: Store         = new Store(debugLevel = 3)
    val input: SetVariable   = SetVariable(model, Set.from(0 to 5))
    val fun: Int => Set[Int] = (x: Int) => Set.from(5 * x - 3 to 5 * x + 3)
    val output: SetVariable  = SetVariable(model, Set.empty)
    val inv: FlatMap         = FlatMap(model, input, fun, output)
    model.close()

    output :+= 50
    output :-= 0
    an[IllegalArgumentException] must be thrownBy inv.checkInternals()
  }

  test("FlatMap: test bench") {
    val minValue: Long = -100
    val maxValue: Long = 100

    class FlatMapBench(additionalSeeds: List[String] = List())
        extends InvTestBenchWithConstGen[Array[Set[Int]]]("FlatMap test bench", additionalSeeds) {
      override def genConst(): Gen[Array[Set[Int]]] = {
        val setGen: Gen[Set[Int]] = for (l <- Gen.listOf(arbitrary[Int])) yield Set.from(l)
        for (l <- Gen.listOfN(maxValue.toInt - minValue.toInt + 1, setGen)) yield l.toArray
      }

      override def createTestBenchSut(model: Store, inputData: Array[Set[Int]]): TestBenchSut = {
        val input: SetVariable = SetVariable(model, Set.empty)
        input.setDomain(minValue, maxValue)
        val fun: Int => Set[Int] = (x: Int) => inputData(x - minValue.toInt)
        val output: SetVariable  = SetVariable(model, Set.empty)
        val inv: FlatMap         = FlatMap(model, input, fun, output)

        TestBenchSut(inv, Array(input), Array(output))
      }
    }

    val flatMapBench = new FlatMapBench
    flatMapBench.test()
  }
}
