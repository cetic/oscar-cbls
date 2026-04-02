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

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import oscar.cbls.lib.invariant.set.SetFilter
import oscar.cbls.modeling.Model
import oscar.cbls.util.invBench.{InvTestBenchWithConstGen, TestBenchSut}

class SetFilterTest extends AnyFunSuite {

  test("SetFilter works as expected") {

    class SetFilterTestBench extends InvTestBenchWithConstGen[Set[Int]]("Set Filter Test Bench") {
      override def genConst(): Gen[Set[Int]] = {
        for {
          n    <- Gen.choose(0, 10)
          data <- Gen.listOfN(n, Gen.choose(Int.MinValue, Int.MaxValue))
        } yield data.toSet
      }

      override def createTestBenchSut(model: Model, inputData: Set[Int]): TestBenchSut = {
        val input = model.setVar(inputData, Int.MinValue, Int.MaxValue)
        val inv   = SetFilter(model.store, input, _ % 5 == 0)
        TestBenchSut(inv, Array(input), Array(inv()))
      }

    }

    val bench = new SetFilterTestBench
    bench.test()
  }
}
