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

import org.scalatest.funsuite.AnyFunSuite
import oscar.cbls.lib.invariant.set.Cardinality
import oscar.cbls.modeling.Model
import oscar.cbls.util.invBench.{InvTestBench, TestBenchSut}

class CardinalityTest extends AnyFunSuite {

  test("Cardinality invariant is correct") {
    def createCardinality(model: Model): TestBenchSut = {
      val input  = model.setVar(Set.empty, Int.MinValue, Int.MaxValue)
      val output = model.intVar(0, 0, Int.MaxValue)
      val inv    = Cardinality(model.store, input, output)

      TestBenchSut(inv, Array(input), Array(output))
    }

    val bench = InvTestBench(createCardinality, "Test Cardinality invariant")
    bench.test()
  }

}
