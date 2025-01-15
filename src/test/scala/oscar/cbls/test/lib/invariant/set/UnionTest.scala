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
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.lib.invariant.set.Union
import oscar.cbls.test.invBench.{InvTestBench, TestBenchSut}

class UnionTest extends AnyFunSuite {

  test("Union invariant is correct ") {
    def createUnion(model: Store): TestBenchSut = {
      val A: SetVariable = SetVariable(model, Set.empty, name = Some("A"))
      val B: SetVariable = SetVariable(model, Set.empty, name = Some("B"))

      val output: SetVariable = SetVariable(model, Set.empty, name = Some("Union"))
      val inter: Union        = Union(model, A, B, output)

      TestBenchSut(inter, Array(A, B), Array(output))
    }

    val bench: InvTestBench = InvTestBench(createUnion, "Test union invariant")
    bench.test()
  }
}
