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

package oscar.cbls.test.lib.invariant.routing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.{Store, Variable}
import oscar.cbls.lib.invariant.routing.NbNodes
import oscar.cbls.modeling.routing.VRS
import oscar.cbls.test.invBench.{InvTestBench, TestBenchSut}

class NbNodesTests extends AnyFunSuite with Matchers {

  test("NbNodes: initialization works as expected") {
    val model = new Store(debugLevel = 3)
    val vrs   = VRS(model, 30, 2)
    val inv   = NbNodes(vrs)
    model.close()

    inv(0).value() must be(1)
    inv(1).value() must be(1)
  }

  test("NbNodes: Assign works and no precomputation are available") {
    val model = new Store(debugLevel = 3)
    val vrs   = VRS(model, 30, 2)
    val inv   = NbNodes(vrs)
    model.close()
    vrs.routes := IntSequence(List.from(0 to 20 by 2) ::: List.from(1 to 20 by 2))
    model.propagate()

    inv(0).value() must be(11)
    inv(1).value() must be(10)

    inv.precomputedValues must contain only 0L
  }

  test("NbNodes: precomputation are performed when defining level 0 checkpoint") {
    val model = new Store(debugLevel = 3)
    val vrs   = VRS(model, 30, 2)
    val inv   = NbNodes(vrs)
    model.close()
    vrs.routes := IntSequence(List.from(0 to 20 by 2) ::: List.from(1 to 20 by 2))
    vrs.routes.defineCurrentValueAsCheckpoint()
    model.propagate()

    inv.precomputedValues must equal(
      Array(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 0, 0, 0, 0, 0, 0, 0,
        0, 0)
    )
  }

  test("NbNodes: test bench") {
    val n = 25
    val v = 5

    def createInv(model: Store): TestBenchSut = {
      val vrs                     = VRS(model, n, v)
      val inv                     = NbNodes(vrs)
      val output: Array[Variable] = Array.from(inv())
      TestBenchSut(inv, Array(vrs.routes), output, Some(vrs))
    }

    val bench = InvTestBench(createInv, "NbNodes test bench")
    bench.test()
  }
}
