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
import oscar.cbls.lib.invariant.routing.NbNodesLogReduced
import oscar.cbls.modeling.routing.VRP
import oscar.cbls.test.invBench.{InvTestBench, TestBenchSut}

class NbNodesWithLogReductionTests extends AnyFunSuite with Matchers {

  test("NbNodesWithLogReduction: initialization works as expected") {
    val model = new Store(debugLevel = 3)
    val vrp   = VRP(model, 30, 2)
    val inv   = NbNodesLogReduced(vrp)
    model.close()

    inv(0).value() must be(1)
    inv(1).value() must be(1)
  }

  test("NbNodesWithLogReduction: Assign works") {
    val model = new Store(debugLevel = 3)
    val vrp   = VRP(model, 30, 2)
    val inv   = NbNodesLogReduced(vrp)
    model.close()
    vrp.routes := IntSequence(List.from(0 to 20 by 2) ::: List.from(1 to 20 by 2))
    model.propagate()

    inv(0).value() must be(11)
    inv(1).value() must be(10)
  }

  test("NbNodesWithLogReduction: test bench") {
    val n = 25
    val v = 5

    def createInv(model: Store): TestBenchSut = {
      val vrp                     = VRP(model, n, v)
      val inv                     = NbNodesLogReduced(vrp)
      val output: Array[Variable] = Array.from(inv())
      TestBenchSut(inv, Array(vrp.routes), output, Some(vrp))
    }

    val bench = InvTestBench(createInv, "NbNodes test bench")
    bench.test()
  }
}
