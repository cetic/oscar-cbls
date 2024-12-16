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

package oscar.cbls.test.invBench

import org.scalatest.funsuite.AnyFunSuite
import oscar.cbls.core.computation.seq.{SeqNotificationTarget, SeqUpdate, SeqVariable}
import oscar.cbls.core.computation.{Invariant, Store}
import oscar.cbls.modeling.routing.VRP

class RoutingTestBench extends AnyFunSuite {

  /** Invariant which does nothing. Used to use the test bench. */
  private class DummyRoutingInvariant(model: Store, input: SeqVariable)
      extends Invariant(model, None)
      with SeqNotificationTarget {

    input.registerStaticallyAndDynamicallyListeningElement(this)
    override def notifySeqChanges(
      v: SeqVariable,
      contextualVarIndex: Int,
      changes: SeqUpdate
    ): Unit = {}

    override def checkInternals(): Unit = {}
  }

  test("Routing test bench generates only valid moves") {
    def createVRP(model: Store): TestBenchSut = {
      val vrp: VRP = VRP(model, 100, 10, debug = true)
      val inv      = new DummyRoutingInvariant(model, vrp.routes)

      TestBenchSut(inv, Array(vrp.routes), Array.empty, Some(vrp))
    }

    val bench = InvTestBench(createVRP, "Routing test bench")
    bench.test()
  }
}
