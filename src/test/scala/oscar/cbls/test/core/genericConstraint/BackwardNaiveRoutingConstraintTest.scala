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

package oscar.cbls.test.core.genericConstraint

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.{Store, Variable}
import oscar.cbls.core.computation.genericConstraint.BackwardNaiveRoutingConstraint
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.modeling.routing.VRS
import oscar.cbls.test.invBench.{InvTestBench, TestBenchSut}

class BackwardNaiveRoutingConstraintTest extends AnyFunSuite with Matchers {

  private object NaiveBackwardNbEdges {
    def apply(vrs: VRS): NaiveBackwardNbEdges = {
      val output = Array.fill(vrs.n)(IntVariable(vrs.store, 0L))
      new NaiveBackwardNbEdges(vrs, output)
    }
  }

  /** Invariant that maintains the number of traveled edges in a route. For a vehicle node, it
    * returns the number of edges through the total route.
    */
  private class NaiveBackwardNbEdges(vrs: VRS, output: Array[IntVariable])
      extends BackwardNaiveRoutingConstraint(
        vrs,
        Int.MaxValue.toLong,
        Array.fill(vrs.v)(0L),
        (_: Int, _: Int, fromV: Long) => fromV + 1,
        Some("NaiveBackwardNbEdges")
      ) {

    output.foreach(_.setDefiningInvariant(this))

    def apply(): Array[IntVariable] = output

    def apply(i: Int): IntVariable = output(i)

    override protected def assignNodeValue(node: Int, value: Long): Unit = output(node) := value
  }

  test("BackwardRoutingConstraint: initialization works as expected") {
    val model = new Store(debugLevel = 3)
    val vrs   = VRS(model, 10, 2)
    val inv   = NaiveBackwardNbEdges(vrs)
    model.close()

    for (i <- vrs.vehicles) inv(i).value() must be(0L)

    for (i <- vrs.customers) {
      withClue(s"Failure for Node $i\n") {
        inv(i).value() must be(Int.MaxValue)
      }
    }
  }

  test("BackwardNaiveRoutingConstraint: assign works as expected") {
    val model = new Store(debugLevel = 3)
    val vrs   = VRS(model, 15, 2)
    val inv   = NaiveBackwardNbEdges(vrs)
    model.close()

    vrs.routes := IntSequence(List.from(0 until 10 by 2) ::: List.from(1 until 10 by 2))
    model.propagate()

    val outputVal = inv().map(x => x.value())
    val maxInt    = Int.MaxValue

    // We are moving backward. So the computations are done following these routes:
    //  0 -> 8 -> 6 -> 4 -> 2 -> 0 and 1 -> 9 -> 7 -> 5 -> 3 -> 1
    outputVal must equal(
      Array(5, 5, 4, 4, 3, 3, 2, 2, 1, 1, maxInt, maxInt, maxInt, maxInt, maxInt)
    )
  }

  test("BackwardNaiveRoutingConstraint: test bench") {
    val n = 25
    val v = 5

    def createInv(model: Store): TestBenchSut = {
      val vrs                     = VRS(model, n, v)
      val inv                     = NaiveBackwardNbEdges(vrs)
      val output: Array[Variable] = Array.from(inv())
      TestBenchSut(inv, Array(vrs.routes), output, Some(vrs))
    }

    val bench = InvTestBench(createInv, "BackwardNaiveRoutingConstraint: test bench")
    bench.test()
  }
}
