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

package oscar.cbls.test.modeling.routing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import oscar.cbls.core.computation.Store
import oscar.cbls.modeling.routing.VRP
import oscar.cbls.test.invBench.{InvTestBench, TestBenchSut}

class VehicleSearchersTestSuite extends AnyFunSuite with Matchers {

  test("Searchers test bench") {
    def createInv(model: Store): TestBenchSut = {
      val vrp = VRP(model, 50, 10)
      val inv = new UpdateVehicleSearchersInvariant(model, vrp.routes, vrp.v)

      TestBenchSut(inv, Array(vrp.routes), Array.empty, Some(vrp))
    }

    val bench = InvTestBench(createInv, "Test the vehicle searchers")
    bench.test()
  }
}
