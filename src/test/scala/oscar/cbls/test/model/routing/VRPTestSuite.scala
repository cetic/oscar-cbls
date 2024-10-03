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

package oscar.cbls.test.model.routing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import oscar.cbls.core.computation.Store
import oscar.cbls.model.routing.VRP
import oscar.cbls.algo.sequence.{IntSequenceExplorer, RootIntSequenceExplorer}

class VRPTestSuite extends AnyFunSuite with Matchers {

  private def initVRP: VRP = {
    val model = new Store(debugLevel = 3)
    val vrp   = VRP(model, 11, 2, debug = true)
    model.close()
    for (node <- 9 to vrp.v by -1) {
      val explorer = vrp.routes.pendingValue.explorerAtAnyOccurrence(node % 2)
      vrp.routes.insertAfterPosition(node, explorer.get)
    }
    model.propagate()

    vrp
  }

  test("Test routeOfVehicle") {
    val vrp = initVRP

    val route0 = vrp.routeOfVehicle(0)
    route0 must equal(List(0, 2, 4, 6, 8))

    val route1 = vrp.routeOfVehicle(1)
    route1 must equal(List(1, 3, 5, 7, 9))
  }

  test("test nextNodeOf") {
    val vrp = initVRP

    vrp.nextNodeOf(0) must contain(2)
    vrp.nextNodeOf(2) must contain(4)
    vrp.nextNodeOf(4) must contain(6)
    vrp.nextNodeOf(6) must contain(8)
    vrp.nextNodeOf(8) must contain(1)
    vrp.nextNodeOf(1) must contain(3)
    vrp.nextNodeOf(3) must contain(5)
    vrp.nextNodeOf(5) must contain(7)
    vrp.nextNodeOf(7) must contain(9)
    vrp.nextNodeOf(9) mustBe empty
    vrp.nextNodeOf(10) must contain(11)
  }

  test("test previousNodeOfAllNodes") {
    val vrp = initVRP

    val prev = vrp.previousNodeOfAllNodes
    prev must equal(Array(11, 11, 0, 1, 2, 3, 4, 5, 6, 7, 11))
  }

  test("test nextNodeOfAllNodes") {
    val vrp = initVRP

    val next = vrp.nextNodeOfAllNodes
    next must equal(Array(2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 11))

  }

  test("test routesPositionOfAllNodes") {
    val vrp = initVRP

    val pos = vrp.routesPositionOfAllNodes
    pos must equal(Array(0, 5, 1, 6, 2, 7, 3, 8, 4, 9, 11))
  }

  test("Can't create a VRP with less than one vehicle") {
    val model = new Store()
    an[IllegalArgumentException] must be thrownBy VRP(model, 10, -1)
  }

  test("nextNodeInRouting works as expected") {
    val vrp            = initVRP
    var exp            = vrp.routes.value().explorerAtPosition(-1).get
    val expectedValues = Array(0, 2, 4, 6, 8, 0, 3, 5, 7, 9, 1)
    var i              = 0
    while (
      exp match {
        case root: RootIntSequenceExplorer => root.beforeStart
        case _: IntSequenceExplorer        => true
      }
    ) {
      vrp.nextNodeInRouting(exp) must be(expectedValues(i))
      exp = exp.next
      i += 1
    }

  }
}
