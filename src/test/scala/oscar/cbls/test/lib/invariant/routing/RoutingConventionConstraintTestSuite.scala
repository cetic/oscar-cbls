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
import oscar.cbls.core.computation.Store
import oscar.cbls.modeling.routing.VRS

class RoutingConventionConstraintTestSuite extends AnyFunSuite with Matchers {

  def initVRS: VRS = {
    val model = new Store(debugLevel = 3)
    val vrs   = VRS(model, 20, 3, debug = true)
    model.close()
    for (node <- 9 to vrs.v by -1) {
      val explorer = vrs.routes.pendingValue.explorerAtAnyOccurrence(node % 3)
      vrs.routes.insertAfterPosition(node, explorer.get)
    }
    model.propagate()
    // route == 0 -> 3 -> 6 -> 9 -> 1 -> 4 -> 7 -> 2 -> 5 -> 8

    vrs
  }

  def containMsg(e: Exception, msg: String): Boolean = e.getMessage.contains(msg)

  test("Routing convention fails when inserting a vehicle") {
    val vrs = initVRS

    val explorer = vrs.routes.pendingValue.explorerAtPosition(0)
    vrs.routes.insertAfterPosition(2, explorer.get)

    val ex = intercept[IllegalArgumentException](vrs.store.propagate())
    assert(
      containMsg(ex, "Trying to insert a vehicle!"),
      "Exception issue when inserting a vehicle"
    )
  }

  test("Routing convention fails when inserting node outside the domain") {
    val vrs = initVRS

    val explorer = vrs.routes.pendingValue.explorerAtPosition(0)
    vrs.routes.insertAfterPosition(50, explorer.get)

    val ex = intercept[IllegalArgumentException](vrs.store.propagate())
    assert(
      containMsg(ex, "Trying to insert a node outside the domain!"),
      "Exception issue when inserting a vehicle"
    )
  }

  test("Routing convention fails when inserting an already routed node") {
    val vrs = initVRS

    val explorer = vrs.routes.pendingValue.explorerAtPosition(0)
    vrs.routes.insertAfterPosition(5, explorer.get)

    val ex = intercept[IllegalArgumentException](vrs.store.propagate())
    assert(
      containMsg(ex, "Node already inserted!"),
      "Exception issue when inserting a already routed node"
    )
  }

  test("Routing convention fails when removing a vehicle") {
    val vrs = initVRS

    val explorer = vrs.routes.pendingValue.explorerAtPosition(0)
    vrs.routes.remove(explorer.get)

    val ex = intercept[IllegalArgumentException](vrs.store.propagate())
    assert(containMsg(ex, "Trying to remove a vehicle!"), "Exception issue when removing a vehicle")

  }

  test("Routing convention fails when moving a vehicle at the start of the moved segment") {
    val vrs = initVRS

    val from  = vrs.routes.pendingValue.explorerAtPosition(0).get
    val to    = vrs.routes.pendingValue.explorerAtAnyOccurrence(6).get
    val after = vrs.routes.pendingValue.explorerAtAnyOccurrence(2).get
    vrs.routes.move(from, to, after, flip = false)
    val ex = intercept[IllegalArgumentException](vrs.store.propagate())
    assert(containMsg(ex, "Trying to move a vehicle!"), "Exception issue when moving a vehicle")

  }

  test("Routing convention fails when moving a vehicle at the end of the moved segment") {
    val vrs = initVRS

    val from  = vrs.routes.pendingValue.explorerAtPosition(1).get
    val to    = vrs.routes.pendingValue.explorerAtAnyOccurrence(1).get
    val after = vrs.routes.pendingValue.explorerAtAnyOccurrence(2).get
    vrs.routes.move(from, to, after, flip = false)
    val ex = intercept[IllegalArgumentException](vrs.store.propagate())
    assert(containMsg(ex, "Trying to move a vehicle!"), "Exception issue when moving a vehicle")

  }

  test("Routing convention fails when moving a vehicle inside the moved segment") {
    val vrs = initVRS

    val from  = vrs.routes.pendingValue.explorerAtAnyOccurrence(6).get
    val to    = vrs.routes.pendingValue.explorerAtAnyOccurrence(4).get
    val after = vrs.routes.pendingValue.explorerAtAnyOccurrence(5).get
    vrs.routes.move(from, to, after, flip = false)
    val ex = intercept[IllegalArgumentException](vrs.store.propagate())
    assert(containMsg(ex, "Trying to move a vehicle!"), "Exception issue when moving a vehicle")

  }

  test("Routing convention fails when assigning seq with wrong number of vehicle") {
    val vrs = initVRS
    vrs.routes := IntSequence(List(0, 5, 6, 2, 8, 10))
    val ex = intercept[IllegalArgumentException](vrs.store.propagate())
    assert(
      containMsg(ex, s"The assigned sequence has not the expected number (${vrs.v}) of vehicles!"),
      "Exception issue when assigning a new route"
    )
  }

  test("Routing convention fails when assigning seq with nodes outside the domain") {
    val vrs = initVRS
    vrs.routes := IntSequence(List(0, 5, 6, 1, 8, 10, 2, 7, 50))
    val ex = intercept[IllegalArgumentException](vrs.store.propagate())
    assert(
      containMsg(ex, s"The assigned sequence has node bigger than ${vrs.n}!"),
      "Exception issue when assigning a new route"
    )
  }

  test("Routing convention fails when assigning seq with bad vehicle order") {
    val vrs = initVRS
    vrs.routes := IntSequence(List(0, 5, 6, 2, 8, 10, 1, 7))
    val ex = intercept[IllegalArgumentException](vrs.store.propagate())
    assert(
      containMsg(ex, "The vehicles' depots are not sorted properly!"),
      "Exception issue when assigning a new route"
    )
  }

  test("Routing convention fails when assigning seq with duplicated values") {
    val vrs = initVRS
    vrs.routes := IntSequence(List(0, 5, 6, 1, 8, 10, 5, 2, 7, 6))
    val ex = intercept[IllegalArgumentException](vrs.store.propagate())
    assert(
      containMsg(ex, s"Some nodes are duplicated!"),
      "Exception issue when assigning a new route"
    )
  }

  test("Routing convention works when using checkpoint") {
    val vrs = initVRS
    vrs.routes.defineCurrentValueAsCheckpoint()

    var explorer = vrs.routes.pendingValue.explorerAtAnyOccurrence(1).get
    vrs.routes.insertAfterPosition(15, explorer)
    explorer = vrs.routes.pendingValue.explorerAtAnyOccurrence(9).get
    vrs.routes.remove(explorer)
    vrs.routes.defineCurrentValueAsCheckpoint()

    explorer = vrs.routes.pendingValue.explorerAtAnyOccurrence(7).get
    vrs.routes.insertAfterPosition(16, explorer)
    explorer = vrs.routes.pendingValue.explorerAtAnyOccurrence(16).get
    vrs.routes.insertAfterPosition(17, explorer)
    vrs.routes.rollbackToTopCheckpoint()
    vrs.routes.releaseTopCheckpoint()

    explorer = vrs.routes.pendingValue.explorerAtAnyOccurrence(2).get
    vrs.routes.insertAfterPosition(17, explorer)

    noException must be thrownBy vrs.store.propagate()
  }

  test("Routing convention fails when inserting two times the same value") {
    val vrs      = initVRS
    var explorer = vrs.routes.pendingValue.explorerAtAnyOccurrence(3).get
    vrs.routes.insertAfterPosition(15, explorer)
    explorer = vrs.routes.pendingValue.explorerAtAnyOccurrence(7).get
    vrs.routes.insertAfterPosition(15, explorer)

    val ex = intercept[IllegalArgumentException](vrs.store.propagate())

    assert(
      containMsg(ex, "Node already inserted!"),
      "Exception issue when inserting a already routed node"
    )
  }

  test("Routing convention constraint manage multiple propagation") {
    val model = new Store(debugLevel = 3)
    val vrs   = VRS(model, 10, 2, debug = true)
    model.close()
    vrs.routes.defineCurrentValueAsCheckpoint()
    var explorer = vrs.routes.pendingValue.explorerAtPosition(0).get
    vrs.routes.insertAfterPosition(5, explorer)
    model.propagate()

    vrs.routes.rollbackToTopCheckpoint()
    explorer = vrs.routes.pendingValue.explorerAtPosition(1).get
    vrs.routes.insertAfterPosition(5, explorer)

    model.propagate()
  }

  test("Routing convention works with assign") {
    val vrs = initVRS
    vrs.routes := IntSequence(List(0, 1, 2))
    val explorer = vrs.routes.pendingValue.explorerAtAnyOccurrence(2).get
    vrs.routes.insertAfterPosition(3, explorer)
    vrs.store.propagate()
  }
}
