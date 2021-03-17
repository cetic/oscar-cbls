package oscar.cbls.test.routing

import oscar.cbls.Store
import oscar.cbls.business.routing.invariants.MovingVehicles
import oscar.cbls.business.routing.model.VRP

object RollBackTest extends App {

  /*
    val heuristic = {
    (
      (OneChainInsert(pdp) exhaust OneEventInsert(pdp))
      exhaust (bestSlopeFirst(List(OnePtMove(pdp), OneChainMove(pdp), ThreeOptMove(pdp))))
    ).afterMove(pdp.searchSpace.update())
  }
   */

  val n = 100
  val v = 5
  val store = new Store()
  val myVRP = new VRP(store,n,v)
  val routes = myVRP.routes

  val moving = MovingVehicles(routes, v)

  store.close()
  routes.insertAtPosition(16, 3)
  routes.insertAtPosition(17, 4)
  println(moving)

  val checkpoint0 = routes.defineCurrentValueAsCheckpoint()

  {
    routes.insertAtPosition(6, 1)
    routes.insertAtPosition(7, 2)
    val checkpoint1 = routes.defineCurrentValueAsCheckpoint()
    routes.insertAtPosition(8, 3)
    routes.insertAtPosition(9, 3)
    routes.rollbackToTopCheckpoint(checkpoint1)
    routes.insertAtPosition(10, 3)
    routes.insertAtPosition(11, 3)
    routes.rollbackToTopCheckpoint(checkpoint1)
    routes.releaseTopCheckpoint()
    routes.insertAtPosition(14, 3)
    routes.insertAtPosition(15, 4)
  }

  //routes.rollbackToTopCheckpoint(checkpoint0)


  routes.releaseTopCheckpoint()
  routes.insertAtPosition(16, 3)
  routes.insertAtPosition(17, 4)


  val checkpoint0b = routes.defineCurrentValueAsCheckpoint()

  {
    routes.insertAtPosition(6, 1)
    routes.insertAtPosition(7, 2)

    val checkpoint1 = routes.defineCurrentValueAsCheckpoint()
    routes.insertAtPosition(8, 3)
    routes.insertAtPosition(9, 3)
    routes.rollbackToTopCheckpoint(checkpoint1)
    routes.insertAtPosition(10, 3)
    routes.insertAtPosition(11, 3)
    routes.rollbackToTopCheckpoint(checkpoint1)
    routes.releaseTopCheckpoint()
    routes.insertAtPosition(14, 3)
    routes.insertAtPosition(15, 4)
  }

  routes.rollbackToTopCheckpoint(checkpoint0b)
  routes.releaseTopCheckpoint()



  println(myVRP)
  println(moving)

}
