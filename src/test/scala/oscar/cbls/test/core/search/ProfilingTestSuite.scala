package oscar.cbls.test.core.search

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import oscar.cbls.algo.generator.RoutingGenerator
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.{IntConstant, IntVariable}
import oscar.cbls.core.computation.objective.Minimize
import oscar.cbls.lib.invariant.numeric.Prod2
import oscar.cbls.lib.invariant.routing.TotalRouteLength
import oscar.cbls.lib.invariant.set.Cardinality
import oscar.cbls.lib.neighborhoods.combinator.{DynAndThen, Exhaust, RoundRobin}
import oscar.cbls.lib.neighborhoods.routing.{
  InsertPointNeighborhoodUnroutedFirst,
  OnePointMoveMove,
  OnePointMoveNeighborhood,
  RemovePointNeighborhood
}
import oscar.cbls.modeling.routing.VRP

class ProfilingTestSuite extends AnyFunSuite with Matchers {

  test(
    "CompositionProfiler works when first call to left Neighborhood of DynAndThen finds nothing."
  ) {
    val model: Store = new Store()
    val vrp          = VRP(model, 10, 1)
    val routingData  = RoutingGenerator.generateRandomRoutingData(10, 1000, 0)

    val routeLength           = TotalRouteLength(vrp, routingData._2)
    val nbUnrouted            = IntVariable(model, 10)
    val nbUnroutedWithPenalty = IntVariable(model, 10 * routingData._3)
    Cardinality(model, vrp.unrouted, nbUnrouted)
    Prod2(model, nbUnrouted, IntConstant(model, routingData._3), nbUnroutedWithPenalty)
    val obj = Minimize(routeLength.routeLength + nbUnroutedWithPenalty)

    model.close()

    val insertPoint: InsertPointNeighborhoodUnroutedFirst =
      InsertPointNeighborhoodUnroutedFirst(
        vrp,
        () => vrp.unroutedNodes,
        _ => vrp.routedWithVehicles.value()
      )
    val removeDynAndThenInsert = DynAndThen(
      RemovePointNeighborhood(vrp, () => vrp.routedWithoutVehicles.value()),
      _ => insertPoint
    )

    // Trying remove and then insert, should not find anything
    // Then inserting stuff
    // Then again remove and then insert
    val search = Exhaust(Exhaust(removeDynAndThenInsert, insertPoint), removeDynAndThenInsert)
    search.profileSearch()
    noException mustBe thrownBy(search.doAllMoves(obj))
    vrp.unroutedNodes mustBe empty
  }

  test("Profiling Routing works") {
    val model: Store = new Store(debugLevel = 3)
    val vrp: VRP     = VRP(model, 20, 2)
    val distMatrix   = RoutingGenerator.generateRandomRoutingData(102, 1000000, 0)
    val routeLength  = TotalRouteLength(vrp, distMatrix._2)
    val nbUnrouted   = IntVariable(model, 0L, name = Some("Nb unrouted"))
    Cardinality(model, vrp.unrouted, nbUnrouted)
    val nbUnroutedWithPenalty = IntVariable(model, 0L, name = Some("Penalized unrouted"))
    Prod2(model, nbUnrouted, IntConstant(model, 1000000), nbUnroutedWithPenalty)
    val obj = routeLength.routeLength + nbUnroutedWithPenalty
    model.close()

    def onePtMove(name: String) = OnePointMoveNeighborhood(
      vrp,
      () => vrp.routedWithoutVehicles.pendingValue.take(5),
      movingNode => vrp.routedWithVehicles.pendingValue.filter(_ != movingNode).take(5),
      name = name
    )
    val moveAndThenMove = DynAndThen(onePtMove("LeftOnePtMove"), _ => onePtMove("RightOnePtMove"))
    val insert = InsertPointNeighborhoodUnroutedFirst(
      vrp,
      () => vrp.unroutedNodes,
      _ => vrp.routedWithVehicles.pendingValue
    )
    val search = RoundRobin(Array((insert, 1), (moveAndThenMove, 1)))
    search.profileSearch()
    // search.verbosityLevel = 2
    noException mustBe thrownBy(search.doAllMoves(Minimize(obj)))
    // search.displayProfiling()
  }
}
