package oscar.cbls.test.core.search

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.algo.generator.RoutingGenerator
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntConstant
import oscar.cbls.core.computation.objective.Minimize
import oscar.cbls.core.search.profiling.SelectionProfiler
import oscar.cbls.lib.invariant.numeric.Prod2
import oscar.cbls.lib.invariant.routing.TotalRouteLength
import oscar.cbls.lib.neighborhoods.combinator.{BestSlopeFirst, DynAndThen, Exhaust, RoundRobin}
import oscar.cbls.lib.neighborhoods.routing.{
  InsertPointNeighborhoodUnroutedFirst,
  OnePointMoveNeighborhood,
  RemovePointNeighborhood
}
import oscar.cbls.modeling.routing.VRS

class ProfilingTestSuite extends AnyFunSuite with Matchers {

  test(
    "Profiling : CompositionProfiler works when first call to left Neighborhood of DynAndThen finds nothing."
  ) {
    val model: Store = new Store()
    val vrs          = VRS(model, 10, 1)
    val routingData  = RoutingGenerator.generateRandomRoutingData(10, 1000, 0)

    val routeLength           = TotalRouteLength(vrs, routingData._2)
    val nbUnrouted            = vrs.unrouted.size()
    val nbUnroutedWithPenalty = nbUnrouted * IntConstant(model, routingData._3)
    val obj                   = Minimize(routeLength.routeLength + nbUnroutedWithPenalty)

    model.close()

    val insertPoint: InsertPointNeighborhoodUnroutedFirst =
      InsertPointNeighborhoodUnroutedFirst(
        vrs,
        () => vrs.unroutedNodes,
        _ => vrs.routedWithVehicles.value()
      )
    val removeDynAndThenInsert = DynAndThen(
      RemovePointNeighborhood(vrs, () => vrs.routedWithoutVehicles.value()),
      _ => insertPoint
    )

    // Trying remove and then insert, should not find anything
    // Then inserting stuff
    // Then again remove and then insert
    val search = Exhaust(Exhaust(removeDynAndThenInsert, insertPoint), removeDynAndThenInsert)
    search.profileSearch()
    noException mustBe thrownBy(search.doAllMoves(obj))
    vrs.unroutedNodes mustBe empty
  }

  test("Profiling : Profiling Routing works") {
    val model: Store          = new Store(debugLevel = 3)
    val vrs: VRS              = VRS(model, 20, 2)
    val routingData           = RoutingGenerator.generateRandomRoutingData(102, 1000000, 0)
    val routeLength           = TotalRouteLength(vrs, routingData._2)
    val nbUnrouted            = vrs.unrouted.size()
    val nbUnroutedWithPenalty = nbUnrouted * IntConstant(model, routingData._3)
    val obj                   = routeLength.routeLength + nbUnroutedWithPenalty
    model.close()

    def onePtMove(name: String) = OnePointMoveNeighborhood(
      vrs,
      () => vrs.routedWithoutVehicles.pendingValue.take(5),
      movingNode => vrs.routedWithVehicles.pendingValue.filter(_ != movingNode).take(5),
      name = name
    )
    val moveAndThenMove = DynAndThen(onePtMove("LeftOnePtMove"), _ => onePtMove("RightOnePtMove"))
    val insert = InsertPointNeighborhoodUnroutedFirst(
      vrs,
      () => vrs.unroutedNodes,
      _ => vrs.routedWithVehicles.pendingValue
    )
    val search = RoundRobin(Array((insert, 1), (moveAndThenMove, 1)))
    search.profileSearch()
    // search.verbosityLevel = 2
    noException mustBe thrownBy(search.doAllMoves(Minimize(obj)))
    // search.displayProfiling()
  }

  test("Profiling : Selection profiler works properly") {
    val model: Store          = new Store()
    val vrs                   = VRS(model, 100, 1)
    val routingData           = RoutingGenerator.generateRandomRoutingData(vrs.n, 1000000, 0)
    val routeLength           = TotalRouteLength(vrs, routingData._2)
    val nbUnrouted            = vrs.unrouted.size()
    val nbUnroutedWithPenalty = nbUnrouted * IntConstant(model, routingData._3)
    val obj                   = Minimize(routeLength.routeLength + nbUnroutedWithPenalty)
    model.close()
    val n1 = InsertPointNeighborhoodUnroutedFirst(
      vrs,
      () => vrs.unrouted.value(),
      _ => vrs.routedWithVehicles.value()
    )
    val n2 = OnePointMoveNeighborhood(
      vrs,
      () => vrs.routedWithoutVehicles.value(),
      node => vrs.routedWithVehicles.value().filter(x => x != node)
    )
    val search = BestSlopeFirst(List(n1, n2), refreshRate = 2)
    search.profileSearch()
    val profiler = search.searchProfiler().get.asInstanceOf[SelectionProfiler]

    // Start : nothing has run yet ==> slope is Long.maxValue
    profiler.efficiencySlope(0) should be(Long.MaxValue)
    profiler.efficiencySlope(1) should be(Long.MaxValue)

    search.doAllMoves(objective = obj, x => x == 1)
    // One move done (first in the list) ==> slope for (0) is no longer maxValue and should be positive (insertion)
    profiler.efficiencySlope(0) != Long.MaxValue should be(true)
    profiler.efficiencySlope(0) > 0 should be(true)
    profiler.efficiencySlope(1) should be(Long.MaxValue)

    search.doAllMoves(objective = obj, x => x == 1)
    // One more move (second in the list) ==> slope for (1) is no longer maxValue and should be 0
    // (only one place for the only node routed)
    profiler.efficiencySlope(0) != Long.MaxValue should be(true)
    profiler.efficiencySlope(0) > 0 should be(true)
    profiler.efficiencySlope(1) != Long.MaxValue should be(true)
    profiler.efficiencySlope(1) == 0 should be(true)

    search.doAllMoves(objective = obj, x => x == 1)
    // One more move ==> reset and search a move ==> slope for (0) should be positive (insertion),
    // slope for (1) should be MaxValue (reset)
    profiler.efficiencySlope(0) != Long.MaxValue should be(true)
    profiler.efficiencySlope(0) > 0 should be(true)
    profiler.efficiencySlope(1) should be(Long.MaxValue)

  }
}
