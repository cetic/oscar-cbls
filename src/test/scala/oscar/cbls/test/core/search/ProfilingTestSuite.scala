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
import oscar.cbls.lib.neighborhoods.combinator.{DynAndThen, Exhaust}
import oscar.cbls.lib.neighborhoods.routing.{
  InsertPointNeighborhoodUnroutedFirst,
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
}
