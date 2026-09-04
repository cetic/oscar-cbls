package oscar.cbls.examples

import oscar.cbls._
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.objective.Minimize
import oscar.cbls.core.distributed.DistributedSearch
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.lib.neighborhoods.combinator._
import oscar.cbls.lib.neighborhoods.combinator.distributed.DistributedPopulationBased
import oscar.cbls.lib.neighborhoods.routing._
import oscar.cbls.modeling.routing.VRS
import oscar.cbls.modeling.{Invariants => Inv}

import oscar.cbls.algo.generator.RoutingGenerator

import scala.util.Random

/** Example of using the DistributedPopulationBased combinator on a Vehicle Routing Problem.
  *
  * This example is the distributed equivalent of the PopulationBasedSearch test that uses:
  *   - A random RemovePoint (removes one random routed node)
  *   - A RoundRobin of OnePointMove and InsertPointInsertionPointFirst
  *   - 3 neighborhoods per individual, keeping the 4 best, for 10 iterations
  *
  * Each worker uses its own Random instance for the removals, so the explorations differ across
  * workers, producing diverse solutions in the population. The initial solution, on the other hand,
  * is built from a fixed seed: the supervisor and all the workers have their own `Store`, and the
  * solutions they exchange carry values only, so these stores must all start from the very same
  * routing to be comparable.
  */
object VRPDistributedPopulationBasedExample {
  def main(args: Array[String]): Unit = {

    val v    = 2
    val n    = 125
    val d    = 100
    val seed = 42

    val (coordinates, distances, _, _) =
      RoutingGenerator.generateRandomRoutingData(n, 2, 0)

    def createModelAndSearch: (Store, Objective, Neighborhood) = {
      // Seeded: this method is called once for the supervisor and once per worker, and all of them
      // must build the exact same initial solution.
      val initialSolutionRandom = new Random(seed)
      // Not seeded: this one only diversifies the local search, it does not affect the model, so
      // each worker may follow its own random stream.
      val searchRandom = new Random()

      val model = new Store()
      val vrs   = VRS(model, n, v)

      // Insert half the non-vehicle nodes into routes (same initial state as the test)
      var toInsert = initialSolutionRandom.shuffle(List.from(v until n)).take((n - v) / 2)
      while (toInsert.nonEmpty) {
        val node     = toInsert.head
        val explorer = vrs.routes.pendingValue.explorerAtPosition(0).get
        vrs.routes.insertAfterPosition(node, explorer)
        toInsert = toInsert.tail
      }

      // Objective: minimize route length + penalty for unrouted nodes
      val routeLength = Inv.routing.totalRouteLength(distances)(vrs)
      val nbUnrouted  = vrs.unrouted.size()
      val objective   = Minimize(routeLength + (nbUnrouted * 1000L))

      model.close()
      model.propagate()

      // Search: random remove then round-robin of move/insert
      def search: Neighborhood = Exhaust(
        MaxMoves(
          AcceptAll(
            RemovePoint(
              vrs,
              relevantNodesToRemove = () => {
                val allPoints = vrs.routedWithoutVehicles.value().toArray
                if (allPoints.nonEmpty) Some(allPoints(searchRandom.nextInt(allPoints.length)))
                else None
              }
            )
          ),
          1
        ),
        RoundRobin(
          Array(
            (
              OnePointMove(
                vrs,
                () => vrs.routedWithoutVehicles.pendingValue,
                (x: Int) =>
                  vrs.routedWithVehicles.value().filter((node: Int) => node != x && node != x - 1),
                selectDestinationBehavior = LoopBehavior.best()
              ),
              1
            ),
            (
              InsertPointInsertionPointFirst(
                vrs,
                () => vrs.routedWithVehicles.value(),
                (_: Int) => vrs.unroutedNodes,
                selectInsertionPointBehavior = LoopBehavior.best()
              ),
              1
            )
          )
        )
      )

      // 3 neighborhoods per individual (equivalent to the test's 3 (search, 0) tuples),
      // keeping 4 best, 10 iterations, parent not kept
      val distributedSearch = DistributedPopulationBased.simple(
        neighborhoods = Array(search, search, search),
        step = (it, _) => if (it < 10) Some(4) else None,
        store = model,
        saveAnytimeBest = false,
        keepOld = false
      )

      (model, objective, distributedSearch)
    }

    // Create the supervisor
    val (store, obj, search) = createModelAndSearch

    // Create the distributed search infrastructure
    val distributedSearch = DistributedSearch(store, verbose = 0, search = search)

    // Spawn workers
    val nbWorkers = Math.max(1, DistributedSearch.nbCores / 4)
    println(s"Nb Workers : $nbWorkers")
    for (_ <- 0 until nbWorkers) {
      val (workerStore, _, workerSearch) = createModelAndSearch
      distributedSearch.spawnLocalWorker(workerStore, workerSearch)
    }

    search.verbosityLevel = 1
    search.doAllMoves(obj)
    distributedSearch.globalShutDown()

    println("finished")
  }
}
