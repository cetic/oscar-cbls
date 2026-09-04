package oscar.cbls.examples

// Imports to model a problem and a search procedure
import oscar.cbls._
import oscar.cbls.core.computation.Store
import oscar.cbls.core.distributed.DistributedSearch
import oscar.cbls.lib.neighborhoods.combinator.distributed.DistributedPopulationBased
import oscar.cbls.modeling.{Invariants => Inv, Neighborhoods => Nrs}

// Object to generate random data
import oscar.cbls.algo.generator.WarehouseLocationGenerator

/** Example of using the DistributedPopulationBased combinator on a Warehouse Location Problem.
  *
  * This example is modeled after WLPDistributedExample but uses a population-based meta-heuristic
  * instead of the DistributedModulo combinator. The population-based search maintains a population
  * of solutions, diversifies them by running neighborhoods on workers in parallel, and selects the
  * best individuals at each iteration.
  */
object WLPDistributedPopulationBasedExample {
  def main(args: Array[String]): Unit = {

    // Problem instance parameters
    val nbFacilities   = 300
    val deliveryPoints = 1000

    val (fixedCosts, warehousesPositions, deliveryPositions, distanceMatrix, _) =
      WarehouseLocationGenerator.generateRandomWLP(nbFacilities, deliveryPoints)

    def createModelAndSearch: (Store, Objective, Neighborhood) = {
      // Model definition
      implicit val m: Model = model("WLP Population-Based example")

      // Decision variables
      val facilitiesVariables = Array.tabulate(nbFacilities)(f => {
        binaryVar(0, name = s"facility_${f}_open")
      })

      // Derived variables (using invariants)
      val openFacilities = Inv.logic.filter(facilitiesVariables, name = "Set of open facilities")

      val distancesToNearestOpenFacility = Array.tabulate(deliveryPoints)(d =>
        Inv.minMax
          .minOfConstants(
            distanceMatrix(d),
            openFacilities,
            name = s"Distance of $d to nearest facility"
          )
      )

      val objExpr =
        sum(distancesToNearestOpenFacility) + partialSumOfConstants(
          fixedCosts,
          indices = openFacilities
        )

      val obj = m.minimize(objExpr)

      m.close()

      // Population-based distributed search:
      // - Uses an exhaust combinator with assign and swap neighborhoods for diversification
      // - Each iteration diversifies all individuals by running doAllMoves on workers
      // - Keeps the 5 best individuals at each iteration
      // - Runs for 10 iterations
      val search: Neighborhood =
        DistributedPopulationBased.simple(
          neighborhoods = Array(
            // Neighborhood 1: assign then swap, with hotRestart for randomized exploration
            Nrs.combinator.exhaust(
              List(
                Nrs.assign(
                  facilitiesVariables,
                  hotRestart = true,
                  name = "assign1"
                ),
                Nrs.swap(
                  facilitiesVariables,
                  hotRestart = true,
                  name = "swap1"
                )
              )
            ),
            // Neighborhood 2: swap then assign (different order explores differently)
            Nrs.combinator.exhaust(
              List(
                Nrs.swap(
                  facilitiesVariables,
                  hotRestart = true,
                  name = "swap2"
                ),
                Nrs.assign(
                  facilitiesVariables,
                  hotRestart = true,
                  name = "assign2"
                )
              )
            )
          ),
          step = (it, _) => if (it < 10) Some(5) else None,
          store = m.store,
          saveAnytimeBest = true,
          keepOld = true
        )

      (m.store, obj, search)
    }

    // Create the supervisor
    val (store, obj, search) = createModelAndSearch

    // Create the distributed search infrastructure
    val distributedSearch =
      DistributedSearch(store, verbose = 0, search = search)

    // Spawn workers (1 per 4 logical cores)
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
