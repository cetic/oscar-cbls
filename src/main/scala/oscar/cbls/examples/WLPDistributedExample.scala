package oscar.cbls.examples

// Imports to model a problem and a search procedure
import oscar.cbls._
import oscar.cbls.core.computation.Store
import oscar.cbls.core.distributed.DistributedSearch
import oscar.cbls.lib.neighborhoods.combinator.distributed.DistributedModulo
import oscar.cbls.lib.neighborhoods.combinator.distributed.DistributedModulo.ModuloRange
import oscar.cbls.modeling.{Invariants => Inv, Neighborhoods => Nrs}

// Object to generate random data
import oscar.cbls.algo.generator.WarehouseLocationGenerator

object WLPDistributedExample {
  def main(args: Array[String]): Unit = {

    // Problem instance parameters
    val nbFacilities   = 300
    val deliveryPoints = 1000

    val (fixedCosts, warehousesPositions, deliveryPositions, distanceMatrix, _) =
      WarehouseLocationGenerator.generateRandomWLP(nbFacilities, deliveryPoints)

    def createModelAndSearch: (Store, Objective, Neighborhood) = {
      // Model definition
      implicit val m: Model = model("WLP example")

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

      val search: Neighborhood =
        DistributedModulo((p: ModuloRange) =>
          Nrs.combinator.exhaust(
            List(
              Nrs.assign(
                facilitiesVariables,
                searchZone = Some(() => p.offset until nbFacilities by p.step),
                hotRestart = false,
                name = s"assignModulo(${p.step},${p.offset})"
              ),
              Nrs.swap(
                facilitiesVariables,
                firstSearchZone = Some(() => p.offset until nbFacilities by p.step),
                name = s"swapModulo(${p.step},${p.offset})",
                hotRestart = false
              )
            )
          )
        )

      (m.store, obj, search)
    }

    // create the supervisor
    val (store, obj, search) = createModelAndSearch

    // create the supervisor
    val distributedSearch =
      DistributedSearch(store, verbose = 0, search = search)

    for (_ <- 0 until DistributedSearch.nbCores / 4) {
      // create a worker; we put a delay to test that workers can join anytime.
      val (store, _, search) = createModelAndSearch
      distributedSearch.spawnLocalWorker(store, search)
    }

    search.verbosityLevel = 1
    search.doAllMoves(obj)
    distributedSearch.globalShutDown()

    println("finished")
  }
}
