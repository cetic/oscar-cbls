package oscar.cbls.examples

// Imports to model a problem and a search procedure
import oscar.cbls._
import oscar.cbls.modeling.{Invariants => Inv, Neighborhoods => Nrs}

// Object to generate random data
import oscar.cbls.algo.generator.WarehouseLocationGenerator

// GUI
import oscar.cbls.visual.cartesian.wlp.WLPInterface

import scala.io.StdIn

object WLPAdvancedModelingExample {
  def main(args: Array[String]): Unit = {

    // Model definition
    implicit val m: Model = model("WLP example")

    // Problem instance parameters
    val nbFacilities   = 300
    val deliveryPoints = 1000
    val (fixedCosts, warehousesPositions, deliveryPositions, distanceMatrix, _) =
      WarehouseLocationGenerator.generateRandomWLP(nbFacilities, deliveryPoints)

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

    // Objective function
    // + 2 is obviously unnecessary, it's just here to demonstrate it's possible to add constants
    val obj = m.minimize(objExpr + 2)

    m.close()

    // Defines the search procedure
    val search: Neighborhood = Nrs.assign(facilitiesVariables, varsDomain = (_, _) => List(0, 1))

    val visu = WLPInterface(
      nbFacilities,
      deliveryPoints,
      warehousesPositions ++ deliveryPositions,
      openFacilities,
      obj.objValue,
      height = 800,
      width = 800
    )

    val searchWithVisu = Nrs.combinator.updateDisplay(search, visu)

    // Profiling initialization and setting up verbosity
    searchWithVisu.profileSearch()
    searchWithVisu.verbosityLevel = 1

    // Performs the search
    searchWithVisu.doAllMoves(obj)

    // Displays results
    searchWithVisu.displayProfiling()
    println(s"Open facilities: ${openFacilities.value()}")
    println(s"Number of open facilities: ${openFacilities.value().size}")
    println(s"Best objective: ${obj.objValue}")

    // May be necessary to keep the display alive depending on your platform
    StdIn.readLine()
  }
}
