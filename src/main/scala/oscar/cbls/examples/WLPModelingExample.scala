package oscar.cbls.examples

import oscar.cbls._
import oscar.cbls.algo.generator.WarehouseLocationGenerator
import oscar.cbls.modeling.{Invariants => Inv, Neighborhoods => Nbrs}

object WLPModelingExample {
  def main(args: Array[String]): Unit = {

    // Problem instance parameters
    val nFacilities    = 300
    val deliveryPoints = 1000
    val (fixedCosts, _, _, distanceMatrix, _) =
      WarehouseLocationGenerator.generateRandomWLP(nFacilities, deliveryPoints)

    implicit val m: Model = model("WLP example")

    // decision variables
    val facilitiesVariables = Array.tabulate(nFacilities)(f => {
      m.intVar(0, min = 0, max = 1, name = s"facility_${f}_open")
    })

    // derived variables (using invariants)
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

    // objective function
    // + 2 is obviously unnecessary, it's just here to demonstrate it's possible to add constants
    val obj = m.minimize(objExpr + 2)

    m.close()

    // At the moment it's necessary to specify the domain of the variables involved
    val search = Nbrs.assign(facilitiesVariables, varsDomain = (_, _) => List(0, 1))

    search.doAllMoves(obj)

    println(s"Best objective: ${obj.objValue}")
  }
}
