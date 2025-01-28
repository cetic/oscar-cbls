package oscar.cbls.examples

import oscar.cbls._
import oscar.cbls.algo.generator.WarehouseLocationGenerator
import oscar.cbls.visual.cartesian.wlp.WLPInterface

import oscar.cbls.modeling.{Invariants => Inv, Neighborhoods => Nbrs}

import scala.io.StdIn

object WLPVisualExample {
  def main(args: Array[String]): Unit = {
    // Problem instance parameters
    val nbWarehouses  = 300
    val nbDelivery    = 1000
    val minXY         = 0
    val maxXY         = 750
    val weightOpening = 3

    WarehouseLocationGenerator.setMapDimensions(minXY, maxXY)

    val (
      costsForOpeningWarehouses,
      warehousesPositions,
      deliveryPositions,
      distancesDeliveryWarehouses,
      _
    ) = WarehouseLocationGenerator.generateRandomWLP(nbWarehouses, nbDelivery, weightOpening)

    // WLP model and search procedure taken from WLPModelingExample
    implicit val m: Model = model("WLP example")

    val facilitiesVariables = Array.tabulate(nbWarehouses)(f => {
      m.intVar(0, min = 0, max = 1, name = s"facility_${f}_open")
    })
    val openFacilities = Inv.logic.filter(facilitiesVariables, name = "Set of open facilities")

    val distancesToNearestOpenFacility = Array.tabulate(nbDelivery)(d =>
      Inv.minMax.min(
        distancesDeliveryWarehouses(d),
        openFacilities,
        name = s"Distance of $d to nearest facility"
      )
    )
    val objExpr =
      sum(distancesToNearestOpenFacility) + partialSum(
        costsForOpeningWarehouses,
        indices = openFacilities
      )
    val obj = m.minimize(objExpr)
    m.close()

    // GUI
    val wlpInterface =
      WLPInterface(
        nbWarehouses,
        nbDelivery,
        warehousesPositions ++ deliveryPositions,
        openFacilities,
        obj.objValue
      )

    // The search procedure includes a combinator that updates the visualization
    val search = Nbrs.combinator.updateDisplay(
      Nbrs.assign(facilitiesVariables, varsDomain = (_, _) => List(0, 1)),
      wlpInterface
    )
    search.doAllMoves(obj)
    println(s"Best objective: ${obj.objValue}")

    // May be necessary to keep the display alive depending on your platform
    StdIn.readLine()
  }
}
