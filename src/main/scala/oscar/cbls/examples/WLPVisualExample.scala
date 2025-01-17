package oscar.cbls.examples

import oscar.cbls._
import oscar.cbls.algo.generator.WarehouseLocationGenerator
import oscar.cbls.lib.neighborhoods.AssignNeighborhood
import oscar.cbls.visual.wlp.WLPInterface

import scala.collection.SortedSet

object WLPVisualExample {
  private val nbWarehouses = 300
  private val nbDelivery = 1000
  private val minXY = 0
  private val maxXY = 750
  private val weightOpening = 3
  WarehouseLocationGenerator.setMapDimensions(minXY, maxXY)
  private val (
    costsForOpeningWarehouses,
    warehousesPositions,
    deliveryPositions,
    distancesDeliveryWarehouses,
    _
    ) = WarehouseLocationGenerator.generateRandomWLP(
    nbWarehouses, nbDelivery, weightOpening
  )
  ////////
  // GUI
  private val wlpInterface = new WLPInterface(
    nbWarehouses,
    nbDelivery,
    costsForOpeningWarehouses,
    warehousesPositions,
    deliveryPositions,
    distancesDeliveryWarehouses,
    wlpSearchProcedure()
  )

  /**
   * WLP Search Procedure
   * Taken from Stefano's WLPModelingExample
   */
  private def wlpSearchProcedure(): Unit = {
    val startTime = System.nanoTime()
    implicit val m: Model = model("WLP example")
    // decision variables
    val facilitiesVariables = Array.tabulate(nbWarehouses)(f => {
      // this is equivalent to m.binaryVar(name = s"facility_${f}_open")
      m.intVar(0, min = 0, max = 1, name = s"facility_${f}_open")
    })
    val openFacilities = m.logic.filter(facilitiesVariables, name = "Set of open facilities")
    val distancesToNearestOpenFacility = Array.tabulate(nbDelivery)(d =>
      m.minMax.min(distancesDeliveryWarehouses(d), openFacilities, name = s"Distance of $d to nearest facility")
    )
    // sum could be a predefined method imported automatically, equivalent to m.Numeric.sum
    // to achieve this, the package object currently extends the Numeric trait
    val objExpr =
      sum(distancesToNearestOpenFacility) + partialSum(costsForOpeningWarehouses, indices = openFacilities)
    // we could consider adding .setName() to handle variables instantiated like this
    val obj = m.minimize(objExpr + 2)
    m.close()
    //
    // actual search procedure
    //
    val search = AssignNeighborhood(facilitiesVariables, (_, _) => List(0, 1))
    search.doAllMoves(obj)
    println(s"Best objective: ${obj.objValue}")
    //
    // encode the final result as a "single" move to the GUI
    //
    val newTime = (System.nanoTime() - startTime) * 1e-9
    val openWs = SortedSet[Int]() ++ openFacilities.value()
    val mv = wlpInterface.Move(wlpInterface.SortedSetResult(openWs, obj.objValue.value()))
    wlpInterface.update(mv, newTime, 0)
  }

  def main(args: Array[String]): Unit = {
    wlpInterface.main(args)
  }

}
