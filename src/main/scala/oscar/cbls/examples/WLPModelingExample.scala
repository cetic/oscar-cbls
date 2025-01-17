package oscar.cbls.examples

import oscar.cbls._
import oscar.cbls.algo.generator.WarehouseLocationGenerator
import oscar.cbls.lib.neighborhoods.AssignNeighborhood

object WLPModelingExample extends App {

  // *** Input ***

  val nFacilities = 300

  val deliveryPoints = 1000

  val (fixedCosts, _, _, distanceMatrix, _) = WarehouseLocationGenerator.generateRandomWLP(nFacilities, deliveryPoints)
  // *** Model ***

//   Old version:
//
//    val m = Store()
//
//    val warehouseOpenArray = Array.tabulate(W)(l =>
//     CBLSIntVar(m, 0, 0 to 1, s"warehouse_${l}_open")
//    )
//    val openWarehouses = filter(warehouseOpenArray).setName("openWarehouses")
//
//    val distanceToNearestOpenWarehouseLazy = Array.tabulate(D)(d =>
//      minConstArrayValueWise(distanceCost(d).map(_.toInt), openWarehouses, defaultCostForNoOpenWarehouse)
//    )
//
//    val obj = Objective(sum(distanceToNearestOpenWarehouseLazy) + sum(costForOpeningWarehouse, openWarehouses))
//
//    m.close()

  implicit val m: Model = model("WLP example")

  // decision variables
  val facilitiesVariables = Array.tabulate(nFacilities)(f => {
    // this is equivalent to m.binaryVar(name = s"facility_${f}_open")
    m.intVar(0, min = 0, max = 1, name = s"facility_${f}_open")
  })

  val openFacilities = m.logic.filter(facilitiesVariables, name = "Set of open facilities")

  val distancesToNearestOpenFacility = Array.tabulate(deliveryPoints)(d =>
    m.minMax.min(distanceMatrix(d), openFacilities, name = s"Distance of $d to nearest facility")
  )

  // sum could be a predefined method imported automatically, equivalent to m.Numeric.sum
  // to achieve this, the package object currently extends the Numeric trait
  val objExpr =
    sum(distancesToNearestOpenFacility) + partialSum(fixedCosts, indices = openFacilities)
  // we could consider adding .setName() to handle variables instantiated like this

  val obj = m.minimize(objExpr + 2)

  m.close()

  val search = AssignNeighborhood(facilitiesVariables, (_, _) => List(0, 1))

  search.doAllMoves(obj)

  println(s"Best objective: ${obj.objValue}")

}
