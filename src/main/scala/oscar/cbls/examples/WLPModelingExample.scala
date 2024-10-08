package oscar.cbls.examples

import oscar.cbls._
import oscar.cbls.modeling.Model

object WLPModelingExample extends App {

  // *** Input ***

  val nFacilities = 2

  val deliveryPoints = 4

  val fixedCosts: Array[Long] = Array(218, 204)

  val distanceMatrix: Array[Array[Long]] =
    Array(Array(6, 69), Array(53, 23), Array(63, 53), Array(45, 52))

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
    m.intVar(min = 0, max = 1, name = s"facility_${f}_open")
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

}
