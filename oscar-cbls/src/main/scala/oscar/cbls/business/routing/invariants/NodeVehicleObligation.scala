package oscar.cbls.business.routing.invariants

import oscar.cbls.core.computation.{CBLSIntVar, ChangingSeqValue}

/**
 * Created by rdl on 20-09-17.
 */
object NodeVehicleObligation{
  /**
   * this invariant maintains a degree of violation for route restriction constraints.
   * there is a set of obligation node->set of vehicles (it must reach one vehicle among any of the given set, or be non-routed)
   * the invariant maintains, for each vehicle, the number of node
   * that it reaches although it should not, according to the mentioned restrictions.
   * we consider that a node that is not routed does not violate the obligation constraint
   * @param routes
   * @param v the number of vehicles
   * @param n the number of nodes
   * @param nodeVehicleObligation the obligation that we are monitoring
   * @return an array telling the violation per vehicle
   * @note this is a preliminary naive version of the constraint. a faster one is to be developed!
   */
  def apply(routes:ChangingSeqValue,v:Int,n:Int, nodeVehicleObligation:Map[Int,Set[Int]]):Array[CBLSIntVar] = {
    val violationPerVehicle =  Array.tabulate(v)(vehicle => CBLSIntVar(routes.model,name=s"violation of NodeVehicleObligation for vehicle$vehicle"))

    val vehicles = 0 until v

    var nodeVehicleRestrictions:List[(Int,Int)] = List.empty

    for((node,vehicleObligations) <- nodeVehicleObligation){
      if(vehicleObligations.size < v){
        val forbiddenVehicles = vehicles.filterNot(vehicleObligations)
        for(forbiddenVehicle <- forbiddenVehicles){
          nodeVehicleRestrictions = (node,forbiddenVehicle) :: nodeVehicleRestrictions
        }
      }
    }

    new NodeVehicleRestrictions(routes, v, nodeVehicleRestrictions, violationPerVehicle)

    violationPerVehicle
  }
}
