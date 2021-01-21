/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
package oscar.examples.cbls.wlp

import oscar.cbls._
import oscar.cbls.core.computation.{CBLSIntVar, Store}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.First
import oscar.cbls.lib.invariant.logic.Filter
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.search.neighborhoods.AssignNeighborhood

import scala.language.postfixOps

/**
  * this is a WarehouseLocation problem with a LateAcceptanceHillClimbing
  * the purpose is to illustrate how standard neighborhoods can be tuned to encompass
  * additional behaviors. Here, we restrict a neighborhood to a specific set of variables that not tabu
  * this set of variables is maintained through invariants
  */
object WarehouseLocationLateAcceptance extends App {

  //the number of warehouses
  val W:Int = 300

  //the number of delivery points
  val D:Int = 150

  println(s"WarehouseLocationLateAcceptance(W:$W, D:$D)")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (costForOpeningWarehouse,distanceCost) = WarehouseLocationGenerator.apply(W,D,0,100,5)

  val m = Store()

  val warehouseOpenArray = Array.tabulate(W)(w => CBLSIntVar(m, 0, 0 to 1, s"warehouse_${w}_open"))

  val openWarehouses = Filter(warehouseOpenArray).setName("openWarehouses")

  val distanceToNearestOpenWarehouse = Array.tabulate(D)(d =>
    minConstArrayValueWise(distanceCost(d).map(_.toInt), openWarehouses, defaultCostForNoOpenWarehouse)
      .setName(s"distance_for_delivery_$d"))

  val obj = Objective(Sum(distanceToNearestOpenWarehouse) + Sum(costForOpeningWarehouse, openWarehouses))

  m.close()
  
  val neighborhoodSA = (AssignNeighborhood(
    warehouseOpenArray,
    selectIndiceBehavior = First(randomized = true),
    hotRestart = false,
    name = "SwitchWarehouse")
    .lateAcceptanceHillClimbing(20,
      maxRelativeIncreaseOnBestObj = 1.2)
    saveBestAndRestoreOnExhaust obj
    showObjectiveFunction obj)

  neighborhoodSA.verbose = 2

  neighborhoodSA.doAllMoves(obj=obj)

  println(openWarehouses)
  println(obj)
}
