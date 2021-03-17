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
import oscar.cbls.core.search.Best
import oscar.cbls.lib.invariant.logic.{Filter, SelectLESetQueue}
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.search.neighborhoods.{AssignMove, AssignNeighborhood}

/**
  * this is a WarehouseLocation problem with a Tabu.
  * the purpose is to illustrate how standard neighborhoods can be tuned to encompass
  * additional behaviors. Here, we restrict a neighborhood to a specific set of variables that not tabu
  * this set of variables is maintained through invariants
  */
object WarehouseLocationTabu extends App {

  //the number of warehouses
  val W:Int = 50

  //the number of delivery points
  val D:Int = 600

  println(s"WarehouseLocationTabu(W:$W, D:$D)")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (costForOpeningWarehouse,distanceCost) = WarehouseLocationGenerator.apply(W,D,0,100,3)

  val m = Store()

  val warehouseOpenArray = Array.tabulate(W)(w => CBLSIntVar(m, 0, 0 to 1, s"warehouse_${w}_open"))

  val openWarehouses = Filter(warehouseOpenArray).setName("openWarehouses")

  val distanceToNearestOpenWarehouse = Array.tabulate(D)(d =>
    minConstArrayValueWise(distanceCost(d).map(_.toInt), openWarehouses, defaultCostForNoOpenWarehouse).setName(s"distance_for_delivery_$d"))

  val obj = Objective(Sum(distanceToNearestOpenWarehouse) + Sum(costForOpeningWarehouse, openWarehouses))

  // we handle the tabu through invariants.
  // they are completely dissociated from the rest of the model in this case.
  val TabuArray = Array.tabulate(W)(w => CBLSIntVar(m,0))
  val It = CBLSIntVar(m)
  val nonTabuWarehouses = SelectLESetQueue(TabuArray,It).setName("non tabu warehouses")

  m.close()

  //this composite neighborhood includes:
  // *the search part restricted to non tabu warehouses
  // *the update of the tabu and iteration count
  // *the stop criterion based on maxMoves since last improvement over obj
  // *the protection of the objectiveFunction
  val tabuTenure = W/5

  //TODO: this does not work at all.

  val switchWithTabuNeighborhood = (
    AssignNeighborhood(
      warehouseOpenArray,
      "SwitchWarehouseTabu",
      searchZone = nonTabuWarehouses, //select non tabu warehouses only
      selectIndiceBehavior = Best(),
      hotRestart = false
    ) //we do not need hot restart since looking for best
    .afterMoveOnMove((a:AssignMove) => {
      //update the tabu mechanics
      TabuArray(a.id) := It.value + tabuTenure
      It :+= 1
      println(nonTabuWarehouses)
    }).acceptAll()
    maxMoves W withoutImprovementOver obj
    saveBestAndRestoreOnExhaust obj
    showObjectiveFunction obj)

  switchWithTabuNeighborhood.verbose = 2

  //all moves are accepted because the neighborhood returns the best found move, and tabu might degrade obj.
  switchWithTabuNeighborhood.doAllMoves(obj=obj)

  println(openWarehouses)
}
