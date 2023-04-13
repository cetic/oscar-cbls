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
package examples.oscar.cbls.wlp

import oscar.cbls._
import oscar.cbls.algo.generator.WarehouseLocationGenerator
import oscar.cbls.core.computation.{CBLSIntVar, Store}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.Best
import oscar.cbls.lib.invariant.logic.{Filter, SelectLESetQueue}
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.search.neighborhoods.{AssignMove, AssignNeighborhood}

import scala.collection.immutable.SortedSet

/**
 * this is a WarehouseLocation problem with a Tabu.
 * the purpose is to illustrate how standard neighborhoods can be tuned to encompass
 * additional behaviors. Here, we restrict a neighborhood to a specific set of variables that not tabu
 * this set of variables is maintained through invariants
 */
object WarehouseLocationTabuAspiration extends App {

  //the number of warehouses
  val W:Int = 500

  //the number of delivery points
  val D:Int = 500

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
  val tabuArray = Array.tabulate(W)(w => CBLSIntVar(m,0))
  val iterationCount = CBLSIntVar(m)
  val nonTabuWarehouses = SelectLESetQueue(tabuArray,iterationCount).setName("non tabu warehouses")
  val tabuWarehouses = CBLSSetConst(SortedSet.empty[Int] ++ (0 until W)) minus nonTabuWarehouses

  m.close()

  //this composite neighborhood includes:
  // *the search part restricted to non tabu warehouses
  // *the update of the tabu and iteration count
  // *the stop criterion based on maxMoves since last improvement over obj
  // *the protection of the objectiveFunction
  val tabuTenure = 20

  //TODO: this does not work at all.

  var bestKnown:Long = Long.MaxValue

  val switchWithTabuNeighborhood = (

    (AssignNeighborhood(
      warehouseOpenArray,
      "Aspiration",
      searchZone = tabuWarehouses,
      selectIndiceBehavior = Best(),
      hotRestart = false
    ).improvingOverBestKnown(() => bestKnown) best
      AssignNeighborhood(
        warehouseOpenArray,
        "SwitchWarehouseTabu",
        searchZone = nonTabuWarehouses, //select non tabu warehouses only
        selectIndiceBehavior = Best(),
        hotRestart = false //we do not need hot restart since looking for best
      )).afterMoveOnMove({case (a:AssignMove) => {
      //update the tabu mechanics
      tabuArray(a.id) := iterationCount.value + tabuTenure
      iterationCount :+= 1
      bestKnown = bestKnown min a.objAfter
    }}).acceptAll()
      maxMoves W/2 withoutImprovementOver obj
      saveBestAndRestoreOnExhaust obj
      showObjectiveFunction obj)

  switchWithTabuNeighborhood.verbose = 2

  //all moves are accepted because the neighborhood returns the best found move, and tabu might degrade obj.
  switchWithTabuNeighborhood.doAllMoves(obj=obj)

  println(openWarehouses)
  switchWithTabuNeighborhood.profilingOnConsole()
}
