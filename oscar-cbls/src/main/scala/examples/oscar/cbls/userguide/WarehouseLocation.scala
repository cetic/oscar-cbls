/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package examples.oscar.cbls.userguide

import examples.oscar.cbls.wlp.WarehouseLocationGenerator
import oscar.cbls._

import scala.language.postfixOps

object WarehouseLocation extends CBLSModel with App {

  //the number of warehouses
  val W:Int = 1000

  //the number of delivery points
  val D:Int = 300

  println(s"WarehouseLocation(W:$W, D:$D)")

  //the cost per delivery point if no location is open (theoretical value)
  val defaultCostForNoOpenWarehouse = 10000

  //loading some data (randomly generated in this case)
  val (costForOpeningWarehouse,distanceCost) = WarehouseLocationGenerator.apply(W,D,0,100,3)

  //decision variables of the problem: an array of boolean variables
  val warehouseOpenArray = Array.tabulate(W)(l => CBLSIntVar(0, 0 to 1, s"warehouse_${l}_open"))

  //the set of open warehouses, derived from the array of boolean variables;
  // filter has a default filtering criterion, it selects indices in the array
  // whose variable has a value different from zero
  val openWarehouses = filter(warehouseOpenArray).setName("openWarehouses")

  //for each delivery localtion, the distance to the nearest open warehouse
  val distanceToNearestOpenWarehouseLazy = Array.tabulate(D)(d =>
    minConstArrayValueWise(distanceCost(d).map(_.toInt), openWarehouses, defaultCostForNoOpenWarehouse))

  //the objective criterion with its two parts: operation cost, and construction cost
  val obj = Objective(sum(distanceToNearestOpenWarehouseLazy) + sum(costForOpeningWarehouse, openWarehouses))

  //when the model is completed, it is closed by this method
  close()

  //The search procesdure featuring two neighborhoods and a restart meta heuristics that randomly switches one fifts of the warehouses.
  //The restart stops after two consecutive restarts without improvements and restores the best solution
  val neighborhood =
    bestSlopeFirst(
      List(
        assignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
        swapsNeighborhood(warehouseOpenArray, "SwapWarehouses")),refresh = W/10)
    .onExhaustRestartAfter(randomizeNeighborhood(warehouseOpenArray, () => W/10), 2, obj)

  //this triggers some verbosities on the console:
  //0 is no verbosities
  // 1 is one line every 0.1 second (roughly) summarising all moves performed
  // 2 is one line per move performed
  // 3 is all neighborhoods explored
  // 4 is all neighbors explored
  neighborhood.verbose = 1

  //this starts the search
  neighborhood.doAllMoves(obj=obj)

  println(openWarehouses)
}
