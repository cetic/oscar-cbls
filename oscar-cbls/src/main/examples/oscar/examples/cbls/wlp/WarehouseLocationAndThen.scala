package oscar.examples.cbls.wlp

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

import oscar.cbls._
import oscar.cbls.core.computation.{CBLSIntVar, Store}
import oscar.cbls.core.objective.Objective
import oscar.cbls.lib.invariant.logic.Filter
import oscar.cbls.lib.invariant.minmax.MinConstArray
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.search.neighborhoods.{AssignNeighborhood, RandomizeNeighborhood}

import scala.language.postfixOps

object WarehouseLocationAndThen extends App{

  //the number of warehouses
  val W:Int = 15

  //the number of delivery points
  val D:Int = 150

  println("WarehouseLocation(W:" + W + ", D:" + D + ")")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (costForOpeningWarehouse,distanceCost) = WarehouseLocationGenerator.apply(W,D,0,100,3)

  val m = Store()

  val warehouseOpenArray = Array.tabulate(W)(l => CBLSIntVar(m, 0, 0 to 1, "warehouse_" + l + "_open"))
  val openWarehouses = Filter(warehouseOpenArray).setName("openWarehouses")

  val distanceToNearestOpenWarehouse = Array.tabulate(D)(d =>
    MinConstArray(distanceCost(d).map(_.toInt), openWarehouses, defaultCostForNoOpenWarehouse).setName("distance_for_delivery_" + d))

  val obj = Objective(Sum(distanceToNearestOpenWarehouse) + Sum(costForOpeningWarehouse, openWarehouses))

  m.close()

  val neighborhood = (AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse")
    exhaustBack (AssignNeighborhood(warehouseOpenArray,"1") andThen AssignNeighborhood(warehouseOpenArray,"2"))
    cutTail (100,0.1)
    orElse (RandomizeNeighborhood(warehouseOpenArray, () => W/5) maxMoves 2)
    saveBest obj restoreBestOnExhaust)

  neighborhood.verbose = 1
  neighborhood.doAllMoves(_ >= W+D, obj)

  println(openWarehouses)
}
