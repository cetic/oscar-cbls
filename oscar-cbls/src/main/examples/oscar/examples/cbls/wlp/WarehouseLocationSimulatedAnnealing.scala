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
import oscar.cbls.core.search.{Best, First}
import oscar.cbls.lib.invariant.logic.{Filter, SelectLESetQueue}
import oscar.cbls.lib.invariant.minmax.MinConstArray
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.search.neighborhoods.{AssignMove, AssignNeighborhood}

import scala.language.postfixOps

/**
  * this is a WarehouseLocation problem with a Tabu.
  * the purpose is to illustrate how standard neighborhoods can be tuned to encompass
  * additional behaviors. Here, we restrict a neighborhood to a specific set of variables that not tabu
  * this set of variables is maintained through invariants
  */
object WarehouseLocationSimulatedAnnealing extends App{

  //the number of warehouses
  val W:Int = 150

  //the number of delivery points
  val D:Int = 150

  println("WarehouseLocationSimulatedAnnealing(W:" + W + ", D:" + D + ")")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (costForOpeningWarehouse,distanceCost) = WarehouseLocationGenerator.apply(W,D,0,100,3)

  val m = Store()

  val warehouseOpenArray = Array.tabulate(W)(w => CBLSIntVar(m, 0, 0 to 1, "warehouse_" + w + "_open"))

  val openWarehouses = Filter(warehouseOpenArray).setName("openWarehouses")

  val distanceToNearestOpenWarehouse = Array.tabulate(D)(d =>
    minConstArrayValueWise(distanceCost(d), openWarehouses, defaultCostForNoOpenWarehouse)
      .setName("distance_for_delivery_" + d))

  val obj = Objective(Sum(distanceToNearestOpenWarehouse) + Sum(costForOpeningWarehouse, openWarehouses))

  m.close()
  
  val neighborhoodSA = (AssignNeighborhood(
    warehouseOpenArray,
    "SwitchWarehouse",
    hotRestart = false,
    selectIndiceBehavior = First(randomized = true))

    cauchyAnnealing(initialTemperature = 5, base = 10)
    //the two stop criterion here below can be used, although they are useless for small size example.
    //maxMoves W*50 withoutImprovementOver obj
    cutTail(timePeriodInMilliSecond = 500,minRelativeImprovementByCut = 0.00001,minTimeBeforeFirstCutInMilliSecond=1000)

    saveBestAndRestoreOnExhaust obj
    showObjectiveFunction obj)

  neighborhoodSA.verbose = 2

  //all moves are accepted because the neighborhood returns the best found move, and tabu might degrade obj.
  neighborhoodSA.doAllMoves(obj=obj)

  println(openWarehouses)
}
