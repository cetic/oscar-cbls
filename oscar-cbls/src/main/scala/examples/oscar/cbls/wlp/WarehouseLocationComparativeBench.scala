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
import oscar.cbls.core.search.First
import oscar.cbls.lib.invariant.logic.Filter
import oscar.cbls.lib.invariant.minmax.MinConstArrayLazy
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, FastestFirst}
import oscar.cbls.lib.search.neighborhoods.{AssignMove, AssignNeighborhood, RandomizeNeighborhood, SwapsNeighborhood}
import oscar.cbls.util.Benchmark

object WarehouseLocationComparativeBench extends App{

  //the number of warehouses
  val W:Int = 1000

  //the number of delivery points
  val D:Int = 300

  println(s"ComparativeBench on WarehouseLocation(W:$W, D:$D)")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (costForOpeningWarehouse,distanceCost) = WarehouseLocationGenerator.apply(W,D,0,100,3)

  val m = Store()

  val warehouseOpenArray = Array.tabulate(W)(l => CBLSIntVar(m, 0, 0 to 1, s"warehouse_${l}_open"))
  val openWarehouses = Filter(warehouseOpenArray).setName("openWarehouses")

  val distanceToNearestOpenWarehouse = Array.tabulate(D)(d =>
    MinConstArrayLazy(distanceCost(d).map(_.toInt), openWarehouses, defaultCostForNoOpenWarehouse).setName(s"distance_for_delivery_$d"))

  val obj = Objective(Sum(distanceToNearestOpenWarehouse) + Sum(costForOpeningWarehouse, openWarehouses))

  m.close()
  /*
    val neighborhood = (Statistics(AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),true)
      exhaustBack Statistics(SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses"))
      orElse (RandomizeNeighborhood(warehouseOpenArray, W/5) maxMoves 2) saveBest obj restoreBestOnExhaust)

    neighborhood.verbose = 1

    neighborhood.doAllMoves(_>= W + D, obj)

    println(neighborhood.statistics)
  */
  val neighborhood1 = ()=>("exhaustBack",AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse")
    exhaustBack SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses")
    orElse (RandomizeNeighborhood(warehouseOpenArray, () => W/5) maxMoves 2) saveBest obj restoreBestOnExhaust)

  val neighborhood2 = ()=>("simulatedAnnealing", (AssignNeighborhood(
    warehouseOpenArray,
    "SwitchWarehouse",
    hotRestart = false,
    selectIndiceBehavior = First(randomized = true))
    .cauchyAnnealing(initialTemperature = 5, base = 10)
    //the two stop criterion here below can be used, although they are useless for small size example.
    //maxMoves W*50 withoutImprovementOver obj
    .cutTail(timePeriodInMilliSecond = 500,minRelativeImprovementByCut = 0.00001,minTimeBeforeFirstCutInMilliSecond=1000)
    saveBestAndRestoreOnExhaust obj))

  val neighborhood3 = ()=>("lateAcceptance", (AssignNeighborhood(
    warehouseOpenArray,
    selectIndiceBehavior = First(randomized = true),
    hotRestart = false,
    name = "SwitchWarehouse")
    .lateAcceptanceHillClimbing(20)
    saveBestAndRestoreOnExhaust obj))


  val neighborhood3b = ()=>("lateAcceptanceSwichAndSwap", (AssignNeighborhood(
    warehouseOpenArray,
    selectIndiceBehavior = First(randomized = true),
    hotRestart = false,
    name = "SwitchWarehouse")
    exhaustBack SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses")
    .lateAcceptanceHillClimbing(20)
    saveBestAndRestoreOnExhaust obj))

  val neighborhood4 = ()=>("best",AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse")
    best SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses")
    orElse (RandomizeNeighborhood(warehouseOpenArray, () => W/5) maxMoves 2) saveBest obj restoreBestOnExhaust)

  val neighborhoods = List(neighborhood1,neighborhood2,neighborhood3,neighborhood3b,neighborhood4)

  val a = Benchmark.benchToStringFull(obj,3,neighborhoods,1,verbose=1)

  println(a)
}