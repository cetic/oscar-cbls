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
import oscar.cbls.lib.invariant.logic.Filter
import oscar.cbls.lib.invariant.minmax.MinConstArrayLazy
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, FastestFirst, LearningRandom}
import oscar.cbls.lib.search.neighborhoods.{AssignMove, AssignNeighborhood, RandomizeNeighborhood, SwapsNeighborhood}
import oscar.cbls.util.Benchmark

object WarehouseLocationComparativeBench extends App{

  //the number of warehouses
  val W:Int = 100

  //the number of delivery points
  val D:Int = 150

  println(s"WarehouseLocation(W:$W, D:$D)")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (costForOpeningWarehouse,distanceCost) = WarehouseLocationGenerator.apply(W,D,0,100,3)

  val m = Store()

  val warehouseOpenArray = Array.tabulate(W)(l => CBLSIntVar(m, 0, 0 to 1, "warehouse_${l}_open"))
  val openWarehouses = Filter(warehouseOpenArray).setName("openWarehouses")

  val distanceToNearestOpenWarehouse = Array.tabulate(D)(d =>
    MinConstArrayLazy(distanceCost(d).map(_.toInt), openWarehouses, defaultCostForNoOpenWarehouse).setName("distance_for_delivery_$d"))

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

  val neighborhood2 = ()=>("random",
    random(
      AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
      SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses"))
    orElse (RandomizeNeighborhood(warehouseOpenArray, () => W/5) maxMoves 2) saveBest obj restoreBestOnExhaust)

  val neighborhood3 = ()=>("LearningRandom", BestSlopeFirst(List(AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
    SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses")))
    orElse (RandomizeNeighborhood(warehouseOpenArray, () => W/5) maxMoves 2) saveBest obj restoreBestOnExhaust)

  val neighborhood4 = ()=>("onlySwitch",AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse")
    orElse (RandomizeNeighborhood(warehouseOpenArray, () => W/5) maxMoves 2) saveBest obj restoreBestOnExhaust)

  val neighborhood5 = ()=>("simulatedAnnealing",AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse")
    metropolis() maxMoves W/2 withoutImprovementOver obj saveBest obj restoreBestOnExhaust)

  val neighborhood6 = ()=>("roundRobin",AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse")
    roundRobin SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses") step 1
    orElse (RandomizeNeighborhood(warehouseOpenArray, () => W/5) maxMoves 2) saveBest obj restoreBestOnExhaust)

  val neighborhood7 = ()=>("best",AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse")
    best SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses")
    orElse (RandomizeNeighborhood(warehouseOpenArray, () => W/5) maxMoves 2) saveBest obj restoreBestOnExhaust)

  val neighborhood8 = ()=>("DynAndThen",
    AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse1") dynAndThen
      {case AssignMove(variable,value,id,_,_) => AssignNeighborhood(warehouseOpenArray, searchZone = (()=> (id+1 until W)),name="SwitchWarehouse2")}
    orElse (RandomizeNeighborhood(warehouseOpenArray, () => W/5) maxMoves 2) saveBest obj restoreBestOnExhaust)

  val neighborhood9 = ()=>("AndThen",
    AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse1") andThen AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse2")
      orElse (RandomizeNeighborhood(warehouseOpenArray, () => W/5) maxMoves 2) saveBest obj restoreBestOnExhaust)

  val neighborhood10 = ()=>("BestSlope",
    BestSlopeFirst(List(
      AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
        SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses")),10,9)
      orElse (RandomizeNeighborhood(warehouseOpenArray, () => W/5) maxMoves 2) saveBest obj restoreBestOnExhaust)

  val neighborhood11 = ()=>("Fastest",
    FastestFirst(List(
      AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
      SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses")),10,9)
      orElse (RandomizeNeighborhood(warehouseOpenArray, () => W/5) maxMoves 2) saveBest obj restoreBestOnExhaust)

  val neighborhoods = List(neighborhood1,neighborhood2,neighborhood4,neighborhood7,neighborhood10,neighborhood11)

  val a = Benchmark.benchToStringFull(obj,5,neighborhoods,1)

  println(a)
}
