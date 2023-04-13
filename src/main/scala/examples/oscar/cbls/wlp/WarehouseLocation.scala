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

package examples.oscar.cbls.wlp

import oscar.cbls._
import oscar.cbls.algo.generator.WarehouseLocationGenerator

import scala.language.postfixOps
import scala.util.Random

object WarehouseLocation extends App{

  //the number of warehouses
  val W:Int = 100

  //the number of delivery points
  val D:Int = 500

  println(s"WarehouseLocation(W:$W, D:$D)")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (costForOpeningWarehouse,distanceCost) = WarehouseLocationGenerator.apply(W,D,0,100,3)

  val m = Store()

  val warehouseOpenArray = Array.tabulate(W)(l => CBLSIntVar(m, 0, 0 to 1, s"warehouse_${l}_open"))
  val openWarehouses = filter(warehouseOpenArray).setName("openWarehouses")

  val distanceToNearestOpenWarehouseLazy = Array.tabulate(D)(d =>
    minConstArrayValueWise(distanceCost(d).map(_.toInt), openWarehouses, defaultCostForNoOpenWarehouse))

  val obj = Objective(sum(distanceToNearestOpenWarehouseLazy) + sum(costForOpeningWarehouse, openWarehouses))

  m.close()


  val neighborhood =(
    bestSlopeFirst(
      List(
        assignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
        swapsNeighborhood(warehouseOpenArray, "SwapWarehouses")),refresh = W/10)
      .onExhaustRestartAfterJump({
        for (_ <- 0 to (5 min openWarehouses.value.size/10)){
          val open = openWarehouses.value
          val r = Random.nextInt(open.size)
          warehouseOpenArray(r) := 0
        }}, name="smallRandomize", minRestarts = 10, obj = obj)
      .onExhaustRestartAfter(randomizeNeighborhood(warehouseOpenArray, () => W/2, name="bigRandomize"), 2, obj))
    .showObjectiveFunction(obj)

  neighborhood.verbose = 1

  neighborhood.doAllMoves(obj=obj)

  println(openWarehouses)

}
