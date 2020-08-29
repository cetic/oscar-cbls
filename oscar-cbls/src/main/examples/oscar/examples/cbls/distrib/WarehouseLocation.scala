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

package oscar.examples.cbls.distrib

import oscar.cbls._
import oscar.cbls.core.computation.Store
import oscar.cbls.core.distrib.{Supervisor, WorkerActor}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.Neighborhood
import oscar.cbls.lib.search.combinators.{DistributedFirst, Remote}

import scala.language.postfixOps
import oscar.examples.cbls.wlp.WarehouseLocationGenerator

object WarehouseLocationDistributed extends App{

  //the number of warehouses
  val W:Int = 1000

  //the number of delivery points
  val D:Int = 300

  println("WarehouseLocation(W:" + W + ", D:" + D + ")")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (costForOpeningWarehouse,distanceCost) = WarehouseLocationGenerator.apply(W,D,0,100,3)

  def createSearchProcedure():(Store,Neighborhood,Objective, ()=>Unit) = {

    val m = Store()

    val warehouseOpenArray = Array.tabulate(W)(l => CBLSIntVar(m, 0, 0 to 1, "warehouse_" + l + "_open"))
    val openWarehouses = filter(warehouseOpenArray).setName("openWarehouses")

    val distanceToNearestOpenWarehouseLazy = Array.tabulate(D)(d =>
      minConstArrayValueWise(distanceCost(d).map(_.toInt), openWarehouses, defaultCostForNoOpenWarehouse))

    val obj = Objective(sum(distanceToNearestOpenWarehouseLazy) + sum(costForOpeningWarehouse, openWarehouses))

    m.close()

    val neighborhood = (new DistributedFirst(
      Array(
        assignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
        swapsNeighborhood(warehouseOpenArray,searchZone1=() => 0 until W/4, name = "SwapWarehouses1"),
        swapsNeighborhood(warehouseOpenArray,searchZone1=() => W/4 until W/2, name = "SwapWarehouses2"),
        swapsNeighborhood(warehouseOpenArray,searchZone1=() => W/2 until W-W/4, name = "SwapWarehouses3"),
        swapsNeighborhood(warehouseOpenArray,searchZone1=() => W-W/4 until W, name = "SwapWarehouses4"))
    )

      onExhaustRestartAfter(randomSwapNeighborhood(warehouseOpenArray,W/10), 2, obj)
      onExhaustRestartAfter(randomizeNeighborhood(warehouseOpenArray, () => W/5), 2, obj)

      )

    neighborhood.verbose = 1

    (m,neighborhood,obj,() => {println(openWarehouses)})
  }

  //supervisor side
  val (store,search,obj, finalPrint) = createSearchProcedure()

  import scala.concurrent.duration._

  val supervisor:Supervisor = Supervisor.startSupervisorAndActorSystem(store,search,tic = 500.millisecond,verbose = false)

  val nbWorker = 2
  for(workerID <- (0 until nbWorker).par) {
    //worker side
    val (store2, search2, _, _) = createSearchProcedure()
    supervisor.createLocalWorker(store2,search2)
  }

  search.doAllMoves(obj = obj)
  supervisor.shutdown()

  finalPrint()
}
