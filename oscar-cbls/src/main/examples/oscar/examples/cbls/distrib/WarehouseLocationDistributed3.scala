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
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.core.computation.Store
import oscar.cbls.core.distrib.Supervisor
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.Neighborhood
import oscar.cbls.lib.search.combinators.BestSlopeFirst
import oscar.cbls.lib.search.combinators.distributed.{DistributedFirst, DistributedRestartFromBest}
import oscar.examples.cbls.wlp.WarehouseLocationGenerator

import scala.collection.parallel.CollectionConverters.IterableIsParallelizable
import scala.collection.parallel.immutable.ParRange
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

object WarehouseLocationDistributed3 extends App{

  //the number of warehouses
  val W:Int = 2000

  //the number of delivery points
  val D:Int = 1000

  val nbWorker = 6

  println(s"WarehouseLocation(W:$W, D:$D)")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (_,distanceCost,_,_,warehouseToWarehouseDistances) =
    WarehouseLocationGenerator.problemWithPositions(W,D,0,100,3)

  val costForOpeningWarehouse = Array.fill(W)(1000L)

  println("created instance")

  def createSearchProcedure():(Store,Neighborhood,Objective,()=>Unit) = {

    val m = Store()

    val warehouseOpenArray = Array.tabulate(W)(l => CBLSIntVar(m, 0, 0 to 1, s"warehouse_${l}_open"))
    val openWarehouses = filter(warehouseOpenArray)

    val distanceToNearestOpenWarehouseLazy = Array.tabulate(D)(d =>
      minConstArrayValueWise(distanceCost(d).map(_.toInt), openWarehouses, defaultCostForNoOpenWarehouse))

    val obj = Objective(sum(distanceToNearestOpenWarehouseLazy) + sum(costForOpeningWarehouse, openWarehouses))

    m.close()

    //this is an array, that, for each warehouse, keeps the sorted closest warehouses in a lazy way.
    val closestWarehouses = Array.tabulate(W)(warehouse =>
      KSmallest.lazySort(
        Array.tabulate(W)(warehouse => warehouse),
        otherwarehouse => warehouseToWarehouseDistances(warehouse)(otherwarehouse)
      ))

    def kNearestClosedWarehouses(warehouse:Int,k:Int) = KSmallest.kFirst(k, closestWarehouses(warehouse), filter = (otherWarehouse) => warehouseOpenArray(otherWarehouse).newValue == 0)

    def swapsK(k:Int,openWarehouseTocConsider:()=>Iterable[Int] = openWarehouses) = swapsNeighborhood(warehouseOpenArray,
      searchZone1 = openWarehouseTocConsider,
      searchZone2 = () => (firstWareHouse,_) => kNearestClosedWarehouses(firstWareHouse,k),
      name = "Swap" + k + "Nearest",
      symmetryCanBeBrokenOnIndices = false)

    val neighborhood =
      new DistributedRestartFromBest(
        bestSlopeFirst(
          List(
            assignNeighborhood(warehouseOpenArray, name = "SwitchWarehouse"),
            swapsNeighborhood(warehouseOpenArray, name = "SwapWarehouses"),
            swapsK(20) guard(() => openWarehouses.value.size >= 5))),
        randomSwapNeighborhood(warehouseOpenArray,() => W/10),
        nbConsecutiveRestartWithoutImprovement = 10,
        visu = true)

    (m,neighborhood,obj,() => {println(openWarehouses)})
  }

  //main search; distributed combinators delegate to worker
  val (store,search,obj,finalPrint) = createSearchProcedure()
  val supervisor:Supervisor = Supervisor.startSupervisorAndActorSystem(store,search,verbose=false,tic=5.seconds)

  for (i <- (0 until nbWorker).par){
    //creating each worker, with its own model and search procedure (we do in in parallel)
    val (store2, search2, _, _) = createSearchProcedure()
    supervisor.createLocalWorker(store2, search2)
  }

  //now, run the main search, we set a maxMoves because distributed restart has a search loop
  search.maxMoves(1).doAllMoves(obj = obj)

  supervisor.shutdown()
  finalPrint()
}
