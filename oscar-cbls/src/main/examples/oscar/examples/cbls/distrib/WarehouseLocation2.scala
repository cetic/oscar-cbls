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

import oscar.cbls.{swapsNeighborhood, _}
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.core.computation.Store
import oscar.cbls.core.distrib.Supervisor
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.Neighborhood
import oscar.cbls.lib.search.combinators.{Atomic, DistributedBest, DistributedFirst}
import oscar.examples.cbls.wlp.WarehouseLocationGenerator

import scala.language.postfixOps

object WarehouseLocationDistributed2 extends App{

  //the number of warehouses
  val W:Int = 2000

  //the number of delivery points
  val D:Int = 1000

  println("WarehouseLocation(W:" + W + ", D:" + D + ")")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (_,distanceCost,_,_,warehouseToWarehouseDistances) = WarehouseLocationGenerator.problemWithPositions(W,D,0,100,3)

  val costForOpeningWarehouse = Array.fill(W)(1000L)

  println("created instance")

  def createSearchProcedure():(Store,Neighborhood,Objective, ()=>Unit) = {

    val m = Store()

    val warehouseOpenArray = Array.tabulate(W)(l => CBLSIntVar(m, 0, 0 to 1, "warehouse_" + l + "_open"))
    val openWarehouses = filter(warehouseOpenArray).setName("openWarehouses")


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

    //this procedure returns the k closest closed warehouses
    def kNearestClosedWarehouses(warehouse:Int,k:Int) = KSmallest.kFirst(k, closestWarehouses(warehouse), filter = (otherWarehouse) => warehouseOpenArray(otherWarehouse).newValue == 0)

    def swapsK(k:Int,openWarehouseTocConsider:()=>Iterable[Int] = openWarehouses,modulo:Int=0,shift:Int=0) =
      swapsNeighborhood(warehouseOpenArray,
        searchZone1 = if(modulo ==0) openWarehouseTocConsider else () => openWarehouseTocConsider().filter(_%modulo == shift),
        searchZone2 = () => (firstWareHouse,_) => kNearestClosedWarehouses(firstWareHouse,k),
        name = "SwapK" + k,// + (if(modulo ==0) "" else s"mod:$modulo,s:$shift"),
        symmetryCanBeBrokenOnIndices = false)

    def swaps(modulo:Int,shift:Int) = {
      val myRange = (0 until W/modulo).map(_*modulo+shift)
      swapsNeighborhood(warehouseOpenArray,searchZone1 = () => myRange, name = "SwapWarehouses")
    }

    //These neighborhoods are inefficient and slow; using multiple core is the wrong answer to inefficiency
    val neighborhood = (
      new DistributedFirst(
        Array(
          assignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
          swapsK(2,modulo=0,shift=0),
          swapsK(20,modulo=4,shift=0),
          swapsK(20,modulo=4,shift=1),
          swapsK(20,modulo=4,shift=2),
          swapsK(20,modulo=4,shift=3),
          swaps(modulo = 5,shift = 0),
          swaps(modulo = 5,shift = 1),
          swaps(modulo = 5,shift = 2),
          swaps(modulo = 5,shift = 3),
          swaps(modulo = 5,shift = 4)))
        onExhaustRestartAfter(randomSwapNeighborhood(warehouseOpenArray,() => W/10), 2, obj)
        onExhaustRestartAfter(randomizeNeighborhood(warehouseOpenArray, () => W/5), 2, obj))

    (m,neighborhood,obj,() => {println(openWarehouses)})
  }

  //supervisor side
  val (store,search,obj, finalPrint) = createSearchProcedure()

  import scala.concurrent.duration._

  val supervisor:Supervisor = Supervisor.startSupervisorAndActorSystem(store,search,tic = 500.millisecond,verbose = false)

  val nbWorker = 5
  
  for(_ <- (0 until nbWorker).par) {
    val (store2, search2, _, _) = createSearchProcedure()
    supervisor.createLocalWorker(store2,search2)
    search2.verbose = 2
  }

  val search2 = search.showObjectiveFunction(obj)
  search2.verbose = 1
  search2.doAllMoves(obj = obj)

  supervisor.shutdown()

  finalPrint()
}
