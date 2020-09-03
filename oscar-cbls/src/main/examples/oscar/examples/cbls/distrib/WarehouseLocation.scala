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
import oscar.cbls.core.distrib.Supervisor
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.Neighborhood
import oscar.cbls.lib.search.combinators.{Atomic, DistributedBest, DistributedFirst}
import oscar.examples.cbls.wlp.WarehouseLocationGenerator

import scala.language.postfixOps

object WarehouseLocationDistributed extends App{

  //the number of warehouses
  val W:Int = 2000

  //the number of delivery points
  val D:Int = 1000

  println("WarehouseLocation(W:" + W + ", D:" + D + ")")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (_,distanceCost) = WarehouseLocationGenerator.apply(W,D,0,100,3)

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

    //These neighborhoods are inefficient and slow; using multiple core is the wrong answer to inefficiency
    val neighborhood = (
      new DistributedFirst(
        Array(
          assignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
          swapsNeighborhood(warehouseOpenArray,searchZone1 = {val range = (0 until W/4).map(_*4    ); () => range}, name = "SwapWarehouses1"),
          swapsNeighborhood(warehouseOpenArray,searchZone1 = {val range = (0 until W/4).map(_*4 + 1); () => range}, name = "SwapWarehouses2"),
          swapsNeighborhood(warehouseOpenArray,searchZone1 = {val range = (0 until W/4).map(_*4 + 2); () => range}, name = "SwapWarehouses3"),
          swapsNeighborhood(warehouseOpenArray,searchZone1 = {val range = (0 until W/4).map(_*4 + 3); () => range}, name = "SwapWarehouses4")))
      onExhaustRestartAfter(randomSwapNeighborhood(warehouseOpenArray,W/10), 2, obj)
      onExhaustRestartAfter(randomizeNeighborhood(warehouseOpenArray, () => W/5), 2, obj))

    //improvament?: how to get feedback on the go from remote worker in case of Atomic? ie: console and obj?
    //idea: propoze a remoteAtomic with dedicated API?
    //NB: this is just for verbosities so not very relevant indeed.
    //improvement2: how to sent only the delta on the model instead of the model itself in order to spare on model loading nd serialization?
    //improvement3: distributedRestart
    //improvement4: also work with the main thread?
    //improvement5: remote
    //improvement6: unified delegate & worker action for different purposes

    val x = 10
    val neighborhood2 =
      new DistributedBest(
        Array.fill(x) (Atomic(
          assignNeighborhood(warehouseOpenArray, "SwitchWarehouse")
            exhaustBack swapsNeighborhood(warehouseOpenArray, name = "SwapWarehouses4")
            onExhaustRestartAfter(randomSwapNeighborhood(warehouseOpenArray,W/10), 2, obj)
            onExhaustRestartAfter(randomizeNeighborhood(warehouseOpenArray, () => W/5), 2, obj),
          shouldStop = _ => false, aggregateIntoSingleMove = true))) maxMoves 1

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
