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

package examples.oscar.cbls.distrib

import examples.oscar.cbls.distrib.WarehouseLocationDistributed1.{createSearchProcedure, nbWorker, obj, search}
import examples.oscar.cbls.wlp.WarehouseLocationGenerator
import oscar.cbls._
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.core.computation.Store
import oscar.cbls.core.distrib.Supervisor
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.Neighborhood
import oscar.cbls.lib.search.combinators.distributed.DistributedBiObjectiveSearch
import oscar.cbls.lib.search.combinators.multiObjective.BiObjectiveSearch
import oscar.cbls.lib.search.neighborhoods.SwapsNeighborhood

import scala.collection.parallel.immutable.ParRange
import scala.language.postfixOps

object WarehouseLocationMultiObjectiveDistrib extends App {

  //the number of warehouses
  val W: Int = 500

  //the number of delivery points
  val D: Int = 500

  val problemName = "BiObjective WLP(W:" + W + ", D:" + D + ")"
  println(problemName)
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  def createSearchProcedure(): (Store, DistributedBiObjectiveSearch) = {
    val (costForOpeningWarehouse, distanceCost, _, _, warehouseToWarehouseDistances) =
      WarehouseLocationGenerator.problemWithPositions(W, D, 0, 1000, 3)

    //for(w <- 0 until W) costForOpeningWarehouse(w) = 100
    costForOpeningWarehouse(0) = 0 //This is for demo purpose; to have a curve that is more readable on the output.
    val m = Store()

    val warehouseOpenArray = Array.tabulate(W)(l => CBLSIntVar(m, 0, 0 to 1, "warehouse_" + l + "_open"))
    val openWarehouses = filter(warehouseOpenArray).setName("openWarehouses")

    val distanceToNearestOpenWarehouseLazy = Array.tabulate(D)(d =>
      minConstArrayValueWise(distanceCost(d).map(_.toInt), openWarehouses, defaultCostForNoOpenWarehouse))

    //obj1
    val operationCost = Objective(sum(distanceToNearestOpenWarehouseLazy))
    //obj2
    val constructionCost = Objective(sum(costForOpeningWarehouse, openWarehouses))

    m.close()

    val closestWarehouses = Array.tabulate(W)(warehouse =>
      KSmallest.lazySort(
        Array.tabulate(W)(warehouse => warehouse),
        otherwarehouse => warehouseToWarehouseDistances(warehouse)(otherwarehouse)
      ))

    //this procedure returns the k closest closed warehouses
    def kNearestClosedWarehouses(warehouse: Int, k: Int) = KSmallest.kFirst(k, closestWarehouses(warehouse), filter = (otherWarehouse) => warehouseOpenArray(otherWarehouse).newValue == 0)

    def kNearestOpenWarehouses(warehouse: Int, k: Int) = KSmallest.kFirst(k, closestWarehouses(warehouse), filter = (otherWarehouse) => warehouseOpenArray(otherWarehouse).newValue != 0)

    def swapsK(k: Int, openWarehousesToConsider: () => Iterable[Int] = openWarehouses) = SwapsNeighborhood(warehouseOpenArray,
      searchZone1 = openWarehousesToConsider,
      searchZone2 = () => (firstWareHouse, _) => kNearestClosedWarehouses(firstWareHouse, k),
      name = "Swap" + k + "Nearest",
      symmetryCanBeBrokenOnIndices = false)

    def paretoSearch() = new DistributedBiObjectiveSearch(
      minObj1Neighborhood = ()=>bestSlopeFirst(
        List(
          assignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
          swapsK(10) exhaust swapsK(20),
          swapsK(5) dynAndThen (swapMove => swapsK(5, () => kNearestOpenWarehouses(swapMove.idI, 4).filter(_ >= swapMove.idI)))
        )).onExhaustRestartAfter(
        randomizeNeighborhood(
          warehouseOpenArray, searchZone = openWarehouses, degree = () => openWarehouses.value.size/10 max 5, name = "smallRandomize")
          acceptAllButStrongViolation,
        5,
        operationCost),
      minObj2Neighborhood = Some(() => bestSlopeFirst(
        List(
          assignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
          swapsK(10) exhaust swapsK(20),
          swapsK(5) dynAndThen (swapMove => swapsK(5, () => kNearestOpenWarehouses(swapMove.idI, 4).filter(_ >= swapMove.idI)))
        ))),
      obj1 = operationCost,
      obj2 = constructionCost,
      visu = true)

    (m, paretoSearch())
  }

  //supervisor side
  val (store, paretoSearch) = createSearchProcedure()

  val supervisor: Supervisor = Supervisor.startSupervisorAndActorSystem(paretoSearch,verbose = false)

  //This is a bit stupid: start the search while workers are not instantiated yet, but it is possible
  for (i <- 0 until 2){ //Supervisor.nbCores/2) {
    val (store2, search2) = createSearchProcedure()
    supervisor.createLocalWorker(store2, search2)
  }

  val allSolutions = paretoSearch.paretoOptimize()
  supervisor.shutdown()

  println("solution:\n\t" +allSolutions.map(s => ("" + s._1 + "," +s._2)).mkString("\n\t"))

}
