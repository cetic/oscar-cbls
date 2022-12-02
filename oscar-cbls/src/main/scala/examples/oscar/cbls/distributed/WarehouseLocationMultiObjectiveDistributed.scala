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

package examples.oscar.cbls.distributed

import oscar.cbls._
import oscar.cbls.algo.generator.WarehouseLocationGenerator
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.core.computation.Store
import oscar.cbls.core.distributed.Supervisor
import oscar.cbls.core.objective.Objective
import oscar.cbls.lib.search.combinators.distributed.DistributedBiObjectiveSearch
import oscar.cbls.lib.search.neighborhoods.SwapsNeighborhood
import oscar.cbls.util.Properties

import scala.language.postfixOps

object WarehouseLocationMultiObjectiveDistributed extends App {

  //the number of warehouses
  val W: Int = 500

  //the number of delivery points
  val D: Int = 500

  val problemName = "BiObjective WLP(W:" + W + ", D:" + D + ")"
  println(problemName)
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (costForOpeningWarehouse, distanceCost, _, _, warehouseToWarehouseDistances) =
    WarehouseLocationGenerator.problemWithPositions(W, D, 0, 1000, 3)

  //This is for demo purpose; to have a curve that is more readable on the output.
  // Opening zero warehouses and having operationCost = D*defaultCostForNoOpenWarehouse
  // is a relevant trade off point in the mathematical sense.
  costForOpeningWarehouse(0) = 0

  def createSearchStructures(): (Store, DistributedBiObjectiveSearch, SetValue) = {
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
    def kNearestClosedWarehouses(warehouse: Int, k: Int) =
      KSmallest.kFirst(k, closestWarehouses(warehouse), filter = (otherWarehouse) => warehouseOpenArray(otherWarehouse).newValue == 0)

    def kNearestOpenWarehouses(warehouse: Int, k: Int) =
      KSmallest.kFirst(k, closestWarehouses(warehouse), filter = (otherWarehouse) => warehouseOpenArray(otherWarehouse).newValue != 0)

    def swapsK(k: Int, openWarehousesToConsider: () => Iterable[Int] = openWarehouses) = SwapsNeighborhood(warehouseOpenArray,
      searchZone1 = openWarehousesToConsider,
      searchZone2 = () => (firstWareHouse, _) => kNearestClosedWarehouses(firstWareHouse, k),
      name = "Swap" + k + "Nearest",
      symmetryCanBeBrokenOnIndices = false)

    val paretoSearch = new DistributedBiObjectiveSearch(
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
      obj1Name = "operationCost",

      obj2 = constructionCost,
      obj2Name = "constructionCost",

      visuTitle = problemName,
      maxPoints = 100,
      verbose = true,
      visu = false)

    (m, paretoSearch,openWarehouses)
  }

  //supervisor side
  val (store, paretoSearch, openWarehouses) = createSearchStructures()
  val supervisor: Supervisor = Supervisor.startSupervisorAndActorSystem(paretoSearch)

  //create the workers
  for (i <- 0 until Supervisor.nbCores/2) {
    val (store2, search2,_) = createSearchStructures()
    supervisor.createLocalWorker(store2, search2)
  }

  //start the search, et the supervisor side
  val allSolutions =
    paretoSearch
      .paretoOptimize()
      .map({ case (obj1,obj,sol) =>
        sol.restoreDecisionVariables()
        val w = openWarehouses.value
        List(obj1,obj,w.size,w)
      })

  //shut down supervisor and all workers
  supervisor.shutdown()

  println(
    Properties.justifyLeftAny(
      List("operationCost","constructionCost","nbWarehouses" ,"open warehouses") :: allSolutions)
    .mkString("\n"))

}
