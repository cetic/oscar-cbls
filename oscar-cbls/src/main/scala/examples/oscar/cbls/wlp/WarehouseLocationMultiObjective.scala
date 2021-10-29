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
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.core.search.Neighborhood
import oscar.cbls.lib.search.combinators.multiObjective.BiObjectiveSearch
import oscar.cbls.lib.search.neighborhoods.SwapsNeighborhood

import scala.language.postfixOps

object WarehouseLocationMultiObjective extends App {

  //the number of warehouses
  val W: Int = 500

  //the number of delivery points
  val D: Int = 500

  val problemName = "BiObjective WLP(W:" + W + ", D:" + D + ")"
  println(problemName)
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (costForOpeningWarehouse,distanceCost,_,_,warehouseToWarehouseDistances) =
    WarehouseLocationGenerator.problemWithPositions(W,D,0,1000,3)

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
  def kNearestClosedWarehouses(warehouse:Int,k:Int) = KSmallest.kFirst(k, closestWarehouses(warehouse), filter = (otherWarehouse) => warehouseOpenArray(otherWarehouse).newValue == 0)
  def kNearestOpenWarehouses(warehouse:Int,k:Int) = KSmallest.kFirst(k, closestWarehouses(warehouse), filter = (otherWarehouse) => warehouseOpenArray(otherWarehouse).newValue != 0)

  def swapsK(k:Int,openWarehousesToConsider:()=>Iterable[Int] = openWarehouses) = SwapsNeighborhood(warehouseOpenArray,
    searchZone1 = openWarehousesToConsider,
    searchZone2 = () => (firstWareHouse,_) => kNearestClosedWarehouses(firstWareHouse,k),
    name = "Swap" + k + "Nearest",
    symmetryCanBeBrokenOnIndices = false)

  println("extreme solution search")
  assignNeighborhood(warehouseOpenArray, name = "SwitchWarehouse").doAllMoves(obj = constructionCost)
  val firstConstructionCost = constructionCost.value
  assignNeighborhood(warehouseOpenArray, name = "SwitchWarehouse").doAllMoves(
    obj = new CascadingObjective(() => (constructionCost.value != firstConstructionCost), operationCost))
  println(openWarehouses)
  println("done")

  println("operationCost:" + operationCost.value)
  println("constructionCost:" + constructionCost.value)
  println("openWarehouses:" + openWarehouses.value)

  val globalMaxObj1: Long = operationCost.value
  val globalMinObj2: Long = constructionCost.value
  val solutionAtMax1Mn2: Solution = m.solution()

  val paretoSearch = new BiObjectiveSearch(
    globalMaxObj1,
    globalMinObj2,
    solutionAtMax1Mn2,
    optimize = {
      case (maxConstructionCost, solution) => {
        //println("search new tradeoff point with maxConstructionCost:" + maxConstructionCost)
        solution.restoreDecisionVariables()
        require(constructionCost.value < maxConstructionCost, "initial solution not acceptable")

        val obj2 = new CascadingObjective(() => (constructionCost.value >= maxConstructionCost), operationCost)

        def neighborhood = bestSlopeFirst(
          List(
            assignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
            swapsK(10) exhaust swapsK(20),
            swapsK(5) dynAndThen (swapMove => swapsK(5,() => kNearestOpenWarehouses(swapMove.idI,4).filter(_ >= swapMove.idI)))
            //TODO: add split and merge if warehouses have different construction costs
          ))

        val search1 = (neighborhood
          .onExhaustRestartAfter(randomizeNeighborhood(warehouseOpenArray, () => (W / 10) max 5, name = "smallRandomize", acceptanceChecking = Some(5)) acceptAllButStrongViolation, 5, operationCost)
          .onExhaustRestartAfter(randomizeNeighborhood(warehouseOpenArray, () => W/2, name = "bigRandomize", acceptanceChecking = Some(20)) acceptAllButStrongViolation, 2, operationCost))
        search1.verbose = 0
        search1.doAllMoves(obj = obj2)

        val foundOperationCost = operationCost.value

        val search2:Neighborhood = neighborhood

        val obj3 = new CascadingObjective(() => (operationCost.value > foundOperationCost), constructionCost)
        search2.verbose = 0
        search2.doAllMoves(obj = obj3)

        Some((foundOperationCost, constructionCost.value,m.solution()))
      }
    },
    maxPoints = 200,
    verbose = true,
    visu = true,
    visuTitle = problemName,
    obj1Name = "operationCost",
    obj2Name = "constructionCost",
    filterSquare = {case (obj1,maxOBj1,obj2,minOBj2) => minOBj2 < obj2 - 26},
    stayAlive = true
  )

  val allSolutions = paretoSearch.paretoOptimize()

  println(allSolutions.map({case (obj1,obj2,sol) => {
    sol.restoreDecisionVariables()
    ("" + obj1 + ";" + obj2 + ";" + openWarehouses.value)
  }}).mkString("\n"))

}
