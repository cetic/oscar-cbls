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

package oscar.examples.cbls.wlp

import oscar.cbls._
import oscar.cbls.core.search.{Best, Neighborhood}
import oscar.cbls.lib.search.BiObjectiveSearch

import scala.language.postfixOps

object WarehouseLocationMultiObjective extends App {

  //the number of warehouses
  val W: Int = 100

  //the number of delivery points
  val D: Int = 300

  println("WarehouseLocation(W:" + W + ", D:" + D + ")")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (costForOpeningWarehouse, distanceCost) = WarehouseLocationGenerator.apply(W, D, 0, 100, 3)

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

  def neighborhood = {
    (
      bestSlopeFirst(
        List(
          assignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
          swapsNeighborhood(warehouseOpenArray, "SwapWarehouses")), refresh = W / 10)
        onExhaustRestartAfter(randomizeNeighborhood(warehouseOpenArray, () => W / 10, name = "smallRandomize"), 1, operationCost)
        onExhaustRestartAfter(randomizeNeighborhood(warehouseOpenArray, () => W / 2, name = "bigRandomize"), 1, operationCost))
  }

  println("extreme solution search")
  //minimize constructionCost only for initial solution
  assignNeighborhood(warehouseOpenArray, selectIndiceBehavior = Best(), name = "SwitchWarehouse").doImprovingMove(constructionCost)
  println("done")

  println("operationCost:" + operationCost.value)
  println("constructionCost:" + constructionCost.value)
  println("openWarehouses:" + openWarehouses.value)

  val globalMaxObj1: Long = operationCost.value*100
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
        neighborhood.doAllMoves(obj = obj2)

        //println("operationCost:" + operationCost.value)
        //println("constructionCost:" + constructionCost.value)
        //println("openWarehouses:" + openWarehouses.value)

        Some((operationCost.value, constructionCost.value,m.solution()))
      }
    },
    stopSurface = 10000,
    maxPoints = 100,
    verbose = true
  )

  val allSolutions = paretoSearch.paretoOptimize()

  println(allSolutions.map({case (obj1,obj2,sol) => {
    sol.restoreDecisionVariables()
    ("" + obj1 + ";" + obj2 + ";" + openWarehouses.value)
  }}).mkString("\n"))

}
