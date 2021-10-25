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
import oscar.cbls.core.search.{Best, Neighborhood}
import oscar.cbls.lib.invariant.logic.Filter
import oscar.cbls.lib.invariant.minmax.MinConstArrayValueWise
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, EjectionChains, Mu, Profile}
import oscar.cbls.lib.search.neighborhoods._
import oscar.cbls.util.{Demo, StopWatch}
import oscar.cbls.visual.SingleFrameWindow
import oscar.cbls.visual.wlp.WareHouseLocationMap

import scala.language.postfixOps

object WareHouseLocationEjectionChain extends App with StopWatch{

  //the number of warehouses
  val W:Int = 300

  //the number of delivery points
  val D:Int = 1000

  val displayDelay = 100

  println(s"WarehouseLocation(W:$W, D:$D)")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (costForOpeningWarehouse1,distanceCost,warehousePositions,deliveryPositions,warehouseToWarehouseDistances) =
    WarehouseLocationGenerator.problemWithPositions(W,D,0,1000,3)

  val costForOpeningWarehouse =  Array.fill[Long](W)(1000)

  val m = Store() //checker = Some(new ErrorChecker()))

  val warehouseOpenArray = Array.tabulate(W)(l => CBLSIntVar(m, 0, 0 to 1, s"warehouse_${l}_open"))
  val openWarehouses = Filter(warehouseOpenArray).setName("openWarehouses")

  val distanceToNearestOpenWarehouseLazy = Array.tabulate(D)(d =>
    new MinConstArrayValueWise(distanceCost(d).map(_.toInt), openWarehouses, defaultCostForNoOpenWarehouse,maxDiameter = 2))

  val obj = Objective(Sum(distanceToNearestOpenWarehouseLazy) + Sum(costForOpeningWarehouse, openWarehouses))

  m.close()

  val visual = new WareHouseLocationMap(deliveryPositions,warehousePositions,distanceCost,costForOpeningWarehouse)

  SingleFrameWindow.show(visual,"Uncapacitated Warehouse Location Problem",width = 960,height = 960)

  var bestObj = Int.MaxValue

  //this is an array, that, for each warehouse, keeps the closest warehouses in a lazy way.
  val closestWarehouses = Array.tabulate(W)(warehouse =>
    KSmallest.lazySort(
      Array.tabulate(W)(warehouse => warehouse),
      otherwarehouse => warehouseToWarehouseDistances(warehouse)(otherwarehouse)
    ))

  //this procedure returns the k closest closed warehouses
  def kNearestClosedWarehouses(warehouse:Int,k:Int) = KSmallest.kFirst(k, closestWarehouses(warehouse), filter = otherWarehouse => warehouseOpenArray(otherWarehouse).newValue == 0)
  //this procedure returns the k closest open warehouses
  def kNearestOpenWarehouses(warehouse:Int,k:Int) = KSmallest.kFirst(k, closestWarehouses(warehouse), filter = otherWarehouse => warehouseOpenArray(otherWarehouse).newValue != 0)
  def kNearestdWarehouses(warehouse:Int,k:Int) = KSmallest.kFirst(k, closestWarehouses(warehouse))

  def muLine(depth:Int,kOpen:Int,kClosed:Int ) = Mu[AssignMove](
    AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
    (assignList:List[AssignMove]) =>
    {
      val lastChangedWarehouse = assignList.head.id
      val setTo = assignList.head.value
      val otherWarehouses = if(setTo == 0) kNearestClosedWarehouses(lastChangedWarehouse,kClosed) else kNearestOpenWarehouses(lastChangedWarehouse,kOpen)
      Some(AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse",searchZone = () => otherWarehouses,hotRestart = false))
    },
    maxDepth = depth,
    intermediaryStops = true)

  def muStar(width:Int,kOpen:Int,kClosed:Int ) = Mu[AssignMove](
    AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
    (assignList:List[AssignMove]) =>
    {
      val lastChangedWarehouse = assignList.last.id
      val setTo = assignList.head.value
      val otherWarehouses = if(setTo == 0) kNearestClosedWarehouses(lastChangedWarehouse,kClosed) else kNearestOpenWarehouses(lastChangedWarehouse,kOpen)
      Some(AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse",searchZone = () => otherWarehouses,hotRestart = false))
    },
    maxDepth = width,
    intermediaryStops = true)

  def swapsK(k:Int, openWarehousesToConsider:()=>Iterable[Int] = openWarehouses) = SwapsNeighborhood(warehouseOpenArray,
    searchZone1 = openWarehousesToConsider,
    searchZone2 = () => (firstWareHouse,_) => kNearestClosedWarehouses(firstWareHouse,k),
    name = s"Swap${k}Nearest",
    symmetryCanBeBrokenOnIndices = false)

  def doubleSwap(k:Int) = (swapsK(k) dynAndThen((firstSwap:SwapMove) => swapsK(k,() => kNearestOpenWarehouses(firstSwap.idI,k)))) name "DoubleSwap"

  var lastDisplay = this.getWatch

  def ejection(maxLength:Int, kOpen:Int, kClosed:Int):Neighborhood =(
    AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse")
      dynAndThen(initMove =>
      EjectionChains(
        initMove,
        nextMove = {
          case assign: AssignMove =>
            val setTo = assign.value
            val lastChangedWarehouse = assign.id
            val otherWarehouses = if (setTo == 0) kNearestClosedWarehouses(lastChangedWarehouse, kClosed) else kNearestOpenWarehouses(lastChangedWarehouse, kOpen)
            AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse", searchZone = () => otherWarehouses, selectIndiceBehavior = Best(), hotRestart = false)
        },
        shouldStop = _ >= maxLength)) name s"EjectionChain($maxLength,$kOpen,$kClosed")

  val neighborhood =(
    BestSlopeFirst(
      List(
        Profile(AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse")),
        Profile(swapsK(20) guard(() => openWarehouses.value.size >= 5)), //we set a minimal size because the KNearest is very expensive if the size is small
        Profile(SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses") guard(() => openWarehouses.value.size >= 5)),
        Profile(ejection(maxLength = 10,kOpen= 5, kClosed = 20))
      ),refresh = W/10)
      onExhaustRestartAfter(randomSwapNeighborhood(warehouseOpenArray, () => openWarehouses.value.size/5,name="smallRandom"), 2, obj)
      onExhaustRestartAfter(RandomizeNeighborhood(warehouseOpenArray, () => W/5,name="bigRandom"), 1, obj)
    ) exhaust Profile(muLine(4,3,15)) afterMove(
    if(this.getWatch > lastDisplay + displayDelay) {
      visual.redraw(openWarehouses.value)
      lastDisplay = this.getWatch
    }
    ) showObjectiveFunction obj

  neighborhood.verbose = 2

  Demo.startUpPause()

  neighborhood.doAllMoves(obj=obj)

  visual.redraw(openWarehouses.value,boldChanges = false)

  println(neighborhood.profilingStatistics)

  println(openWarehouses)
}

