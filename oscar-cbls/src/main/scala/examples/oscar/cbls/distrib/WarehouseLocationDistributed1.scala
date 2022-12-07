package examples.oscar.cbls.distrib

import oscar.cbls._
import oscar.cbls.algo.generator.WarehouseLocationGenerator
import oscar.cbls.core.computation.Store
import oscar.cbls.core.distrib.Supervisor
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.Neighborhood
import oscar.cbls.lib.search.combinators.distributed.{DistributedBest, DistributedBestSlopeFirst, DistributedFirst}

import scala.collection.parallel.immutable.ParRange
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

object WarehouseLocationDistributed1 extends App {

  //the number of warehouses
  val W: Int = 2000

  //the number of delivery points
  val D: Int = 1000

  val nbWorker = 6

  println(s"WarehouseLocation(W:$W, D:$D)")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (_, distanceCost) = WarehouseLocationGenerator.apply(W, D, 0, 100, 3)

  val costForOpeningWarehouse = Array.fill(W)(10L)

  println("created instance")

  def createSearchProcedure(): (Store, Neighborhood, Objective, () => Unit) = {

    val m = Store()

    val warehouseOpenArray = Array.tabulate(W)(l => CBLSIntVar(m, 0, 0 to 1, s"warehouse_${l}_open"))
    val openWarehouses = filter(warehouseOpenArray).setName("openWarehouses")

    val distanceToNearestOpenWarehouseLazy = Array.tabulate(D)(d =>
      minConstArrayValueWise(distanceCost(d).map(_.toInt), openWarehouses, defaultCostForNoOpenWarehouse))

    val obj = Objective(sum(distanceToNearestOpenWarehouseLazy) + sum(costForOpeningWarehouse, openWarehouses))

    m.close()

    val divideSwap = 40
    def swapShifted(shift:Int,modulo:Int):Neighborhood=swapsNeighborhood(warehouseOpenArray, searchZone1 = {
      val range = (0 until W / modulo).map(_ * modulo + shift); () => range
    }, name = "SwapWarehouses" + shift)

    //These neighborhoods are inefficient and slow; using multiple core is the wrong answer to inefficiency
    val neighborhood = (
      new DistributedBestSlopeFirst(
        Array(
          assignNeighborhood(warehouseOpenArray, "SwitchWarehouse")) ++
          Array.tabulate(divideSwap)(x => swapShifted(x,divideSwap):Neighborhood))
        onExhaustRestartAfter(randomSwapNeighborhood(warehouseOpenArray, () => W / 10), 2, obj)
        onExhaustRestartAfter(randomizeNeighborhood(warehouseOpenArray, () => W / 5), 2, obj)
      )
    (m, neighborhood, obj, () => {
      println(openWarehouses)
    })
  }

  //supervisor side
  val (store, search, obj, finalPrint) = createSearchProcedure()

  val supervisor: Supervisor = Supervisor.startSupervisorAndActorSystem(search,verbose = false)

  //This is a bit stupid: start the search while workers are not instantiated yet, but it is possible
  for (i <- ParRange(0, nbWorker, 1, inclusive = true)) {
    if(i == 0){
      val search2 = search.showObjectiveFunction(obj)
      search2.verbose = 2
      search2.doAllMoves(obj = obj)
    }else {
      val (store2, search2, _, _) = createSearchProcedure()
      supervisor.createLocalWorker(store2, search2)
    }
  }

  println(search.profilingStatistics)

  supervisor.shutdown()
  finalPrint()
}
