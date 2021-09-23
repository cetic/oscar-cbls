package oscar.examples.cbls.distrib

import oscar.cbls.core.computation.Store
import oscar.cbls.core.distrib.Supervisor
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.Neighborhood
import oscar.cbls.lib.search.combinators.distributed.DistributedFirst
import oscar.cbls._
import oscar.examples.cbls.wlp.WarehouseLocationGenerator

import scala.collection.parallel.immutable.ParRange
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

object WarehouseLocationDistributed1 extends App {

  //the number of warehouses
  val W: Int = 2000

  //the number of delivery points
  val D: Int = 1000

  println(s"WarehouseLocation(W:$W, D:$D)")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (_, distanceCost) = WarehouseLocationGenerator.apply(W, D, 0, 100, 3)

  val costForOpeningWarehouse = Array.fill(W)(1000L)

  println("created instance")

  def createSearchProcedure(): (Store, Neighborhood, Objective, () => Unit) = {

    val m = Store()

    val warehouseOpenArray = Array.tabulate(W)(l => CBLSIntVar(m, 0, 0 to 1, s"warehouse_${l}_open"))
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
          swapsNeighborhood(warehouseOpenArray, searchZone1 = {
            val range = (0 until W / 6).map(_ * 6); () => range
          }, name = "SwapWarehouses1"),
          swapsNeighborhood(warehouseOpenArray, searchZone1 = {
            val range = (0 until W / 6).map(_ * 6 + 1); () => range
          }, name = "SwapWarehouses2"),
          swapsNeighborhood(warehouseOpenArray, searchZone1 = {
            val range = (0 until W / 6).map(_ * 6 + 2); () => range
          }, name = "SwapWarehouses3"),
          swapsNeighborhood(warehouseOpenArray, searchZone1 = {
            val range = (0 until W / 6).map(_ * 6 + 3); () => range
          }, name = "SwapWarehouses4"),
          swapsNeighborhood(warehouseOpenArray, searchZone1 = {
            val range = (0 until W / 6).map(_ * 6 + 4); () => range
          }, name = "SwapWarehouses5"),
          swapsNeighborhood(warehouseOpenArray, searchZone1 = {
            val range = (0 until W / 6).map(_ * 6 + 5); () => range
          }, name = "SwapWarehouses6")))
        onExhaustRestartAfter(randomSwapNeighborhood(warehouseOpenArray, () => W / 10), 2, obj)
        onExhaustRestartAfter(randomizeNeighborhood(warehouseOpenArray, () => W / 5), 2, obj))

    (m, neighborhood, obj, () => {
      println(openWarehouses)
    })
  }

  //supervisor side
  val (store, search, obj, finalPrint) = createSearchProcedure()

  val supervisor: Supervisor = Supervisor.startSupervisorAndActorSystem(search, verbose = false, tic = 1.seconds)

  val nbWorker = 6
  for (i <- ParRange(0, nbWorker, 1, inclusive = true)) {
    if (i == 0) {
      val search2 = search.showObjectiveFunction(obj)
      search2.verbose = 1
      search2.doAllMoves(obj = obj)
    } else {
      val (store2, search2, _, _) = createSearchProcedure()
      supervisor.createLocalWorker(store2, search2)
      search2.verbose = 2
    }
  }

  supervisor.shutdown()

  finalPrint()
}
