package examples.oscar.cbls.distributed

import oscar.cbls._
import oscar.cbls.algo.generator.WarehouseLocationGenerator
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.core.computation.Store
import oscar.cbls.core.distributed
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{Best, Neighborhood}
import oscar.cbls.lib.search.combinators.distributed.DistributedFirst

import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.language.postfixOps

object WarehouseLocationDistributed2 extends App {
  val nbWorker = 6

  //the number of warehouses
  val W: Int = 4000

  //the number of delivery points
  val D: Int = 2000

  println("WarehouseLocation(W:" + W + ", D:" + D + ")")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (_, distanceCost, _, _, warehouseToWarehouseDistances) = WarehouseLocationGenerator.problemWithPositions(W, D, 0, 100, 3)

  val costForOpeningWarehouse = Array.fill(W)(1000L)

  println("created instance")

  def createSearchProcedure(): (Store, Neighborhood, Objective, () => Unit) = {

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
        otherWarehouse => warehouseToWarehouseDistances(warehouse)(otherWarehouse)
      ))

    //TODO: normally, we should search for k being the first encountered open warehouse
    //this procedure returns the k closest closed warehouses
    def kNearestClosedWarehouses(warehouse: Int, k: Int) = KSmallest.kFirst(k, closestWarehouses(warehouse), filter = (otherWarehouse) => warehouseOpenArray(otherWarehouse).newValue == 0)

    def swapsK(k: Int, openWarehouseTocConsider: () => Iterable[Int] = openWarehouses, modulo: Int = 0, shift: Int = 0) =
      swapsNeighborhood(warehouseOpenArray,
        searchZone1 = if (modulo == 0) openWarehouseTocConsider else () => openWarehouseTocConsider().filter(_ % modulo == shift),
        searchZone2 = () => (firstWareHouse, _) => kNearestClosedWarehouses(firstWareHouse, k),
        name = "SwapK" + k, //+ (if(modulo ==0) "" else s"mod:$modulo,s:$shift"),
        symmetryCanBeBrokenOnIndices = false)

    val nbSmallSwaps = 1 max nbWorker
    val nbBigSwaps = 1 max nbWorker
    //These neighborhoods are inefficient and slow; using multiple core is the wrong answer to inefficiency
    val neighborhood = (
      new DistributedFirst((
        List(assignNeighborhood(warehouseOpenArray, "SwitchWarehouse")
        ) ++ ((0 until nbSmallSwaps).map((i: Int) => swapsK(20, modulo = nbSmallSwaps, shift = i)))
          ++ ((0 until nbBigSwaps).map((i: Int) => swapsK(100, modulo = nbBigSwaps, shift = i)))
        ).toArray)
      onExhaustRestartAfter(randomSwapNeighborhood(warehouseOpenArray, () => W / 10), 2, obj, minRestarts = 5)
      )

    (m, neighborhood, obj, () => {
      println(openWarehouses)
    })
  }

  val arrayOfStoreSearchObjAndFinalPrint: Array[(Store, Neighborhood, Objective, () => Unit)] =
    Array.fill(nbWorker + 1)(null)

  for (i <- (0 to nbWorker).par) {
    arrayOfStoreSearchObjAndFinalPrint(i) = createSearchProcedure()
  }

  //supervisor side
  val (store, search, obj, finalPrint) = arrayOfStoreSearchObjAndFinalPrint(nbWorker)
  val supervisor = distributed.startSupervisorAndActorSystem(search)

  //creating all the workers; here we only create local workers
  for (i <- 0 until nbWorker) {
    val (store2, search2, _, _) = arrayOfStoreSearchObjAndFinalPrint(i)
    supervisor.createLocalWorker(store2, search2)
  }

  val start = System.currentTimeMillis()

  val search2 = search.showObjectiveFunction(obj)
  search2.verbose = 1
  search2.doAllMoves(obj = obj)

  //    this.time(nbWorker) = System.currentTimeMillis() - start

  println(search2.profilingStatistics)
  supervisor.shutdown()

  finalPrint()
  //  System.exit(0)

  System.gc()
  //  }

  //println("time:\n\t" + time.zipWithIndex.mkString("\n\t"))
}