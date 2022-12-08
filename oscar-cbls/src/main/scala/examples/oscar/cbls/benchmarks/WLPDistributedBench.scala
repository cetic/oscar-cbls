package examples.oscar.cbls.benchmarks

import oscar.cbls._
import oscar.cbls.algo.generator.WarehouseLocationGenerator
import oscar.cbls.business.scheduling.ActivityId
import oscar.cbls.core.distributed.Supervisor
import oscar.cbls.core.search.Neighborhood
import oscar.cbls.lib.search.combinators.distributed.{DistributedBest, DistributedFirst}

import scala.collection.parallel.immutable.ParRange

object WLPDistributedBench {
  case class BenchResult(nbName: String,
                         nbWorkers: Int,
                         nbWarehouses: Int,
                         nbDelivers: Int,
                         timeCreatedModel: Long,
                         timeActorSysCreation: Long,
                         timeRunning: Long,
                         minObjective: Long,
                         maxObjective: Long) {
    def toCSVLine: String = s"$nbName;$nbWorkers;$nbWarehouses;$nbDelivers;$timeCreatedModel;$timeActorSysCreation;$timeRunning;$minObjective;$maxObjective"
  }

  //Nb Iterations per benchmark
  val NB_ITERS = 5

  //max nbWorkers
  val MAX_WORKERS: Int = 8

  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  def createCosts(nbWarehouses: Int,
                  nbDelivers: Int): (Array[Array[Long]], Array[Long]) = {
    //cost matrix
    val (_, distanceCost) = WarehouseLocationGenerator.apply(nbWarehouses, nbDelivers)
    val costForOpeningWarehouse: Array[Long] = Array.fill(nbWarehouses)(1000L)
    (distanceCost, costForOpeningWarehouse)
  }

  def createCBLSModel(nbWarehouses: ActivityId,
                      nbDelivers: ActivityId,
                      distanceCost: Array[Array[Long]],
                      costForOpeningWarehouse: Array[Long],
                      isFirstNb: Boolean = true): (Store, Objective, Neighborhood) = {
    //CBLS Store
    val m = Store()
    val warehouseOpenArray = Array.tabulate(nbWarehouses)(l => CBLSIntVar(m, 0, 0 to 1, s"warehouse_${l}_open"))
    val openWarehouses = filter(warehouseOpenArray).setName("openWarehouses")
    val distanceToNearestOpenWarehouseLazy = Array.tabulate(nbDelivers)(d =>
      minConstArrayValueWise(distanceCost(d).map(_.toInt), openWarehouses, defaultCostForNoOpenWarehouse))
    val obj = Objective(sum(distanceToNearestOpenWarehouseLazy) + sum(costForOpeningWarehouse, openWarehouses))
    m.close()
    //Neighborhood
    val divWarehouses = nbWarehouses / 4
    val divRange = 0 until divWarehouses
    val arrayNbs: Array[Neighborhood] = Array(
      assignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
      swapsNeighborhood(warehouseOpenArray, searchZone1 = {
        val range = divRange.map(_ * 4);
        () => range
      }, name = "SwapWarehouses1"),
      swapsNeighborhood(warehouseOpenArray, searchZone1 = {
        val range = divRange.map(_ * 4 + 1);
        () => range
      }, name = "SwapWarehouses2"),
      swapsNeighborhood(warehouseOpenArray, searchZone1 = {
        val range = divRange.map(_ * 4 + 2);
        () => range
      }, name = "SwapWarehouses3"),
      swapsNeighborhood(warehouseOpenArray, searchZone1 = {
        val range = divRange.map(_ * 4 + 3);
        () => range
      }, name = "SwapWarehouses4"))
    val neighborhood = if (isFirstNb) new DistributedFirst(arrayNbs) else new DistributedBest(arrayNbs)
    (m, obj, neighborhood)
  }

  def runProblem(nbWorkers: Int,
                 nbWarehouses: Int,
                 nbDelivers: Int,
                 distanceCost: Array[Array[Long]],
                 costForOpeningWarehouse: Array[Long],
                 isFirstNb: Boolean = true): BenchResult = {
    val nbName = if (isFirstNb) "DistributedFirst" else "DistributedBest"
    // Stage 1 : Create Model
    val t0 = System.nanoTime()
    val (m, obj, nb) = createCBLSModel(nbWarehouses, nbDelivers, distanceCost, costForOpeningWarehouse, isFirstNb)
    val t1 = System.nanoTime()
    val timeCreation = (t1 - t0) / 1000000
    // Stage 2 : Start actor system
    val t2 = System.nanoTime()
    val supervisor: Supervisor = Supervisor.startSupervisorAndActorSystem(nb)
    val t3 = System.nanoTime()
    val timeActSys = (t3 - t2) / 1000000
    // Stage 3 : Run procedure
    val t4 = System.nanoTime()
    for (i <- ParRange(0, nbWorkers + 1, 1, inclusive = true)) {
      if (i == 0) {
        nb.verbose = 0
        nb.doAllMoves(obj = obj)
      } else {
        val (mi, _, nbi) = createCBLSModel(nbWarehouses, nbDelivers, distanceCost, costForOpeningWarehouse, isFirstNb)
        supervisor.createLocalWorker(mi, nbi)
        nbi.verbose = 0
      }
    }
    supervisor.shutdown()
    val t5 = System.nanoTime()
    val timeRun = (t5 - t4) / 1000000
    val objValue = obj.value
    BenchResult(nbName, nbWorkers, nbWarehouses, nbDelivers, timeCreation, timeActSys, timeRun, objValue, objValue)
  }

  def runBenchmarkNb(j: Int,
                     nbWsI: Int,
                     nbDsI: Int,
                     distanceCost: Array[Array[Long]],
                     costForOpeningWarehouse: Array[Long],
                     isFirstNb: Boolean): Unit = {
    val nbName = if (isFirstNb) "DistributedFirst" else "DistributedBest"
    var (nb1, nb2, nb3, minObj, maxObj) = (0L, 0L, 0L, Long.MaxValue, 0L)
    for {_ <- 1 to NB_ITERS} {
      System.gc()
      val bench = runProblem(j, nbWsI, nbDsI, distanceCost, costForOpeningWarehouse)
      nb1 += bench.timeCreatedModel
      nb2 += bench.timeActorSysCreation
      nb3 += bench.timeRunning
      minObj = minObj min bench.minObjective
      maxObj = maxObj max bench.maxObjective
    }
    val benchF = BenchResult(nbName, j, nbWsI, nbDsI, nb1 / NB_ITERS, nb2 / NB_ITERS, nb3 / NB_ITERS, minObj, maxObj)
    println(benchF.toCSVLine)
  }

  def main(args: Array[String]): Unit = {
    val nbWs = Array(1000, 2000, 3000, 4000)
    val nbDs = Array(500, 1000, 1500, 2000)
    // Warming loop
    val (dc1, c1) = createCosts(1000, 500)
    runProblem(1, 1000, 500, dc1, c1)
    println("Neighborhood Name;Workers;Warehouses;Deliver Points;Model Creation;Actor Creation;Solution Computing;Minimum Objective;Maximum Objective")
    for {i <- nbWs.indices} {
      val (distanceCost, costForOpeningWarehouse) = createCosts(nbWs(i), nbDs(i))
      for {j <- 1 to MAX_WORKERS} {
        runBenchmarkNb(j, nbWs(i), nbDs(i), distanceCost, costForOpeningWarehouse, isFirstNb = true)
        runBenchmarkNb(j, nbWs(i), nbDs(i), distanceCost, costForOpeningWarehouse, isFirstNb = false)
      }
    }
  }
}
