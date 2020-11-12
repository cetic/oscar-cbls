package oscar.examples.cbls.distrib

import oscar.cbls._
import oscar.cbls.core.distrib.Supervisor
import oscar.cbls.core.search.Neighborhood
import oscar.cbls.lib.search.combinators.DistributedFirst
import oscar.examples.cbls.wlp.WarehouseLocationGenerator

import scala.collection.parallel.immutable.ParRange
//import scala.concurrent.duration._

object WLDBenchmark {
  case class BenchResult(nbWorkers: Int,
                         nbWarehouses: Int,
                         nbDelivers: Int,
                         timeCreatedModel: Long,
                         timeActorSysCreation: Long,
                         timeRunning: Long,
                         minObjective: Long,
                         maxObjective: Long) {
    def toCSVLine: String = s"$nbWorkers;$nbWarehouses;$nbDelivers;$timeCreatedModel;$timeActorSysCreation;$timeRunning;$minObjective;$maxObjective"
  }

  //Nb Iterations per benchmark
  val NB_ITERS = 5

  //max nbWorkers
  val MAX_WORKERS: Int = 16
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  def createCosts(nbWarehouses: Int,
                  nbDelivers: Int): (Array[Array[Long]], Array[Long]) = {
    //cost matrix
    val (_,distanceCost) = WarehouseLocationGenerator.apply(nbWarehouses, nbDelivers)
    val costForOpeningWarehouse: Array[Long] = Array.fill(nbWarehouses)(1000L)
    (distanceCost, costForOpeningWarehouse)
  }

  def createCBLSModel(nbWorkers: Int,
                      nbWarehouses: Int,
                      nbDelivers: Int,
                      distanceCost: Array[Array[Long]],
                      costForOpeningWarehouse: Array[Long]): (Store, Objective, Neighborhood) = {
    //CBLS Store
    val m = Store()
    val warehouseOpenArray = Array.tabulate(nbWarehouses)(l => CBLSIntVar(m, 0, 0 to 1, s"warehouse_${l}_open"))
    val openWarehouses = filter(warehouseOpenArray).setName("openWarehouses")
    val distanceToNearestOpenWarehouseLazy = Array.tabulate(nbDelivers)(d =>
      minConstArrayValueWise(distanceCost(d).map(_.toInt), openWarehouses, defaultCostForNoOpenWarehouse))
    val obj = Objective(sum(distanceToNearestOpenWarehouseLazy) + sum(costForOpeningWarehouse, openWarehouses))
    m.close()
    //Neighborhood
    val divWarehouses = nbWarehouses/4
    val divRange = 0 until divWarehouses
    //val swapsNbs = for {i <- 0 until nbWorkers} yield swapsNeighborhood(warehouseOpenArray,searchZone1 = {val range = divRange.map(w => w*nbWorkers + i); () => range}, name = s"SwapWarehouses${i+1}")
    val neighborhood = new DistributedFirst(Array(
      assignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
      swapsNeighborhood(warehouseOpenArray,searchZone1 = {val range = divRange.map(_*4    ); () => range}, name = "SwapWarehouses1"),
      swapsNeighborhood(warehouseOpenArray,searchZone1 = {val range = divRange.map(_*4 + 1); () => range}, name = "SwapWarehouses2"),
      swapsNeighborhood(warehouseOpenArray,searchZone1 = {val range = divRange.map(_*4 + 2); () => range}, name = "SwapWarehouses3"),
      swapsNeighborhood(warehouseOpenArray,searchZone1 = {val range = divRange.map(_*4 + 3); () => range}, name = "SwapWarehouses4")))
    (m, obj, neighborhood)
  }

  def runProblem(nbWorkers: Int,
                 nbWarehouses: Int,
                 nbDelivers: Int,
                 distanceCost: Array[Array[Long]],
                 costForOpeningWarehouse: Array[Long]): BenchResult = {
    // Stage 1 : Create Model
    val t0 = System.nanoTime()
    val (m, obj, nb) = createCBLSModel(nbWorkers, nbWarehouses, nbDelivers, distanceCost, costForOpeningWarehouse)
    val t1 = System.nanoTime()
    val timeCreation = (t1-t0)/1000000
    // Stage 2 : Start actor system
    val t2 = System.nanoTime()
    val supervisor:Supervisor = Supervisor.startSupervisorAndActorSystem(m, nb)
    val t3 = System.nanoTime()
    val timeActSys = (t3-t2)/1000000
    // Stage 3 : Run procedure
    val t4 = System.nanoTime()
    for (i <- ParRange(0, nbWorkers+1, 1, inclusive = true)) {
      if (i == 0) {
        nb.verbose = 0
        nb.doAllMoves(obj = obj)
      } else {
        val (mi, _, nbi) = createCBLSModel(nbWorkers, nbWarehouses, nbDelivers, distanceCost, costForOpeningWarehouse)
        supervisor.createLocalWorker(mi, nbi)
        nbi.verbose = 0
      }
    }
    supervisor.shutdown()
    val t5 = System.nanoTime()
    val timeRun = (t5-t4)/1000000
    val objValue = obj.value
    BenchResult(nbWorkers, nbWarehouses, nbDelivers, timeCreation, timeActSys, timeRun, objValue, objValue)
  }

  def main(args: Array[String]): Unit = {
    val nbWs = Array(1000, 2000, 3000, 4000)
    val nbDs = Array(500, 1000, 1500, 2000)
    // Warming loop
    val (dc1, c1) = createCosts(1000, 500)
    runProblem(1, 1000, 500, dc1, c1)
    println("Workers;Warehouses;Deliver points;Model Creation;Actor Creation;Solution Computing;Minimum Objective;Maximum Objective")
    for {i <- nbWs.indices} {
      val (distanceCost, costForOpeningWarehouse) = createCosts(nbWs(i), nbDs(i))
      for {j <- 1 to MAX_WORKERS} {
        var (nb1, nb2, nb3, minObj, maxObj) = (0L, 0L, 0L, Long.MaxValue, 0L)
        for {_ <- 1 to NB_ITERS} {
          System.gc()
          val bench = runProblem(j, nbWs(i), nbDs(i), distanceCost, costForOpeningWarehouse)
          nb1 += bench.timeCreatedModel
          nb2 += bench.timeActorSysCreation
          nb3 += bench.timeRunning
          minObj = minObj min bench.minObjective
          maxObj = maxObj max bench.maxObjective
        }
        val benchF = BenchResult(j, nbWs(i), nbDs(i), nb1/NB_ITERS, nb2/NB_ITERS, nb3/NB_ITERS, minObj, maxObj)
        println(benchF.toCSVLine)
      }
    }
  }
}
