package oscar.examples.cbls.distrib

import oscar.cbls._
import oscar.cbls.core.distrib.Supervisor
import oscar.cbls.core.search.Neighborhood
import oscar.cbls.lib.search.combinators.distributed.{DistributedBest, DistributedFirst, DistributedRestartFromBest, Remote}
import oscar.cbls.modeling.ModelingAPI
import oscar.examples.cbls.wlp.WarehouseLocationGenerator

import scala.collection.parallel.immutable.ParRange

object Benchmarker extends ModelingAPI {
  // Inner Classes
  sealed trait NeighborhoodType {
    def withHotRestart: Boolean
  }
  case object Sequential extends NeighborhoodType {

    override def toString: String = "Sequential"

    override val withHotRestart: Boolean = false
  }
  case object DistRemote extends NeighborhoodType {
    override def toString: String = "Remote"

    override val withHotRestart: Boolean = false
  }
  case object DistRestart extends NeighborhoodType  {
    override def toString: String = "Distributed Restart"

    override val withHotRestart: Boolean = false
  }
  case class DistFirst(withHotRestart: Boolean = true) extends NeighborhoodType  {
    override def toString: String = "Distributed First"
  }
  case class DistBest(withHotRestart: Boolean = true) extends NeighborhoodType {
    override def toString: String = "Distributed Best"
  }

  case class BenchResult(nbName: String,
                         withHotRestart: Boolean,
                         nbWorkers: Int,
                         nbWarehouses: Int,
                         nbDelivers: Int,
                         timeCreatedModel: Long,
                         timeActorSysCreation: Long,
                         timeRunning: Long,
                         objective: Long) {
    def toCSVLine: String = s"$nbName;$withHotRestart;$nbWorkers;$nbWarehouses;$nbDelivers;$timeCreatedModel;$timeActorSysCreation;$timeRunning;$objective"
  }

  //Nb Iterations per benchmark
  val NB_ITERS = 10

  //max nbWorkers
  val MAX_WORKERS: Int = 10

  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  def createCosts(nbWarehouses: Int,
                  nbDelivers: Int): (Array[Array[Long]], Array[Long]) = {
    //cost matrix
    val (_, distanceCost) = WarehouseLocationGenerator(nbWarehouses, nbDelivers)
    val costForOpeningWarehouse: Array[Long] = Array.fill(nbWarehouses)(1000L)
    (distanceCost, costForOpeningWarehouse)
  }

  def createCBLSModel(nbWarehouses: Int,
                      nbDelivers: Int,
                      distanceCost: Array[Array[Long]],
                      costForOpeningWarehouse: Array[Long],
                      neighborhoodType: NeighborhoodType): (Store, Objective, Neighborhood) = {
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
    val arrayNbs: Array[Neighborhood] = Array(
      assignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
      swapsNeighborhood(warehouseOpenArray,searchZone1 = {val range = divRange.map(_*4    ); () => range}, name = "SwapWarehouses1"),
      swapsNeighborhood(warehouseOpenArray,searchZone1 = {val range = divRange.map(_*4 + 1); () => range}, name = "SwapWarehouses2"),
      swapsNeighborhood(warehouseOpenArray,searchZone1 = {val range = divRange.map(_*4 + 2); () => range}, name = "SwapWarehouses3"),
      swapsNeighborhood(warehouseOpenArray,searchZone1 = {val range = divRange.map(_*4 + 3); () => range}, name = "SwapWarehouses4"))
    val basicNeighborhood: Neighborhood = bestSlopeFirst(List(
      assignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
      swapsNeighborhood(warehouseOpenArray, "SwapWarehouses"))
    )
    val seqNeighborhood: Neighborhood = basicNeighborhood onExhaustRestartAfter (randomSwapNeighborhood(warehouseOpenArray, () => divWarehouses/10), 2, obj)
    val neighborhood: Neighborhood = neighborhoodType match {
      case Sequential =>
        seqNeighborhood
      case DistRemote =>
        new Remote(seqNeighborhood)
      case DistRestart =>
        new DistributedRestartFromBest(
          basicNeighborhood,
          randomSwapNeighborhood(warehouseOpenArray, () => nbWarehouses/10),
          minNbRestarts = 0,
          nbConsecutiveRestartWithoutImprovement = 5
        )
      case DistFirst(_) =>
        new DistributedFirst(arrayNbs) onExhaustRestartAfter (randomSwapNeighborhood(warehouseOpenArray, () => nbWarehouses/10), 2, obj)
      case DistBest(_) =>
        new DistributedBest(arrayNbs) onExhaustRestartAfter (randomSwapNeighborhood(warehouseOpenArray, () => nbWarehouses/10), 2, obj)
    }
    (m, obj, neighborhood)
  }

  def runProblem(nbName: String,
                 nbWorkers: Int,
                 nbWarehouses: Int,
                 nbDelivers: Int,
                 distanceCost: Array[Array[Long]],
                 costForOpeningWarehouse: Array[Long],
                 neighborhoodType: NeighborhoodType): BenchResult = {
    // Stage 1 : Create Model
    val t0 = System.nanoTime()
    val (m, obj, nb) = createCBLSModel(nbWarehouses, nbDelivers, distanceCost, costForOpeningWarehouse, neighborhoodType)
    val t1 = System.nanoTime()
    val timeCreation = (t1-t0)/1000000
    // Sequential Neighborhood
    val benchResult: BenchResult = neighborhoodType match {
      case Sequential =>
        // Sequential execution
        val t2 = System.nanoTime()
        nb.doAllMoves(obj = obj)
        val t3 = System.nanoTime()
        val timeRun = (t3-t2)/1000000
        val objValue = obj.value
        BenchResult(nbName, neighborhoodType.withHotRestart, 0, nbWarehouses, nbDelivers, timeCreation, 0, timeRun, objValue)
      case DistRestart =>
        // Stage 2 : Start actor system
        val t2 = System.nanoTime()
        val supervisor:Supervisor = Supervisor.startSupervisorAndActorSystem(nb, hotRestart = neighborhoodType.withHotRestart)
        val t3 = System.nanoTime()
        val timeActSys = (t3-t2)/1000000
        // Stage 3 : Run procedure
        val t4 = System.nanoTime()
        for (_ <- ParRange(0, nbWorkers, 1, inclusive = true)) {
          val (mi, _, nbi) = createCBLSModel(nbWarehouses, nbDelivers, distanceCost, costForOpeningWarehouse, neighborhoodType)
          supervisor.createLocalWorker(mi, nbi)
          nbi.verbose = 0
        }
        nb.maxMoves(1).doAllMoves(obj = obj)
        supervisor.shutdown()
        val t5 = System.nanoTime()
        val timeRun = (t5-t4)/1000000
        val objValue = obj.value
        BenchResult(nbName, neighborhoodType.withHotRestart, nbWorkers, nbWarehouses, nbDelivers, timeCreation, timeActSys, timeRun, objValue)
      case _ =>
        // Stage 2 : Start actor system
        val t2 = System.nanoTime()
        val supervisor:Supervisor = Supervisor.startSupervisorAndActorSystem(nb, hotRestart = neighborhoodType.withHotRestart)
        val t3 = System.nanoTime()
        val timeActSys = (t3-t2)/1000000
        // Stage 3 : Run procedure
        val t4 = System.nanoTime()
        for (i <- ParRange(0, nbWorkers+1, 1, inclusive = true)) {
          if (i == 0) {
            nb.verbose = 0
            nb.doAllMoves(obj = obj)
          } else {
            val (mi, _, nbi) = createCBLSModel(nbWarehouses, nbDelivers, distanceCost, costForOpeningWarehouse, neighborhoodType)
            supervisor.createLocalWorker(mi, nbi)
            nbi.verbose = 0
          }
        }
        supervisor.shutdown()
        val t5 = System.nanoTime()
        val timeRun = (t5-t4)/1000000
        val objValue = obj.value
        BenchResult(nbName, neighborhoodType.withHotRestart, nbWorkers, nbWarehouses, nbDelivers, timeCreation, timeActSys, timeRun, objValue)
    }
    benchResult
  }

  def runBenchmarkNb(j: Int,
                     nbWsI: Int,
                     nbDsI: Int,
                     distanceCost: Array[Array[Long]],
                     costForOpeningWarehouse: Array[Long],
                     neighborhoodType: NeighborhoodType): Unit = {
    for {_ <- 1 to NB_ITERS} {
      System.gc()
      val bench = runProblem(neighborhoodType.toString, j, nbWsI, nbDsI, distanceCost, costForOpeningWarehouse, neighborhoodType)
      println(bench.toCSVLine)
    }
  }

  def main(args: Array[String]): Unit = {
    val nbWs = Array(1000, 2000, 3000, 4000)
    val nbDs = Array(500, 1000, 1500, 2000)
    // Warming loop
    val (dc1, c1) = createCosts(1000, 500)
    runProblem("Warming", 0, 1000, 500, dc1, c1, Sequential)
    println("Neighborhood Name;Hot Restart;# Workers;# Warehouses;# Delivery Points;Model Creation (ms);Actor Creation (ms);Solution Computing (ms);Objective Value")
    for {i <- nbWs.indices} {
      val (distanceCost, costForOpeningWarehouse) = createCosts(nbWs(i), nbDs(i))
      // Sequential
      runBenchmarkNb(0, nbWs(i), nbDs(i), distanceCost, costForOpeningWarehouse, Sequential)
      for {j <- 1 to MAX_WORKERS} {
        // Remote
        runBenchmarkNb(j, nbWs(i), nbDs(i), distanceCost, costForOpeningWarehouse, DistRemote)
        // Distributed Restart
        runBenchmarkNb(j, nbWs(i), nbDs(i), distanceCost, costForOpeningWarehouse, DistRestart)
        // Distributed First with hot restart
        runBenchmarkNb(j, nbWs(i), nbDs(i), distanceCost, costForOpeningWarehouse, DistFirst())
        // Distributed First without hot restart
        runBenchmarkNb(j, nbWs(i), nbDs(i), distanceCost, costForOpeningWarehouse, DistFirst(false))
        // Distributed Best with hot restart
        runBenchmarkNb(j, nbWs(i), nbDs(i), distanceCost, costForOpeningWarehouse, DistBest())
        // Distributed Best without hot restart
        runBenchmarkNb(j, nbWs(i), nbDs(i), distanceCost, costForOpeningWarehouse, DistBest(false))
      }
    }
  }
}