package oscar.examples.cbls.distrib

import oscar.cbls._
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.core.distrib.Supervisor
import oscar.cbls.core.search.Neighborhood
import oscar.cbls.lib.search.combinators.distributed.{DistributedBest, DistributedFirst, DistributedRestartFromBest, Remote}
import oscar.cbls.modeling.ModelingAPI
import oscar.examples.cbls.wlp.WarehouseLocationGenerator

import scala.collection.parallel.immutable.ParRange

object BenchmarkerUWLP extends ModelingAPI {
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
                         objective: Long,
                         nbIters: Long) {
    def toCSVLine: String = s"$nbName;$withHotRestart;$nbWorkers;$nbWarehouses;$nbDelivers;$timeCreatedModel;$timeActorSysCreation;$timeRunning;$objective;$nbIters"
  }

  //Nb Iterations per benchmark
  val NB_ITERS = 25

  //max nbWorkers
  val MAX_WORKERS: Int = 8

  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  def createCosts(nbWarehouses: Int,
                  nbDelivers: Int): (Array[Array[Long]], Array[Long], Array[Array[Long]]) = {
    //cost matrix

    val (_,distanceCost,_,_,wtwDistances) =
      WarehouseLocationGenerator.problemWithPositions(nbWarehouses, nbDelivers)

    //val (_, distanceCost) = WarehouseLocationGenerator(nbWarehouses, nbDelivers)
    val costForOpeningWarehouse: Array[Long] = Array.fill(nbWarehouses)(1000L)
    (distanceCost, costForOpeningWarehouse, wtwDistances)
  }

  //def kNearestClosedWarehouses(warehouse:Int,k:Int) = KSmallest.kFirst(k, closestWarehouses(warehouse), filter = (otherWarehouse) => warehouseOpenArray(otherWarehouse).newValue == 0)

  def createCBLSModel(nbWarehouses: Int,
                      nbDelivers: Int,
                      distanceCost: Array[Array[Long]],
                      costForOpeningWarehouse: Array[Long],
                      warehouseToWarehouseDistances: Array[Array[Long]],
                      neighborhoodType: NeighborhoodType,
                      nbWorkers: Int): (Store, Objective, Neighborhood) = {

    //This is an array that, for each warehouse, keeps the sorted closest warehouses in a lazy way.
    val closestWarehouses = Array.tabulate(nbWarehouses)(warehouse =>
      KSmallest.lazySort(
        Array.tabulate(nbWarehouses)(warehouse => warehouse),
        otherWarehouse => warehouseToWarehouseDistances(warehouse)(otherWarehouse)
      ))

    //CBLS Store
    val m = Store()
    val warehouseOpenArray = Array.tabulate(nbWarehouses)(l => CBLSIntVar(m, 0, 0 to 1, s"warehouse_${l}_open"))
    val openWarehouses = filter(warehouseOpenArray).setName("openWarehouses")
    val distanceToNearestOpenWarehouseLazy = Array.tabulate(nbDelivers)(d =>
      minConstArrayValueWise(distanceCost(d).map(_.toInt), openWarehouses, defaultCostForNoOpenWarehouse))
    val obj = Objective(sum(distanceToNearestOpenWarehouseLazy) + sum(costForOpeningWarehouse, openWarehouses))
    m.close()
    //Neighborhoods
    val divWarehouses = nbWarehouses/nbWorkers
    val divRange = 0 until divWarehouses
    val assignNb: Neighborhood = assignNeighborhood(warehouseOpenArray, "SwitchWarehouse")
    val swapNb: Neighborhood = swapsNeighborhood(warehouseOpenArray, "SwapWarehouse")
    /////
    def kNearestClosedWarehouses(warehouse:Int, k:Int) =
      KSmallest.kFirst(k, closestWarehouses(warehouse),
        filter = otherWarehouse => warehouseOpenArray(otherWarehouse).newValue == 0
      )
    /////
    def swapsK(k:Int, openWarehouseTocConsider:()=>Iterable[Int] = openWarehouses) =
      swapsNeighborhood(
        warehouseOpenArray,
        searchZone1 = openWarehouseTocConsider,
        searchZone2 = () => (firstWareHouse,_) => kNearestClosedWarehouses(firstWareHouse,k),
        name = "Swap" + k + "Nearest",
        symmetryCanBeBrokenOnIndices = false
      )
    val swapsKNb: Neighborhood = swapsK(20) guard(() => openWarehouses.value.size >= 5)
    val randomSwapNb: Neighborhood = randomSwapNeighborhood(warehouseOpenArray, () => nbWarehouses/10)
    val swapNbs: Seq[Neighborhood] = if (nbWorkers < 4) {
      Seq(swapNb)
    } else {
      for {i <- 0 until nbWorkers} yield swapsNeighborhood(warehouseOpenArray,searchZone1 = {val range = divRange.map(_*nbWorkers+i); () => range}, name = s"SwapWarehouse_$i")
    }
    val basicNeighborhood: Neighborhood = bestSlopeFirst(List(
      assignNb,
      swapNb,
      swapsKNb
    ))
    val seqNeighborhood: Neighborhood = basicNeighborhood onExhaustRestartAfter (randomSwapNb, 0, obj, minRestarts = 20, restartFromBest = true)
    val parNeighborhoods: Array[Neighborhood] = (assignNb +: (swapsKNb +: swapNbs)).toArray
    val neighborhood: Neighborhood = neighborhoodType match {
      case Sequential =>
        seqNeighborhood
      case DistRemote =>
        new Remote(seqNeighborhood)
      case DistRestart =>
        new DistributedRestartFromBest(
          basicNeighborhood,
          randomSwapNb,
          minNbRestarts = 20,
          nbConsecutiveRestartWithoutImprovement = 0,
          nbOngoingSearchesToCancelWhenNewBest = 0,
          setMaxWorkers = Some(nbWorkers),
          gracefulStop = false
        )
      case DistFirst(_) =>
        new DistributedFirst(parNeighborhoods) onExhaustRestartAfter (randomSwapNb, 0, obj, minRestarts = 20, restartFromBest = true)
      case DistBest(_) =>
        new DistributedBest(parNeighborhoods) onExhaustRestartAfter (randomSwapNb, 0, obj, minRestarts = 20, restartFromBest = true)
    }
    (m, obj, neighborhood)
  }

  def runProblem(nbName: String,
                 nbWorkers: Int,
                 nbWarehouses: Int,
                 nbDelivers: Int,
                 distanceCost: Array[Array[Long]],
                 costForOpeningWarehouse: Array[Long],
                 warehouseToWarehouseDistances: Array[Array[Long]],
                 neighborhoodType: NeighborhoodType): BenchResult = {
    // Search Procedure
    val benchResult: BenchResult = neighborhoodType match {
      case Sequential =>
        // Sequential execution
        val t0 = System.nanoTime()
        val (_, obj, nb) = createCBLSModel(nbWarehouses, nbDelivers, distanceCost,
          costForOpeningWarehouse, warehouseToWarehouseDistances, neighborhoodType, nbWorkers)
        val t1 = System.nanoTime()
        val timeCreation = (t1-t0)/1000000
        val t2 = System.nanoTime()
        val iters = nb.doAllMoves(obj = obj)
        val t3 = System.nanoTime()
        val timeRun = (t3-t2)/1000000
        val objValue = obj.value
        BenchResult(nbName, neighborhoodType.withHotRestart, 0, nbWarehouses, nbDelivers, timeCreation, 0, timeRun, objValue, iters)
      case DistRestart =>
        // Stage 1 : Creation of models
        val t0 = System.nanoTime()
        val arrayOfStoreSearchObjs:Array[(Store, Objective, Neighborhood)] = new Array[(Store, Objective, Neighborhood)](nbWorkers+1)
        for (i <- ParRange(0, nbWorkers, 1, inclusive = true)) {
          arrayOfStoreSearchObjs(i) = createCBLSModel(nbWarehouses, nbDelivers, distanceCost,
            costForOpeningWarehouse, warehouseToWarehouseDistances, neighborhoodType, nbWorkers)
        }
        val t1 = System.nanoTime()
        val timeCreation = (t1-t0)/1000000
        // Stage 2 : Start actor system
        val t2 = System.nanoTime()
        val (_, obj, nb) = arrayOfStoreSearchObjs(nbWorkers)
        val supervisor:Supervisor = Supervisor.startSupervisorAndActorSystem(nb, hotRestart = neighborhoodType.withHotRestart)
        val t3 = System.nanoTime()
        val timeActSys = (t3-t2)/1000000
        // Stage 3 : Run procedure
        val t4 = System.nanoTime()
        for (i <- 0 until nbWorkers) {
          val (mi, _, nbi) = arrayOfStoreSearchObjs(i)
          supervisor.createLocalWorker(mi, nbi)
          nbi.verbose = 0
        }
        val iters = nb.maxMoves(1).doAllMoves(obj = obj)
        supervisor.shutdown()
        val t5 = System.nanoTime()
        val timeRun = (t5-t4)/1000000
        val objValue = obj.value
        BenchResult(nbName, neighborhoodType.withHotRestart, nbWorkers, nbWarehouses, nbDelivers, timeCreation, timeActSys, timeRun, objValue, iters)
      case _ =>
        // Stage 1 : Creation of models
        val t0 = System.nanoTime()
        val arrayOfStoreSearchObjs:Array[(Store,Objective,Neighborhood)] = new Array[(Store, Objective, Neighborhood)](nbWorkers+1)
        for (i <- ParRange(0, nbWorkers, 1, inclusive = true)) {
          arrayOfStoreSearchObjs(i) = createCBLSModel(nbWarehouses, nbDelivers, distanceCost,
            costForOpeningWarehouse, warehouseToWarehouseDistances, neighborhoodType, nbWorkers)
        }
        val t1 = System.nanoTime()
        val timeCreation = (t1-t0)/1000000
        // Stage 2 : Start actor system
        val t2 = System.nanoTime()
        val (_, obj, nb) = arrayOfStoreSearchObjs(nbWorkers)
        val supervisor:Supervisor = Supervisor.startSupervisorAndActorSystem(nb, hotRestart = neighborhoodType.withHotRestart)
        val t3 = System.nanoTime()
        val timeActSys = (t3-t2)/1000000
        // Stage 3 : Run procedure
        val t4 = System.nanoTime()
        for (i <- 0 until nbWorkers) {
          val (mi, _, nbi) = arrayOfStoreSearchObjs(i)
          supervisor.createLocalWorker(mi, nbi)
          nbi.verbose = 0
        }
        val iters = nb.doAllMoves(obj = obj)
        supervisor.shutdown()
        val t5 = System.nanoTime()
        val timeRun = (t5-t4)/1000000
        val objValue = obj.value
        BenchResult(nbName, neighborhoodType.withHotRestart, nbWorkers, nbWarehouses, nbDelivers, timeCreation, timeActSys, timeRun, objValue, iters)
    }
    benchResult
  }

  def runBenchmarkNb(j: Int,
                     nbWsI: Int,
                     nbDsI: Int,
                     distanceCost: Array[Array[Long]],
                     costForOpeningWarehouse: Array[Long],
                     warehouseToWarehouseDistances: Array[Array[Long]],
                     neighborhoodType: NeighborhoodType): Unit = {
    for {_ <- 1 to NB_ITERS} {
      System.gc()
      val bench = runProblem(neighborhoodType.toString, j, nbWsI, nbDsI, distanceCost,
        costForOpeningWarehouse, warehouseToWarehouseDistances, neighborhoodType)
      println(bench.toCSVLine)
    }
  }

  def main(args: Array[String]): Unit = {
    val nbWs = Array(2000, 4000) // Array(500, 1000) // Array(1000, 2000, 3000, 4000)
    val nbDs = Array(1000, 2000) // Array(250, 500) // Array(500, 1000, 1500, 2000)
    // Warming loop
    val (dc1, c1, wtw1) = createCosts(1000, 500)
    runProblem("Warming", 1, 1000, 500, dc1, c1, wtw1, Sequential)
    runProblem("Warming", 1, 1000, 500, dc1, c1, wtw1, DistRemote)
    println("Neighborhood Name;Hot Restart;# Workers;# Warehouses;# Delivery Points;Model Creation (ms);Actor Creation (ms);Solution Computing (ms);Objective Value;# Iterations")
    for {i <- nbWs.indices} {
      val (distanceCost, costForOpeningWarehouse, wtwDistances) = createCosts(nbWs(i), nbDs(i))
      // Sequential
      runBenchmarkNb(1, nbWs(i), nbDs(i), distanceCost, costForOpeningWarehouse, wtwDistances, Sequential)
      // Remote
      runBenchmarkNb(1, nbWs(i), nbDs(i), distanceCost, costForOpeningWarehouse, wtwDistances, DistRemote)
      for {j <- 1 to MAX_WORKERS} {
        // Distributed Restart
        runBenchmarkNb(j, nbWs(i), nbDs(i), distanceCost, costForOpeningWarehouse, wtwDistances, DistRestart)
        // Distributed First with Hot restart
        runBenchmarkNb(j, nbWs(i), nbDs(i), distanceCost, costForOpeningWarehouse, wtwDistances, DistFirst())
        // Distributed First without Hot restart
        //runBenchmarkNb(j, nbWs(i), nbDs(i), distanceCost, costForOpeningWarehouse, wtwDistances, DistFirst(false))
        // Distributed Best with Hot restart
        runBenchmarkNb(j, nbWs(i), nbDs(i), distanceCost, costForOpeningWarehouse, wtwDistances, DistBest())
        // Distributed Best without Hot restart
        //runBenchmarkNb(j, nbWs(i), nbDs(i), distanceCost, costForOpeningWarehouse, wtwDistances, DistBest(false))
      }
    }
  }
}
