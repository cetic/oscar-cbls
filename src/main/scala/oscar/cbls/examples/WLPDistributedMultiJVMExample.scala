package oscar.cbls.examples

import oscar.cbls._
import oscar.cbls.core.computation.Store
import oscar.cbls.core.distributed.DistributedSearch
import oscar.cbls.core.distributed.protocol.ProblemStatement
import oscar.cbls.modeling.{Invariants => Inv, Neighborhoods => Nrs}
import oscar.cbls.algo.generator.WarehouseLocationGenerator
import oscar.cbls.lib.neighborhoods.combinator.distributed.DistributedModulo
import oscar.cbls.lib.neighborhoods.combinator.distributed.DistributedModulo.ModuloRange
import oscar.cbls.util.ArgParser

/** Problem statement for the Warehouse Location Problem.
  * Sent by the supervisor to worker nodes so they can build their local models.
  *
  * @param fixedCosts
  *   the fixed cost for opening each warehouse
  * @param warehousesPositions
  *   the (x, y) positions of each warehouse
  * @param deliveryPositions
  *   the (x, y) positions of each delivery point
  * @param distanceMatrix
  *   the distance matrix: distanceMatrix(d)(w) = distance from delivery d to warehouse w
  * @param nbParal
  *   the number of parallel neighborhoods to create
  */
case class WLPProblemStatement(
                                fixedCosts: Array[Long],
                                warehousesPositions: Array[(Long, Long)],
                                deliveryPositions: Array[(Long, Long)],
                                distanceMatrix: Array[Array[Long]],
                                nbParal: Int = 10
                              ) extends ProblemStatement {

  override def buildLocalSearchModel(): (Store, Objective, Seq[Neighborhood]) = {
    val nbFacilities = fixedCosts.length
    val deliveryPoints = this.deliveryPositions.length

    implicit val m: Model = model("WLP Distributed")

    val facilitiesVariables = Array.tabulate(nbFacilities)(f =>
      binaryVar(0, name = s"facility_${f}_open")
    )

    val openFacilities = Inv.logic.filter(facilitiesVariables, name = "Set of open facilities")

    val distancesToNearestOpenFacility = Array.tabulate(deliveryPoints)(d =>
      Inv.minMax.minOfConstants(
        distanceMatrix(d),
        openFacilities,
        name = s"Distance of $d to nearest facility"
      )
    )

    val objExpr = sum(distancesToNearestOpenFacility) +
      partialSumOfConstants(fixedCosts, indices = openFacilities)

    val obj = m.minimize(objExpr)
    m.close()

    val search: Neighborhood =
      DistributedModulo((p: ModuloRange) =>
        Nrs.combinator.exhaust(
          List(
            Nrs.assign(
              facilitiesVariables,
              searchZone = Some(() => p.offset until nbFacilities by p.step),
              hotRestart = false,
              name = s"assignModulo(${p.step},${p.offset})"
            ),
            Nrs.swap(
              facilitiesVariables,
              firstSearchZone = Some(() => p.offset until nbFacilities by p.step),
              name = s"swapModulo(${p.step},${p.offset})",
              hotRestart = false
            )
          )
        )
      )

    (m.store, obj, Seq(search))
  }
}

/**
 * Supervisor node for distributed WLP optimization.
 * Run this first, then start worker nodes.
 *
 * The supervisor generates the problem data and sends it to worker nodes
 * via the WLPProblemStatement message.
 *
 * Command line arguments:
 *   -h <host>         Supervisor host (default: 127.0.0.1)
 *   -p <port>         Supervisor port (default: 2551)
 *   -w <localWorkers> Number of local workers (default: 2)
 *   -d <delay>        Time in seconds to wait for remote workers (default: 10)
 */
object WLPSupervisorNode {

  // Problem parameters
  val nbFacilities   = 300
  val deliveryPoints = 1000
  private val nbParal        = 10

  // Generate problem data (seed for reproducibility)
  val (fixedCosts, warehousesPositions, deliveryPositions, distanceMatrix, _) =
    WarehouseLocationGenerator.generateRandomWLP(nbFacilities, deliveryPoints, seed = 42L)

  // Create the problem statement to be sent to worker nodes
  val problemStatement: WLPProblemStatement = WLPProblemStatement(
    fixedCosts,
    warehousesPositions,
    deliveryPositions,
    distanceMatrix,
    nbParal
  )

  def main(args: Array[String]): Unit = {
    val parsedArgs = ArgParser.parse(args)

    val supervisorHost = parsedArgs.getOrElse("-h", "127.0.0.1")
    val supervisorPort = parsedArgs.get("-p").map(_.toInt).getOrElse(2551)
    val localWorkers = parsedArgs.get("-w").map(_.toInt).getOrElse(2)
    val delay = parsedArgs.get("-d").map(_.toInt).getOrElse(10)

    println(s"Starting WLP Supervisor on $supervisorHost:$supervisorPort")
    println(s"Local workers: $localWorkers")
    println(s"Delay for remote workers: ${delay}s")

    // Create supervisor's model and search using the problem statement
    val (store, obj, searches) = problemStatement.buildLocalSearchModel()
    val search = searches.head

    // Create distributed search in cluster mode, with problem statement for remote workers
    val distributedSearch = DistributedSearch(
      store,
      verbose = 1,
      distributed = true,
      problemStatement = Some(problemStatement),
      supervisorHost = supervisorHost,
      supervisorPort = supervisorPort,
      search = searches: _*
    )

    println(s"Supervisor address: ${distributedSearch.supervisorAddress}")
    println("Waiting for workers to connect...")

    // Optionally spawn local workers
    for (_ <- 0 until localWorkers) {
      val (workerStore, _, workerSearches) = problemStatement.buildLocalSearchModel()
      distributedSearch.spawnLocalWorker(workerStore, workerSearches.head)
    }

    // Wait for remote workers to connect
    Thread.sleep(delay * 1000L)

    // Run optimization
    println("Starting optimization...")
    search.verbosityLevel = 1
    val moves = search.doAllMoves(obj)

    println(s"Optimization complete. Total moves: $moves")
    println(s"Final objective: ${obj.objValue.value()}")

    distributedSearch.globalShutDown()
  }
}

/**
 * Worker node for distributed WLP optimization.
 * Connect to a running supervisor.
 *
 * Each worker receives problem data from the supervisor via WLPProblemStatement
 * and creates its local model and search from that data using buildLocalSearchModel().
 *
 * Command line arguments:
 *   -h <workerHost>     Worker host (default: 127.0.0.1)
 *   -s <supervisorHost> Supervisor host (default: 127.0.0.1)
 *   -p <supervisorPort> Supervisor port (default: 2551)
 *   -w <nbWorkers>      Number of workers (default: availableProcessors / 4)
 */
object WLPWorkerNode {

  def main(args: Array[String]): Unit = {
    val parsedArgs = ArgParser.parse(args)

    val workerHost = parsedArgs.getOrElse("-h", "127.0.0.1")
    val supervisorHost = parsedArgs.getOrElse("-s", "127.0.0.1")
    val supervisorPort = parsedArgs.get("-p").map(_.toInt).getOrElse(2551)
    val nbWorkers = parsedArgs.get("-w").map(_.toInt).getOrElse(
      Runtime.getRuntime.availableProcessors() / 4
    )

    println(s"Starting $nbWorkers workers on $workerHost, connecting to $supervisorHost:$supervisorPort")

    // Worker node that receives the problem statement and uses buildLocalSearchModel()
    val workerNode = DistributedSearch(
      supervisorHost = supervisorHost,
      supervisorPort = supervisorPort,
      workerHost = workerHost,
      nbWorkers = nbWorkers,
      verbose = 1
    )

    println("Workers started. Waiting for problem data from supervisor...")
    workerNode.awaitTermination()
  }
}