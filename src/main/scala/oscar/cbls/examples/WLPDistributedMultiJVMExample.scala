package oscar.cbls.examples

import oscar.cbls._
import oscar.cbls.core.computation.Store
import oscar.cbls.core.distributed.DistributedSearch
import oscar.cbls.core.distributed.protocol.ProblemStatement
import oscar.cbls.modeling.{Invariants => Inv, Neighborhoods => Nrs}
import oscar.cbls.algo.generator.WarehouseLocationGenerator
import oscar.cbls.lib.neighborhoods.combinator.distributed.DistributedModulo
import oscar.cbls.lib.neighborhoods.combinator.distributed.DistributedModulo.ModuloRange

import scala.concurrent.duration.DurationInt

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
 * The supervisor generates the problem data from its constructor parameters and sends it to
 * worker nodes via the WLPProblemStatement message.
 *
 * @param nbFacilities
 *   the number of facilities (warehouses) that can be opened
 * @param deliveryPoints
 *   the number of delivery points to serve
 * @param nbParal
 *   the number of parallel neighborhoods to create
 * @param seed
 *   random seed used to generate the problem data (for reproducibility)
 */
class WLPSupervisorNode(
                         val nbFacilities: Int,
                         val deliveryPoints: Int,
                         val nbParal: Int,
                         val seed: Int
                       ) {

  // Generate problem data (seed for reproducibility)
  val (fixedCosts, warehousesPositions, deliveryPositions, distanceMatrix, _) =
    WarehouseLocationGenerator.generateRandomWLP(
      nbFacilities,
      deliveryPoints,
      seed = Some(seed.toLong)
    )

  // Create the problem statement to be sent to worker nodes
  val problemStatement: WLPProblemStatement = WLPProblemStatement(
    fixedCosts,
    warehousesPositions,
    deliveryPositions,
    distanceMatrix,
    nbParal
  )

  /** Starts the supervisor and runs the distributed optimization until completion.
    *
    * @param supervisorHost
    *   the host (IP address) the supervisor listens on
    * @param supervisorPort
    *   the port the supervisor listens on
    * @param localWorkers
    *   the number of workers spawned inside the supervisor's own JVM
    * @param nbWorkersToWaitFor
    *   the number of registered workers, local and remote ones alike, that must be there before
    *   the search starts
    * @param maxWaitInSeconds
    *   the maximal time in seconds to wait for these workers; after that the search starts with
    *   whatever workers did register
    */
  def run(
    supervisorHost: String,
    supervisorPort: Int,
    localWorkers: Int,
    nbWorkersToWaitFor: Int,
    maxWaitInSeconds: Int
  ): Unit = {
    println(s"Starting WLP Supervisor on $supervisorHost:$supervisorPort")
    println(s"Problem: facilities=$nbFacilities, deliveryPoints=$deliveryPoints, " +
      s"nbParal=$nbParal, seed=$seed")
    println(s"Local workers: $localWorkers")
    println(s"Waiting for $nbWorkersToWaitFor workers, at most ${maxWaitInSeconds}s")

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

    // Optionally spawn local workers
    for (_ <- 0 until localWorkers) {
      val (workerStore, _, workerSearches) = problemStatement.buildLocalSearchModel()
      distributedSearch.spawnLocalWorker(workerStore, workerSearches.head)
    }

    // Wait for the workers to register to the supervisor, rather than sleeping and hoping that
    // the remote ones showed up in the meantime.
    println("Waiting for workers to connect...")
    val nbConnectedWorkers =
      distributedSearch.waitForWorkers(nbWorkersToWaitFor, maxWaitInSeconds.seconds)
    if (nbConnectedWorkers < nbWorkersToWaitFor) {
      println(
        s"Only $nbConnectedWorkers workers out of $nbWorkersToWaitFor registered within " +
          s"${maxWaitInSeconds}s; starting the search anyway"
      )
    } else {
      println(s"$nbConnectedWorkers workers registered")
    }

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
 * Standalone entry point running [[WLPSupervisorNode]] with the default problem instance.
 * Use [[MultiJVMExampleRunner]] to choose the problem parameters from command line.
 *
 * Run it with `--help` to get the list of the supported options; see [[SupervisorNodeArgs]].
 */
object WLPSupervisorNode {

  // Default problem parameters
  val defaultNbFacilities: Int   = 300
  val defaultDeliveryPoints: Int = 1000
  val defaultNbParal: Int        = 10
  val defaultSeed: Int           = 42

  def main(args: Array[String]): Unit = {
    SupervisorNodeArgs.parse("WLPSupervisorNode", args) match {
      case None => CliExit.onParseFailure(args)
      case Some(parsedArgs) =>
        new WLPSupervisorNode(
          defaultNbFacilities,
          defaultDeliveryPoints,
          defaultNbParal,
          defaultSeed
        ).run(
          supervisorHost = parsedArgs.supervisorHost,
          supervisorPort = parsedArgs.supervisorPort,
          localWorkers = parsedArgs.localWorkers,
          nbWorkersToWaitFor = parsedArgs.nbWorkersToWaitFor,
          maxWaitInSeconds = parsedArgs.delay
        )
    }
  }
}

/**
 * Worker node for distributed WLP optimization.
 * Connect to a running supervisor.
 *
 * Each worker receives problem data from the supervisor via WLPProblemStatement
 * and creates its local model and search from that data using buildLocalSearchModel().
 *
 * Run it with `--help` to get the list of the supported options; see [[WorkerNodeArgs]].
 */
object WLPWorkerNode {

  /** Default number of workers spawned in this JVM. */
  def defaultNbWorkers: Int = Runtime.getRuntime.availableProcessors() / 4

  /** Starts the worker node and blocks until the supervisor shuts it down.
    *
    * @param workerHost
    *   the host (IP address) this worker node listens on
    * @param supervisorHost
    *   the host (IP address) of the supervisor to connect to
    * @param supervisorPort
    *   the port of the supervisor to connect to
    * @param nbWorkers
    *   the number of workers to spawn in this JVM
    */
  def run(
    workerHost: String,
    supervisorHost: String,
    supervisorPort: Int,
    nbWorkers: Int
  ): Unit = {
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

  def main(args: Array[String]): Unit = {
    WorkerNodeArgs.parse("WLPWorkerNode", args, defaultNbWorkers) match {
      case None => CliExit.onParseFailure(args)
      case Some(parsedArgs) =>
        run(
          parsedArgs.workerHost,
          parsedArgs.supervisorHost,
          parsedArgs.supervisorPort,
          parsedArgs.nbWorkers
        )
    }
  }
}