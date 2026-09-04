package oscar.cbls.examples

import oscar.cbls._
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.objective.Minimize
import oscar.cbls.core.distributed.DistributedSearch
import oscar.cbls.core.distributed.protocol.ProblemStatement
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.lib.neighborhoods.combinator._
import oscar.cbls.lib.neighborhoods.combinator.distributed.DistributedPopulationBased
import oscar.cbls.lib.neighborhoods.routing._
import oscar.cbls.modeling.routing.VRS
import oscar.cbls.modeling.{Invariants => Inv}
import oscar.cbls.algo.generator.RoutingGenerator

import scala.concurrent.duration.DurationInt
import scala.util.Random

/** Problem statement for the Vehicle Routing Problem with DistributedPopulationBased search.
  * Sent by the supervisor to worker nodes so they can build their local models.
  *
  * @param n
  *   total number of nodes (vehicles + customers)
  * @param v
  *   number of vehicles
  * @param coordinates
  *   the (x, y) positions of each node
  * @param distances
  *   the distance matrix: distances(i)(j) = distance from node i to node j
  * @param initialSolutionSeed
  *   the random seed used to build the initial routes. It is part of the statement, and hence
  *   shipped to every worker node, so that the supervisor and all the workers start from the very
  *   same initial solution. Drawing it locally on each node would give each of them a different
  *   initial routing, and the solutions exchanged by the population-based search would then be
  *   evaluated against inconsistent models.
  */
case class VRPPopulationProblemStatement(
  n: Int,
  v: Int,
  coordinates: Array[(Long, Long)],
  distances: Array[Array[Long]],
  initialSolutionSeed: Long
) extends ProblemStatement {

  override def buildLocalSearchModel(): (Store, Objective, Seq[Neighborhood]) = {
    // Seeded from the statement: every node must build the exact same initial solution.
    val initialSolutionRandom = new Random(initialSolutionSeed)
    // Not seeded: this one only diversifies the local search, it does not affect the model, so
    // each node (and each neighborhood within a node) may follow its own random stream.
    val searchRandom = new Random()

    val model = new Store()
    val vrs   = VRS(model, n, v)

    // Insert half the non-vehicle nodes into routes (random initial state)
    var toInsert = initialSolutionRandom.shuffle(List.from(v until n)).take((n - v) / 2)
    while (toInsert.nonEmpty) {
      val node     = toInsert.head
      val explorer = vrs.routes.pendingValue.explorerAtPosition(0).get
      vrs.routes.insertAfterPosition(node, explorer)
      toInsert = toInsert.tail
    }

    // Objective: minimize route length + penalty for unrouted nodes
    val routeLength = Inv.routing.totalRouteLength(distances)(vrs)
    val nbUnrouted  = vrs.unrouted.size()
    val objective   = Minimize(routeLength + (nbUnrouted * 1000L))

    model.close()
    model.propagate()

    // Search: random remove then round-robin of move/insert
    def search: Neighborhood = Exhaust(
      MaxMoves(
        AcceptAll(
          RemovePoint(
            vrs,
            relevantNodesToRemove = () => {
              val allPoints = vrs.routedWithoutVehicles.value().toArray
              if (allPoints.nonEmpty) Some(allPoints(searchRandom.nextInt(allPoints.length)))
              else None
            }
          )
        ),
        1
      ),
      RoundRobin(
        Array(
          (
            OnePointMove(
              vrs,
              () => vrs.routedWithoutVehicles.pendingValue,
              (x: Int) =>
                vrs.routedWithVehicles.value().filter((node: Int) => node != x && node != x - 1),
              selectDestinationBehavior = LoopBehavior.best()
            ),
            1
          ),
          (
            InsertPointInsertionPointFirst(
              vrs,
              () => vrs.routedWithVehicles.value(),
              (_: Int) => vrs.unroutedNodes,
              selectInsertionPointBehavior = LoopBehavior.best()
            ),
            1
          )
        )
      )
    )

    // 3 neighborhoods per individual, keeping 4 best, 10 iterations
    val distributedSearch = DistributedPopulationBased.simple(
      neighborhoods = Array(search, search, search),
      step = (it, _) => if (it < 10) Some(4) else None,
      store = model,
      saveAnytimeBest = false,
      keepOld = false
    )

    (model, objective, Seq(distributedSearch))
  }
}

/** Supervisor node for distributed VRP population-based optimization.
  * Run this first, then start worker nodes.
  *
  * The problem data is generated from the constructor parameters and sent to the worker nodes
  * via a [[VRPPopulationProblemStatement]] message.
  *
  * @param v
  *   number of vehicles
  * @param n
  *   total number of nodes (vehicles + customers)
  * @param d
  *   distance between two consecutive generated nodes
  * @param seed
  *   random seed used to generate the node coordinates and the initial routes (for reproducibility)
  */
class VRPPopulationSupervisorNode(val v: Int, val n: Int, val d: Int, val seed: Int) {

  // Generate problem data (seed for reproducibility)
  val (coordinates, distances, _, _) =
    RoutingGenerator.generateRandomRoutingData(n, 2, 0, seed = Some(seed))

  // Create the problem statement to be sent to worker nodes. The seed of the initial solution
  // travels with it, so that every node builds the same initial routing.
  val problemStatement: VRPPopulationProblemStatement = VRPPopulationProblemStatement(
    n, v, coordinates, distances, seed.toLong
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
    println(s"Starting VRP Population Supervisor on $supervisorHost:$supervisorPort")
    println(s"Problem: v=$v, n=$n, d=$d, seed=$seed")
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

/** Standalone entry point running [[VRPPopulationSupervisorNode]] with the default problem
  * instance. Use [[MultiJVMExampleRunner]] to choose the problem parameters from command line.
  *
  * Run it with `--help` to get the list of the supported options; see [[SupervisorNodeArgs]].
  */
object VRPPopulationSupervisorNode {

  // Default problem parameters
  val defaultV: Int    = 2
  val defaultN: Int    = 50
  val defaultD: Int    = 100
  val defaultSeed: Int = 42

  def main(args: Array[String]): Unit = {
    SupervisorNodeArgs.parse("VRPPopulationSupervisorNode", args) match {
      case None => CliExit.onParseFailure(args)
      case Some(parsedArgs) =>
        new VRPPopulationSupervisorNode(defaultV, defaultN, defaultD, defaultSeed)
          .run(
            supervisorHost = parsedArgs.supervisorHost,
            supervisorPort = parsedArgs.supervisorPort,
            localWorkers = parsedArgs.localWorkers,
            nbWorkersToWaitFor = parsedArgs.nbWorkersToWaitFor,
            maxWaitInSeconds = parsedArgs.delay
          )
    }
  }
}

/** Worker node for distributed VRP population-based optimization.
  * Connect to a running supervisor.
  *
  * Each worker receives problem data from the supervisor via VRPPopulationProblemStatement
  * and creates its local model and search from that data using buildLocalSearchModel().
  *
  * Run it with `--help` to get the list of the supported options; see [[WorkerNodeArgs]].
  */
object VRPPopulationWorkerNode {

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
    WorkerNodeArgs.parse("VRPPopulationWorkerNode", args, defaultNbWorkers) match {
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
