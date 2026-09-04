package oscar.cbls.examples

import scopt.OParser

/** The command line configuration of [[MultiJVMExampleRunner]].
  *
  * @param problem
  *   the problem to solve, either `wlp` or `vrp`
  * @param role
  *   the kind of node to launch, either `supervisor` or `worker`
  * @param seed
  *   the random seed used to generate the problem data
  * @param vehicles
  *   VRP only: the number of vehicles `v`
  * @param nodes
  *   VRP only: the number of points `n`, vehicles included
  * @param distance
  *   VRP only: the distance `d` between two consecutive generated nodes
  * @param facilities
  *   WLP only: the number of facilities
  * @param deliveryPoints
  *   WLP only: the number of delivery points
  * @param parallel
  *   WLP only: the number of parallel neighborhoods to create
  * @param host
  *   supervisor only: the IP address the supervisor listens on
  * @param port
  *   supervisor only: the port the supervisor listens on
  * @param minWorkers
  *   supervisor only: the number of workers to wait for before starting the search; defaults to the
  *   number of local workers
  * @param delay
  *   supervisor only: the maximal number of seconds to wait for these workers
  * @param supervisorHost
  *   worker only: the IP address of the supervisor to connect to
  * @param supervisorPort
  *   worker only: the port of the supervisor to connect to
  * @param workerHost
  *   worker only: the IP address this worker node listens on
  * @param workers
  *   the number of workers to spawn in this JVM; the default depends on the role
  */
case class MultiJVMExampleConfig(
  problem: String = "",
  role: String = "",
  seed: Int = 42,
  vehicles: Int = VRPPopulationSupervisorNode.defaultV,
  nodes: Int = VRPPopulationSupervisorNode.defaultN,
  distance: Int = VRPPopulationSupervisorNode.defaultD,
  facilities: Int = WLPSupervisorNode.defaultNbFacilities,
  deliveryPoints: Int = WLPSupervisorNode.defaultDeliveryPoints,
  parallel: Int = WLPSupervisorNode.defaultNbParal,
  host: String = "127.0.0.1",
  port: Int = 2551,
  minWorkers: Option[Int] = None,
  delay: Int = 10,
  supervisorHost: String = "127.0.0.1",
  supervisorPort: Int = 2551,
  workerHost: String = "127.0.0.1",
  workers: Option[Int] = None
) {

  /** The number of workers to spawn in this JVM, using the default of the requested role when the
    * option was not given on the command line.
    */
  def nbWorkers: Int = workers.getOrElse {
    if (isSupervisor) MultiJVMExampleRunner.defaultLocalWorkers
    else if (problem == "vrp") VRPPopulationWorkerNode.defaultNbWorkers
    else WLPWorkerNode.defaultNbWorkers
  }

  /** The number of registered workers the supervisor waits for before starting the search. */
  def nbWorkersToWaitFor: Int = minWorkers.getOrElse(nbWorkers)

  def isSupervisor: Boolean = role == "supervisor"
}

/** Single entry point launching either node of the multi-JVM distributed examples.
  *
  * It replaces the four standalone `main` methods of `VRPDistributedPopulationBasedMultiJVMExample`
  * and `WLPDistributedMultiJVMExample` by one runnable object where the problem instance, the role
  * of the node (supervisor or worker) and the network setup are all given on the command line.
  *
  * It is the main class of the standalone jar built by `sbt assembly`:
  * {{{
  * java -jar target/scala-2.13/oscar-cbls-examples-<version>.jar --help
  * }}}
  *
  * ==Usage==
  * {{{
  * MultiJVMExampleRunner --problem <wlp|vrp> --role <supervisor|worker> [options]
  * }}}
  *
  * Run it with `--help` to get the full list of options.
  *
  * ==Examples==
  * {{{
  * # VRP supervisor, 60 nodes, 3 vehicles, 2 local workers, listening on port 2551
  * MultiJVMExampleRunner --problem vrp --role supervisor \
  *   --vehicles 3 --nodes 60 --distance 100 --seed 42 \
  *   --workers 2 --host 127.0.0.1 --port 2551
  *
  * # WLP supervisor waiting for 8 workers (2 local ones plus 6 remote ones) for at most 30s
  * MultiJVMExampleRunner --problem wlp --role supervisor \
  *   --facilities 300 --delivery-points 1000 --parallel 10 --seed 42 \
  *   --workers 2 --min-workers 8 --delay 30 --host 127.0.0.1 --port 2551
  *
  * # Worker node connecting to a remote supervisor (same for both problems)
  * MultiJVMExampleRunner --problem wlp --role worker \
  *   --supervisor-host 10.0.0.5 --supervisor-port 2551 --worker-host 10.0.0.7
  * }}}
  */
object MultiJVMExampleRunner {

  /** Default number of workers spawned inside the supervisor's own JVM. */
  val defaultLocalWorkers: Int = 2

  private val builder = OParser.builder[MultiJVMExampleConfig]

  private val parser: OParser[_, MultiJVMExampleConfig] = {
    import builder._
    OParser.sequence(
      programName("MultiJVMExampleRunner"),
      head("MultiJVMExampleRunner", "launches one node of a multi-JVM OscaR-CBLS example"),
      opt[String]("problem")
        .required()
        .valueName("<wlp|vrp>")
        .validate(p =>
          if (Set("wlp", "vrp").contains(p.toLowerCase)) success
          else failure(s"Unknown problem '$p', expected 'wlp' or 'vrp'")
        )
        .action((p, c) => c.copy(problem = p.toLowerCase))
        .text("problem to solve"),
      opt[String]("role")
        .required()
        .valueName("<supervisor|worker>")
        .validate(r =>
          if (Set("supervisor", "s", "worker", "w").contains(r.toLowerCase)) success
          else failure(s"Unknown role '$r', expected 'supervisor' or 'worker'")
        )
        .action((r, c) =>
          c.copy(role = if (Set("supervisor", "s").contains(r.toLowerCase)) "supervisor"
          else "worker")
        )
        .text("kind of node to launch"),
      opt[Int]("seed")
        .action((s, c) => c.copy(seed = s))
        .text("random seed for the problem data (default: 42)"),
      note(""),
      note("VRP problem options (supervisor only):"),
      opt[Int]("vehicles")
        .validate(v => if (v > 0) success else failure("--vehicles must be strictly positive"))
        .action((v, c) => c.copy(vehicles = v))
        .text(s"number of vehicles v (default: ${VRPPopulationSupervisorNode.defaultV})"),
      opt[Int]("nodes")
        .validate(n => if (n > 0) success else failure("--nodes must be strictly positive"))
        .action((n, c) => c.copy(nodes = n))
        .text(
          "number of points n, vehicles included " +
            s"(default: ${VRPPopulationSupervisorNode.defaultN})"
        ),
      opt[Int]("distance")
        .action((d, c) => c.copy(distance = d))
        .text(
          s"distance d between generated nodes (default: ${VRPPopulationSupervisorNode.defaultD})"
        ),
      note(""),
      note("WLP problem options (supervisor only):"),
      opt[Int]("facilities")
        .action((f, c) => c.copy(facilities = f))
        .text(s"number of facilities (default: ${WLPSupervisorNode.defaultNbFacilities})"),
      opt[Int]("delivery-points")
        .action((d, c) => c.copy(deliveryPoints = d))
        .text(s"number of delivery points (default: ${WLPSupervisorNode.defaultDeliveryPoints})"),
      opt[Int]("parallel")
        .action((p, c) => c.copy(parallel = p))
        .text(s"number of parallel neighborhoods (default: ${WLPSupervisorNode.defaultNbParal})"),
      note(""),
      note("Supervisor options:"),
      opt[String]("host")
        .action((h, c) => c.copy(host = h))
        .text("IP the supervisor listens on (default: 127.0.0.1)"),
      opt[Int]("port")
        .action((p, c) => c.copy(port = p))
        .text("port the supervisor listens on (default: 2551)"),
      opt[Int]("min-workers")
        .action((w, c) => c.copy(minWorkers = Some(w)))
        .text("number of workers to wait for before starting (default: --workers)"),
      opt[Int]("delay")
        .action((d, c) => c.copy(delay = d))
        .text("maximum number of seconds to wait for these workers (default: 10)"),
      note(""),
      note("Worker options:"),
      opt[String]("supervisor-host")
        .action((h, c) => c.copy(supervisorHost = h))
        .text("IP of the supervisor (default: 127.0.0.1)"),
      opt[Int]("supervisor-port")
        .action((p, c) => c.copy(supervisorPort = p))
        .text("port of the supervisor (default: 2551)"),
      opt[String]("worker-host")
        .action((h, c) => c.copy(workerHost = h))
        .text("IP this worker node listens on (default: 127.0.0.1)"),
      note(""),
      note("Common options:"),
      opt[Int]("workers")
        .validate(w => if (w >= 0) success else failure("--workers must be positive"))
        .action((w, c) => c.copy(workers = Some(w)))
        .text(
          s"number of workers to spawn in this JVM (default: $defaultLocalWorkers for a " +
            "supervisor, nbCores / 4 for a worker node)"
        ),
      help("help").text("prints this usage text")
    )
  }

  def main(args: Array[String]): Unit = {
    OParser.parse(parser, args, MultiJVMExampleConfig()) match {
      case None         => CliExit.onParseFailure(args)
      case Some(config) => run(config)
    }
  }

  private def run(config: MultiJVMExampleConfig): Unit = {
    (config.role, config.problem) match {
      case ("supervisor", "vrp") =>
        new VRPPopulationSupervisorNode(
          v = config.vehicles,
          n = config.nodes,
          d = config.distance,
          seed = config.seed
        ).run(
          supervisorHost = config.host,
          supervisorPort = config.port,
          localWorkers = config.nbWorkers,
          nbWorkersToWaitFor = config.nbWorkersToWaitFor,
          maxWaitInSeconds = config.delay
        )

      case ("supervisor", _) =>
        new WLPSupervisorNode(
          nbFacilities = config.facilities,
          deliveryPoints = config.deliveryPoints,
          nbParal = config.parallel,
          seed = config.seed
        ).run(
          supervisorHost = config.host,
          supervisorPort = config.port,
          localWorkers = config.nbWorkers,
          nbWorkersToWaitFor = config.nbWorkersToWaitFor,
          maxWaitInSeconds = config.delay
        )

      // A worker node builds its model from the problem statement sent by the supervisor,
      // so the two problems only differ by the default number of workers.
      case (_, "vrp") =>
        VRPPopulationWorkerNode.run(
          config.workerHost,
          config.supervisorHost,
          config.supervisorPort,
          config.nbWorkers
        )

      case _ =>
        WLPWorkerNode.run(
          config.workerHost,
          config.supervisorHost,
          config.supervisorPort,
          config.nbWorkers
        )
    }
  }
}
