package oscar.cbls.examples

import scopt.OParser

/** Shared exit path of the runnable multi-JVM examples, for the case where scopt did not produce a
  * configuration.
  */
object CliExit {

  /** Terminates the JVM after scopt failed to produce a configuration. scopt has already printed
    * either the usage text, when `--help` was asked, or the reason why the command line was
    * rejected; only the exit code differentiates the two cases.
    *
    * @param args
    *   the command line arguments that were given to scopt
    */
  def onParseFailure(args: Array[String]): Nothing =
    sys.exit(if (args.contains("--help")) 0 else 1)
}

/** The command line arguments shared by the standalone `main` of every supervisor node example.
  *
  * @param supervisorHost
  *   the IP address the supervisor listens on
  * @param supervisorPort
  *   the port the supervisor listens on
  * @param localWorkers
  *   the number of workers spawned inside the supervisor's own JVM
  * @param minWorkers
  *   the number of registered workers to wait for before starting the search; defaults to
  *   `localWorkers`, i.e. no remote worker is expected
  * @param delay
  *   the maximal number of seconds to wait for these workers
  */
case class SupervisorNodeArgs(
  supervisorHost: String = "127.0.0.1",
  supervisorPort: Int = 2551,
  localWorkers: Int = 2,
  minWorkers: Option[Int] = None,
  delay: Int = 10
) {

  /** The number of registered workers the supervisor waits for before starting the search. */
  def nbWorkersToWaitFor: Int = minWorkers.getOrElse(localWorkers)
}

object SupervisorNodeArgs {

  /** Parses the command line of a supervisor node example.
    *
    * @param exampleName
    *   the name of the example, used in the usage text
    * @param args
    *   the command line arguments
    * @return
    *   the parsed arguments, or `None` when the command line is invalid or `--help` was asked, in
    *   which case scopt already printed what the user needs to read
    */
  def parse(exampleName: String, args: Array[String]): Option[SupervisorNodeArgs] = {
    val builder = OParser.builder[SupervisorNodeArgs]
    val parser = {
      import builder._
      OParser.sequence(
        programName(exampleName),
        head(exampleName, "supervisor node of a multi-JVM OscaR-CBLS example"),
        opt[String]('h', "host")
          .action((h, a) => a.copy(supervisorHost = h))
          .text("IP the supervisor listens on (default: 127.0.0.1)"),
        opt[Int]('p', "port")
          .action((p, a) => a.copy(supervisorPort = p))
          .text("port the supervisor listens on (default: 2551)"),
        opt[Int]('w', "workers")
          .validate(w => if (w >= 0) success else failure("--workers must be positive"))
          .action((w, a) => a.copy(localWorkers = w))
          .text("number of local workers (default: 2)"),
        opt[Int]("min-workers")
          .action((w, a) => a.copy(minWorkers = Some(w)))
          .text("number of workers to wait for before starting (default: --workers)"),
        opt[Int]('d', "delay")
          .action((d, a) => a.copy(delay = d))
          .text("maximum number of seconds to wait for these workers (default: 10)"),
        help("help").text("prints this usage text")
      )
    }
    OParser.parse(parser, args, SupervisorNodeArgs())
  }
}

/** The command line arguments shared by the standalone `main` of every worker node example.
  *
  * @param workerHost
  *   the IP address this worker node listens on
  * @param supervisorHost
  *   the IP address of the supervisor to connect to
  * @param supervisorPort
  *   the port of the supervisor to connect to
  * @param nbWorkers
  *   the number of workers to spawn in this JVM
  */
case class WorkerNodeArgs(
  workerHost: String = "127.0.0.1",
  supervisorHost: String = "127.0.0.1",
  supervisorPort: Int = 2551,
  nbWorkers: Int = 1
)

object WorkerNodeArgs {

  /** Parses the command line of a worker node example.
    *
    * @param exampleName
    *   the name of the example, used in the usage text
    * @param args
    *   the command line arguments
    * @param defaultNbWorkers
    *   the number of workers to spawn when `--workers` is not given
    * @return
    *   the parsed arguments, or `None` when the command line is invalid or `--help` was asked, in
    *   which case scopt already printed what the user needs to read
    */
  def parse(
    exampleName: String,
    args: Array[String],
    defaultNbWorkers: Int
  ): Option[WorkerNodeArgs] = {
    val builder = OParser.builder[WorkerNodeArgs]
    val parser = {
      import builder._
      OParser.sequence(
        programName(exampleName),
        head(exampleName, "worker node of a multi-JVM OscaR-CBLS example"),
        opt[String]('h', "worker-host")
          .action((h, a) => a.copy(workerHost = h))
          .text("IP this worker node listens on (default: 127.0.0.1)"),
        opt[String]('s', "supervisor-host")
          .action((h, a) => a.copy(supervisorHost = h))
          .text("IP of the supervisor (default: 127.0.0.1)"),
        opt[Int]('p', "supervisor-port")
          .action((p, a) => a.copy(supervisorPort = p))
          .text("port of the supervisor (default: 2551)"),
        opt[Int]('w', "workers")
          .validate(w => if (w > 0) success else failure("--workers must be strictly positive"))
          .action((w, a) => a.copy(nbWorkers = w))
          .text(s"number of workers to spawn in this JVM (default: $defaultNbWorkers)"),
        help("help").text("prints this usage text")
      )
    }
    OParser.parse(parser, args, WorkerNodeArgs(nbWorkers = defaultNbWorkers))
  }
}
