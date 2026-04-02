package oscar.cbls.core.distributed.actors

import org.apache.pekko.actor.typed.scaladsl.adapter._
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import oscar.cbls.core.distributed.computation.SearchConnector
import oscar.cbls.core.distributed.protocol._

import scala.collection.parallel.CollectionConverters._
import scala.concurrent.duration._

/**
 * Guardian actor for the worker node.
 * Resolves supervisor reference, registers to receive problem data, and spawns workers.
 * When all spawned workers terminate (e.g., due to global shutdown), the WorkerNode terminates as well.
 */
object WorkerNode {

  /**
   * Creates a WorkerNode that receives problem data (ProblemStatement) from the supervisor
   * and uses ProblemStatement.buildLocalSearchModel() to create models.
   *
   * @param nbWorkers Number of workers to spawn
   * @param verbose Verbosity level
   * @param supervisorHost Supervisor hostname
   * @param supervisorPort Supervisor port
   */
  def apply(
             nbWorkers: Int,
             verbose: Int,
             supervisorHost: String,
             supervisorPort: Int
           ): Behavior[MessageToWorkerNode] = {
    Behaviors.setup[MessageToWorkerNode] { context =>
      context.log.info(s"Worker node starting, connecting to supervisor at $supervisorHost:$supervisorPort")
      // Resolve supervisor ActorRef using ActorSelection
      val supervisorPath = s"pekko://Supervisor@$supervisorHost:$supervisorPort/user"
      // IMPORTANT: Send StartWorkers to self to trigger supervisor resolution
      context.self ! StartWorkers
      connecting(nbWorkers, verbose, supervisorPath)
    }
  }

  private def connecting(
                          nbWorkers: Int,
                          verbose: Int,
                          supervisorPath: String
                        ): Behavior[MessageToWorkerNode] = {
    Behaviors.receive { (context, message) =>
      message match {
        case StartWorkers =>
          import context.executionContext
          val supervisorSelection = context.system.classicSystem.actorSelection(supervisorPath)
          supervisorSelection.resolveOne(30.seconds).foreach { classicRef =>
            val typedRef = classicRef.toTyped[MessageToSupervisor]
            context.self ! SupervisorResolved(typedRef)
          }
          Behaviors.same

        case SupervisorResolved(supervisorRef) =>
          context.log.info(s"Supervisor resolved, registering to receive problem data")
          // Register with supervisor to receive problem statement
          supervisorRef ! WorkerNodeRegister(context.self, nbWorkers)
          waitingForProblem(nbWorkers, verbose, supervisorRef)

        case ShutdownNode =>
          Behaviors.stopped

        case _ =>
          Behaviors.same
      }
    }
  }

  private def waitingForProblem(
                                 nbWorkers: Int,
                                 verbose: Int,
                                 supervisorRef: ActorRef[MessageToSupervisor]
                               ): Behavior[MessageToWorkerNode] = {
    Behaviors.receive { (context, message) =>
      message match {
        case problem: ProblemStatement =>
          context.log.info(s"Problem statement received, spawning $nbWorkers workers")
          val spawnedWorkers = spawnWorkers(context, problem, nbWorkers, verbose, supervisorRef)
          running(spawnedWorkers)

        case ShutdownNode =>
          Behaviors.stopped

        case _ =>
          Behaviors.same
      }
    }
  }

  private def spawnWorkers(
                            context: ActorContext[MessageToWorkerNode],
                            problem: ProblemStatement,
                            nbWorkers: Int,
                            verbose: Int,
                            supervisorRef: ActorRef[MessageToSupervisor]
                          ): Set[ActorRef[MessageToWorker]] = {
    // Parallelizing building local search models
    val models = (0 until nbWorkers).par.map { i =>
      (i, problem.buildLocalSearchModel())
    }.seq
    // For safety, spawning of actors is sequential
    models.map { case (i, (store, _, searches)) =>
      val searchConnector = new SearchConnector(store, searches.toList)
      searchConnector.supervisor = supervisorRef
      val workerRef = context.spawn(
        Worker(
          supervisorRef,
          s"RemoteWorker-${context.system.address.hostPort}-$i",
          searchConnector,
          verbose,
          None,
          workerNode = Some(context.self)
        ),
        s"Worker$i"
      )
      // Watch the worker to detect when it terminates
      context.watchWith(workerRef, NodeWorkerTerminated(workerRef))
      workerRef
    }.toSet
  }

  private def running(activeWorkers: Set[ActorRef[MessageToWorker]]): Behavior[MessageToWorkerNode] = {
    Behaviors.receive { (context, message) =>
      message match {
        case ShutdownNode =>
          Behaviors.stopped

        case NodeWorkerTerminated(worker) =>
          val remainingWorkers = activeWorkers - worker
          context.log.info(s"Worker $worker terminated (${remainingWorkers.size} workers remaining)")
          if (remainingWorkers.isEmpty) {
            context.log.info("All workers terminated, shutting down WorkerNode")
            Behaviors.stopped
          } else {
            running(remainingWorkers)
          }

        case _ =>
          Behaviors.same
      }
    }
  }
}