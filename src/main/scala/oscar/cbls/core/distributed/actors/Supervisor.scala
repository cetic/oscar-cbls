package oscar.cbls.core.distributed.actors

import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.slf4j.{Logger, LoggerFactory}
import oscar.cbls.core.distributed.computation.SearchConnector
import oscar.cbls.core.distributed.protocol._
import oscar.cbls.core.distributed.search.TestBehavior

import scala.annotation.tailrec
import scala.util.Random

/** The supervisor actor that distributed the tasks among the available worker. IT also offers a few
  * auxiliary functionalities like spawning worker actors and spawning actors for distributed search
  * combinators. It is defined in details in the subclasses of [[MessageToSupervisor]]
  */
object Supervisor {

  /** Creates a Supervisor actor.
    * @param searchConnector
    *   the search connector that includes the store and search procedure at the supervisor side;
    *   where the main search procedure is executed
    * @param verbosityLevel
    *   the verbosity level for the whole distributed search, which is directed to the logger.
    *   - 0: nothing
    *   - 1: when workers join and leave
    *   - 2: 1 + all tasks start and ends
    * @return
    *   the behavior of the Supervisor
    */
  def apply(
    searchConnector: SearchConnector,
    verbosityLevel: Int,
    problemStatement: Option[ProblemStatement] = None
  ): Behavior[MessageToSupervisor] = {
    val supervisorLogger: Logger = LoggerFactory.getLogger("supervisor")
    Behaviors.setup[MessageToSupervisor](supervisorContext => {
      supervisorLogger.info("Supervisor started")
      searchConnector.supervisor = supervisorContext.self
      new Supervisor(searchConnector, supervisorLogger, verbosityLevel, problemStatement).initialize()
    })
  }
}

/** The class that includes the actor; it also includes a set of constants
  * @param computationSupport
  *   the computation support aof the main search procedure
  * @param log
  *   the logger for verbosity
  * @param verbosityLevel
  *   the verbosity level
  */
class Supervisor(
  computationSupport: SearchConnector,
  log: Logger,
  verbosityLevel: Int,
  problemStatement: Option[ProblemStatement] = None
) {
  val checkSumOnStoreStructure: Long = computationSupport.checkSumOnStoreAndSearch

  abstract sealed class TaskStatus(val c: CreateTask) {
    def assignedWorkers: Iterable[ActorRef[MessageToWorker]]
  }
  case class TaskEnqueued(
    override val c: CreateTask,
    workers: Set[ActorRef[MessageToWorker]] = Set.empty
  ) extends TaskStatus(c) {
    override def assignedWorkers: Iterable[ActorRef[MessageToWorker]] = workers
  }
  private case class TaskRunning(
    override val c: CreateTask,
    worker: ActorRef[MessageToWorker],
    enqueued: TaskEnqueued
  ) extends TaskStatus(c) {
    override def assignedWorkers: Iterable[ActorRef[MessageToWorker]] = Some(worker)
  }
  private case class TaskCancelled(
    override val c: CreateTask,
    workers: Set[ActorRef[MessageToWorker]]
  ) extends TaskStatus(c) {
    override def assignedWorkers: Iterable[ActorRef[MessageToWorker]] = None
  }

  case class State(
    currentSearchProcedure: Option[ActorRef[MessageToSearch]],
    taskIdToStatus: Map[Long, TaskStatus],
    workerStatus: Map[ActorRef[MessageToWorker], WorkerStatusReport],
    workerNodeToWorkers: Map[ActorRef[MessageToWorkerNode], Set[ActorRef[MessageToWorker]]] =
      Map.empty,
    terminatedWorkerNodes: Set[ActorRef[MessageToWorkerNode]] = Set.empty,
    nextTaskId: Long = 0,
    nextSearchId: Int = 0,
    nextLocalWorkerId: Int = 0
  ) {

    def incrementAllocatedWorkerId(): State = this.copy(nextLocalWorkerId = nextLocalWorkerId + 1)
    def incrementAllocateSearchId(): State  = this.copy(nextSearchId = nextSearchId + 1)
    def updateWorkerStatus(report: WorkerStatusReport): State = {
      copy(workerStatus = workerStatus + (report.fromWorker -> report))
    }

    def incrementStateId(nbIds: Int): (State, Long, Long) = {
      val firstTaskId   = nextTaskId
      val lastTaskId    = firstTaskId + nbIds
      val newNextTaskId = lastTaskId + 1
      (this.copy(nextTaskId = newNextTaskId), firstTaskId, lastTaskId)
    }

    def getAllTasksAssignedTo(worker: ActorRef[MessageToWorker]): List[Long] = {
      taskIdToStatus.values.toList
        .filter {
          case TaskEnqueued(_, workers)  => workers contains worker
          case TaskRunning(_, worker, _) => worker equals worker
          case TaskCancelled(_, _)       => false
        }
        .map(_.c.task.taskId)
    }

    def isCurrentSearchTheRightSearch(
      search: ActorRef[MessageToSearch],
      canBeNew: Boolean = false
    ): Boolean = {
      currentSearchProcedure match {
        case None                => canBeNew
        case Some(ongoingSearch) => ongoingSearch equals search
      }
    }

    def addNewTask(task: CreateTask): State = {
      this.copy(taskIdToStatus = taskIdToStatus + (task.task.taskId -> TaskEnqueued(task)))
    }

    def taskWasCancelledBySearch(taskId: Long): State = {
      this.copy(taskIdToStatus =
        taskIdToStatus + (taskId -> TaskCancelled(
          taskIdToStatus(taskId).c,
          taskIdToStatus(taskId).assignedWorkers.toSet
        ))
      )
    }

    def completeTaskCancellation(taskId: Long, fromWorker: ActorRef[MessageToWorker]): State = {
      taskIdToStatus.get(taskId) match {
        case Some(TaskCancelled(c, workers)) =>
          val newWorkers = workers - fromWorker
          if (newWorkers.isEmpty) {
            copy(taskIdToStatus = taskIdToStatus + (taskId -> TaskCancelled(c, newWorkers)))
          } else {
            copy(taskIdToStatus = taskIdToStatus - taskId)
          }
        case Some(TaskEnqueued(_, _)) =>
          // ignore this message
          log.error(
            "received complete task cancellation confirmation for non cancelled task, TaskEnqueued"
          )
          this
        case Some(TaskRunning(_, _, _)) =>
          log.error(
            "received complete task cancellation confirmation for non cancelled task, TaskRunning"
          )
          this
        case None => this
      }
    }

    def statusReport: StatusReport =
      StatusReport(
        nbWorkers = workerStatus.size,
        nbBusy = workerStatus.values.toList.count(_.workload != 0),
        nbIdle = workerStatus.values.toList.count(_.workload == 0)
      )

    def taskEnqueuedToWorker(task: TaskEnqueued, worker: ActorRef[MessageToWorker]): State = {
      taskIdToStatus(task.c.task.taskId) match {
        case t: TaskEnqueued =>
          this.copy(taskIdToStatus =
            taskIdToStatus + (task.c.task.taskId -> t.copy(workers = t.workers + worker))
          )
        case st => throw new Error(s"Invalid status for task ${task.c.task.taskId} ($st)")
      }

    }

    def isTaskAlreadyEnqueuedToWorker(
      task: CreateTask,
      worker: ActorRef[MessageToWorker]
    ): Boolean = {
      taskIdToStatus(task.task.taskId) match {
        case t: TaskEnqueued => t.assignedWorkers.exists(_.equals(worker))
        case _               => false
      }
    }

    def deleteWorkerAndTasks(disconnectedWorker: ActorRef[MessageToWorker]): State = {
      // delete all tasks associated to this worker
      val newTaskIdToStatus = if (workerStatus.contains(disconnectedWorker)) {
        // find all tasks that are enqueued to this worker
        taskIdToStatus.flatMap({
            case (taskId, t @ TaskEnqueued(c, workers)) =>
              if (workers contains disconnectedWorker) {
                // there could be zero workers after this; this is not an issue;
                // the 'ensureEveryoneHasWork' method starts with zero workers Enqueued
                Some((taskId, TaskEnqueued(c, workers - disconnectedWorker)))
              } else {
                Some((taskId, t))
              }
            case (taskId, t @ TaskRunning(_, worker, enqueued)) =>
              if (worker equals disconnectedWorker) {
                Some((taskId, enqueued))
              } else {
                Some((taskId, t))
              }
            case (taskId, t @ TaskCancelled(c, workers)) =>
              if (workers contains disconnectedWorker) {
                if (workers.size == 1) {
                  // no other enqueued worker; just complete the cancellation
                  None
                } else {
                  Some((taskId, TaskCancelled(c, workers - disconnectedWorker)))
                }
              } else {
                Some((taskId, t))
              }
          })
      } else {
        // unknown worker; nothing to do
        taskIdToStatus
      }

      // Remove the worker form the worker list
      val newWorkerStatus = workerStatus - disconnectedWorker

      // Also remove worker from workerNodeToWorkers mapping
      val newWorkerNodeToWorkers = workerNodeToWorkers.map { case (node, workers) =>
        node -> (workers - disconnectedWorker)
      }.filter(_._2.nonEmpty)

      this.copy(
        taskIdToStatus = newTaskIdToStatus,
        workerStatus = newWorkerStatus,
        workerNodeToWorkers = newWorkerNodeToWorkers
      )
    }

    /** Registers a worker as belonging to a specific WorkerNode.
      * @param workerNode the WorkerNode that owns the worker
      * @param worker the worker to register
      * @return the updated state
      */
    def registerWorkerToNode(
        workerNode: ActorRef[MessageToWorkerNode],
        worker: ActorRef[MessageToWorker]
    ): State = {
      val currentWorkers = workerNodeToWorkers.getOrElse(workerNode, Set.empty)
      this.copy(workerNodeToWorkers = workerNodeToWorkers + (workerNode -> (currentWorkers + worker)))
    }

    /** Checks if a worker belongs to a terminated WorkerNode.
      * @param worker the worker to check
      * @return true if the worker's WorkerNode has already been marked as terminated
      */
    def isWorkerFromTerminatedNode(worker: ActorRef[MessageToWorker]): Boolean = {
      workerNodeToWorkers.exists { case (node, workers) =>
        terminatedWorkerNodes.contains(node) && workers.contains(worker)
      }
    }

    /** Marks a WorkerNode as terminated and returns all workers that belong to it.
      * @param workerNode the WorkerNode that terminated
      * @return a tuple of (updated state, list of workers that belong to this node)
      */
    def markWorkerNodeTerminated(
        workerNode: ActorRef[MessageToWorkerNode]
    ): (State, List[ActorRef[MessageToWorker]]) = {
      val workers = workerNodeToWorkers.getOrElse(workerNode, Set.empty).toList
      val newState = this.copy(
        terminatedWorkerNodes = terminatedWorkerNodes + workerNode
      )
      (newState, workers)
    }

    /** Removes a WorkerNode and all its workers from the state.
      * @param workerNode the WorkerNode to remove
      * @return the updated state with all workers from this node removed
      */
    def deleteWorkerNodeAndWorkers(workerNode: ActorRef[MessageToWorkerNode]): State = {
      val workers = workerNodeToWorkers.getOrElse(workerNode, Set.empty)
      val stateAfterWorkerDeletion = workers.foldLeft(this) { (state, worker) =>
        state.deleteWorkerAndTasks(worker)
      }
      stateAfterWorkerDeletion.copy(
        workerNodeToWorkers = stateAfterWorkerDeletion.workerNodeToWorkers - workerNode,
        terminatedWorkerNodes = stateAfterWorkerDeletion.terminatedWorkerNodes - workerNode
      )
    }
  }

  private def initialize(): Behavior[MessageToSupervisor] = {
    if (verbosityLevel >= 1) log.info("initializing supervisor")
    idle(State(currentSearchProcedure = None, taskIdToStatus = Map.empty, workerStatus = Map.empty))
  }

  @tailrec
  private final def ensureEveryoneHasWork(state: State): State = {
    if (state.workerStatus.isEmpty) {
      // there is no worker at all
      return state
    }
    if (state.taskIdToStatus.isEmpty) {
      // no task to perform
      return state
    }

    // running tasks are not considered here
    val leastAssignedTasks: List[TaskEnqueued] =
      state.taskIdToStatus.values
        .flatMap {
          case e: TaskEnqueued => Some(e)
          case _               => None
        }
        .toList
        .sortBy(_.assignedWorkers.size)

    if (leastAssignedTasks.isEmpty) return state

    // do not use the workerStatusReport; use the TaskStatus!

    val workerAndTasksAndBusynessLevel =
      state.taskIdToStatus.values
        .flatMap(task => task.assignedWorkers.map(a => (task.c, a)))
        .groupBy(x => x._2)
        .toList
        .map({ case (worker, alist) => (worker, (alist.map(_._1), alist.size)) })
        .toMap

    val lessBusyWorker: List[(ActorRef[MessageToWorker], (Iterable[CreateTask], Int))] =
      state.workerStatus.keys.toList
        .sortBy(worker => worker)
        .map({ worker =>
          (worker, workerAndTasksAndBusynessLevel.getOrElse(worker, (List.empty, 0)))
        })
        .sortBy(_._2._2)

    if (lessBusyWorker.isEmpty) return state

    val nbQueuedTasksInLessBusyWorker = lessBusyWorker.head._2._2

    val minNbWorkersAssigned = leastAssignedTasks.head.assignedWorkers.size

    if (nbQueuedTasksInLessBusyWorker >= 2 || minNbWorkersAssigned >= 3) {
      // Every worker has at least one TODO, so we are fine
      // or the least assigned task is already given to three different workers;
      // pointless to assign it more because we will need to cancel it anyway
      // and might receive more tasks
      state
    } else {
      // some workers have not enough on their TODO-list

      if (minNbWorkersAssigned == 0 && nbQueuedTasksInLessBusyWorker == 0) {
        // assign in a zip fashion
        var potentiallyIDleWorkers     = Random.shuffle(lessBusyWorker)
        var potentiallyUnassignedTasks = Random.shuffle(leastAssignedTasks)
        var newState                   = state
        while (
          potentiallyIDleWorkers.nonEmpty && potentiallyIDleWorkers.head._2._2 == 0 &&
          potentiallyUnassignedTasks.nonEmpty && potentiallyUnassignedTasks.head.assignedWorkers.isEmpty
        ) {
          if (
            !newState.isTaskAlreadyEnqueuedToWorker(
              potentiallyUnassignedTasks.head.c,
              potentiallyIDleWorkers.head._1
            )
          ) {
            potentiallyIDleWorkers.head._1 ! EnqueueTask(potentiallyUnassignedTasks.head.c.task)

            newState = newState.taskEnqueuedToWorker(
              potentiallyUnassignedTasks.head,
              potentiallyIDleWorkers.head._1
            )
          }
          potentiallyIDleWorkers = potentiallyIDleWorkers.tail
          potentiallyUnassignedTasks = potentiallyUnassignedTasks.tail
        }

        return newState
      }

      // assign the least assigned task to the most idle worker
      def assignationCouple(): Option[(TaskEnqueued, ActorRef[MessageToWorker])] = {
        for (
          taskToAssign <- leastAssignedTasks.filter(_.assignedWorkers.size == minNbWorkersAssigned)
        ) {
          for (worker <- lessBusyWorker.filter(_._2._2 == nbQueuedTasksInLessBusyWorker)) {
            if (!state.isTaskAlreadyEnqueuedToWorker(taskToAssign.c, worker._1)) {
              return Some((taskToAssign, worker._1))
            }
          }
        }
        None
      }

      assignationCouple() match {
        case None => // this is impossible actually
          state
        case Some((task, worker)) =>
          worker ! EnqueueTask(task.c.task)
          ensureEveryoneHasWork(state.taskEnqueuedToWorker(task, worker))
      }
    }
  }

  private def doGlobalShutdown(state: State): Behavior[MessageToSupervisor] = {
    if (verbosityLevel >= 1)
      log.info("supervisor got shutdown command")
    state.workerStatus.foreach(_._1 ! WorkerShutdown(Some("global shutdown")))
    Behaviors.stopped
  }

  private def doWorkerNodeRegistration(
    workerNode: ActorRef[MessageToWorkerNode],
    nbWorkers: Int,
    context: ActorContext[MessageToSupervisor]
  ): Unit = {
    problemStatement match {
      case Some(ps) =>
        if (verbosityLevel >= 1)
          log.info(s"WorkerNode registered, sending problem statement (nbWorkers=$nbWorkers)")
        // Watch the WorkerNode to detect disconnections/crashes (Death Watch)
        // When the WorkerNode terminates, we receive a WorkerNodeTerminated message
        context.watchWith(workerNode, WorkerNodeTerminated(workerNode))
        workerNode ! ps
      case None =>
        log.error("WorkerNode registered but no problem statement available")
        workerNode ! ShutdownNode
    }
  }

  private def doSpanNewWorker(
    state: State,
    searchStructure: SearchConnector,
    context: ActorContext[MessageToSupervisor],
    testBehaviorOpt: Option[TestBehavior]
  ): State = {
    if (verbosityLevel >= 1)
      log.info("spawning new local worker: Worker" + state.nextLocalWorkerId)

    context.spawn(
      Worker(
        context.self,
        "Worker" + state.nextLocalWorkerId,
        searchStructure,
        verbosityLevel = this.verbosityLevel,
        testBehaviorOpt
      ),
      "Worker" + state.nextLocalWorkerId
    )
    state.incrementAllocatedWorkerId()
  }

  private def doSpawnSearchActorAndStatusRequest(
    searchProcedure: Behavior[MessageToSearch],
    state: State,
    context: ActorContext[MessageToSupervisor]
  ): State = {
    val currentSearchProcedure =
      context.spawn(searchProcedure, "search" + state.nextSearchId)

    currentSearchProcedure ! state.statusReport

    state
      .incrementAllocateSearchId()
      .copy(currentSearchProcedure = Some(currentSearchProcedure))
  }

  private def doWorkerRegistration(
    fromWorker: ActorRef[MessageToWorker],
    modelChecksum: Long,
    workerNode: Option[ActorRef[MessageToWorkerNode]],
    state: State,
    context: ActorContext[MessageToSupervisor]
  ): State = {

    if (checkSumOnStoreStructure != modelChecksum) {
      // not the proper model checksum
      fromWorker ! WorkerShutdown(Some("not the proper store"))

      if (verbosityLevel >= 1)
        log.error(
          s"Got worker registration from $fromWorker with incorrect optimization model; rejected"
        )

      state
    } else {
      // Watch the worker to detect disconnections/crashes (Death Watch)
      // When the worker terminates (crash, network failure, etc.), we receive a WorkerTerminated message
      context.watchWith(fromWorker, WorkerTerminated(fromWorker))

      val initStatusReport = WorkerStatusReport(
        fromWorker = fromWorker,
        ongoingTaskAndTimeSpent = None,
        toDoList = List.empty,
        nbStartedTasks = 0,
        nbFinishedTasks = 0
      )

      val stateWithWorker =
        state.copy(workerStatus = state.workerStatus + (fromWorker -> initStatusReport))

      // If worker belongs to a WorkerNode, track the mapping
      val newState = workerNode match {
        case Some(node) => stateWithWorker.registerWorkerToNode(node, fromWorker)
        case None       => stateWithWorker
      }

      newState.currentSearchProcedure match {
        case Some(p) => p ! newState.statusReport
        case None    => ;
      }
      if (verbosityLevel >= 1)
        log.info(
          "Got worker registration " + fromWorker + " (total workers:" + newState.workerStatus.size + ")"
        )
      newState
    }
  }

  private def ongoingSearch(state: State): Behavior[MessageToSupervisor] = {
    Behaviors.receive { (context, command) =>
      try {
        if (verbosityLevel >= 2) log.info(s"supervisor ongoingSearch; got $command")
        require(
          state.currentSearchProcedure.isDefined,
          "internal error: ongoingSearch and no currentSearchProcedure"
        )
        val currentSearchProcedure = state.currentSearchProcedure.get
        command match {
          case GlobalShutDown =>
            doGlobalShutdown(state)

          case WorkerNodeRegister(workerNode, nbWorkers) =>
            doWorkerNodeRegistration(workerNode, nbWorkers, context)
            Behaviors.same

          case SpawnLocalWorker(searchStructure, testBehaviorOpt) =>
            ongoingSearch(doSpanNewWorker(state, searchStructure, context, testBehaviorOpt))

          case SpawnSearchActorAndStatusRequest(searchProcedure: Behavior[MessageToSearch]) =>
            ongoingSearch(doSpawnSearchActorAndStatusRequest(searchProcedure, state, context))

          case w @ WorkersDisconnected(disconnectedWorkers) =>
            val newState =
              ensureEveryoneHasWork(disconnectedWorkers.foldLeft(state)({ case (state, worker) =>
                state.deleteWorkerAndTasks(worker)
              }))
            if (verbosityLevel >= 1) {
              log.info(s"$w(total workers:${newState.workerStatus.size})")
            }
            ongoingSearch(newState)

          case command: Command =>
            if (!state.isCurrentSearchTheRightSearch(command.from)) {
              // not from current search procedure; ignore
              log.error("not the right search")
              command.from ! Crash(
                new Error(
                  "received command from a search procedure that was not supposed to be running"
                )
              )
              currentSearchProcedure ! Crash(
                new Error(
                  "received command from a search procedure that was not supposed to be running"
                )
              )
              state.workerStatus.foreach(
                _._1 ! WorkerShutdown(
                  Some(
                    "received command from a search procedure that was not supposed to be running"
                  )
                )
              )
              Behaviors.stopped
            }

            command match {
              case SpawnSearchActorAndStatusRequest(_) =>
                throw new Error(
                  "received SpawnSearchActorAndStatusRequest while already running a search"
                )
              case GetNewUniqueTaskIds(_, n, answerTo) =>
                val (newState, firsTAskId, lastTaskId) = state.incrementStateId(n)
                answerTo ! NewUniqueTaskIds(firsTAskId, lastTaskId)
                ongoingSearch(state = newState)

              case c @ CreateTask(_, _, _) =>
                val newState = ensureEveryoneHasWork(state.addNewTask(c))
                ongoingSearch(newState)

              case CreateTasks(fromWorker, tasks) =>
                val newState = ensureEveryoneHasWork(
                  tasks.foldLeft(state)((state, taskAndAnswerTo) =>
                    state
                      .addNewTask(CreateTask(fromWorker, taskAndAnswerTo._1, taskAndAnswerTo._2))
                  )
                )
                ongoingSearch(newState)

              case CancelTask(_, taskId) =>
                for (assignedWorker <- state.taskIdToStatus(taskId).assignedWorkers) {
                  assignedWorker ! KillTasks(List(taskId))
                }

                val newState = ensureEveryoneHasWork(state.taskWasCancelledBySearch(taskId))
                ongoingSearch(newState)

              case CancelAllMyRemainingTasks(_, finished) =>
                // cancel all tasks
                for (worker <- state.workerStatus.keys) {
                  val t = state.getAllTasksAssignedTo(worker)
                  if (t.nonEmpty)
                    worker ! KillTasks(t)
                }

                // update status
                var newState = state
                for ((taskId, status) <- state.taskIdToStatus) {
                  newState = newState.copy(taskIdToStatus =
                    newState.taskIdToStatus + (taskId -> TaskCancelled(
                      status.c,
                      status.assignedWorkers.toSet
                    ))
                  )
                }

                if (finished) {
                  idle(newState)
                } else {
                  ongoingSearch(newState)
                }
            }

          case upwards: Upwards =>
            upwards match {
              case WorkerRegister(fromWorker, modelChecksum, workerNode) =>
                ongoingSearch(
                  ensureEveryoneHasWork(
                    doWorkerRegistration(fromWorker, modelChecksum, workerNode, state, context)
                  )
                )

              case WorkerTaskFinished(fromWorker, result) =>
                // check task status
                state.taskIdToStatus.get(result.taskId) match {
                  case None =>
                    // ignore it
                    ongoingSearch(state = state)

                  case Some(TaskEnqueued(c, enqueued)) =>
                    // task was finished before we got the start notification

                    if (enqueued contains fromWorker) {
                      c.answerTo ! ResultObtained(result)
                      // ok someone did the job in between, so we keep the result and cancel it
                      for (otherWorker <- enqueued - fromWorker) {
                        otherWorker ! KillTasks(List(c.task.taskId))
                      }
                      val newState =
                        state.copy(taskIdToStatus = state.taskIdToStatus - result.taskId)
                      val newState2 = ensureEveryoneHasWork(newState)
                      ongoingSearch(newState2)
                    } else {
                      log.error("received unexpected result from a worker?? ignored")
                      Behaviors.same
                    }

                  case Some(TaskCancelled(_, _)) =>
                    ongoingSearch(state.completeTaskCancellation(result.taskId, fromWorker))

                  case Some(TaskRunning(c, worker, enqueued)) =>
                    // normal finish of task

                    if (fromWorker equals worker) {
                      // expected ending
                      c.answerTo ! ResultObtained(result)
                      val newState =
                        state.copy(taskIdToStatus = state.taskIdToStatus - result.taskId)
                      val newState2 = ensureEveryoneHasWork(newState)
                      ongoingSearch(newState2)
                    } else {
                      if (enqueued.workers contains fromWorker) {
                        // ok someone else did the job in between, so we keep the result and cancel it
                        worker ! KillTasks(List(c.task.taskId))
                        val newState =
                          state.copy(taskIdToStatus = state.taskIdToStatus - result.taskId)
                        val newState2 = ensureEveryoneHasWork(newState)
                        ongoingSearch(newState2)
                      } else {
                        // what???
                        log.error("received unexpected result from a worker?? ignored")
                        Behaviors.same
                      }
                    }
                }

              case WorkerTaskCancelled(fromWorker, cancelledTaskId) =>
                // cancellation succeeded
                val newState2 =
                  ensureEveryoneHasWork(state.completeTaskCancellation(cancelledTaskId, fromWorker))
                ongoingSearch(newState2)

              case report: WorkerStatusReport =>
                val newState2 = ensureEveryoneHasWork(state.updateWorkerStatus(report))
                ongoingSearch(newState2)

              case WorkerCrash(_, _, error) =>
                currentSearchProcedure ! Crash(error)

                state.workerStatus.foreach(
                  _._1 ! WorkerShutdown(Some("global shutdown due to worker crash"))
                )
                Behaviors.stopped
            }

          case WorkerTerminated(terminatedWorker) =>
            // Death Watch detected that a worker terminated (crash, network failure, etc.)
            // Check if this worker belongs to a terminated WorkerNode (in which case, ignore)
            if (state.isWorkerFromTerminatedNode(terminatedWorker)) {
              // This worker's WorkerNode has already terminated, we've already handled it
              if (verbosityLevel >= 2) {
                log.info(
                  s"Ignoring WorkerTerminated for $terminatedWorker (WorkerNode already terminated)"
                )
              }
              Behaviors.same
            } else {
              val newState =
                ensureEveryoneHasWork(state.deleteWorkerAndTasks(terminatedWorker))
              if (verbosityLevel >= 1) {
                log.info(
                  s"Worker terminated (Death Watch): $terminatedWorker (total workers: ${newState.workerStatus.size})"
                )
              }
              // Notify search procedure about the updated worker count
              newState.currentSearchProcedure.foreach(_ ! newState.statusReport)
              ongoingSearch(newState)
            }

          case WorkerNodeTerminated(workerNode) =>
            // Death Watch detected that a WorkerNode terminated (crash, network failure, etc.)
            // Mark the node as terminated and get all workers that belong to it
            val (stateWithMarkedNode, workers) = state.markWorkerNodeTerminated(workerNode)
            if (workers.nonEmpty) {
              // Send WorkersTerminated to self to handle all workers at once
              if (verbosityLevel >= 1) {
                log.info(
                  s"WorkerNode terminated (Death Watch): $workerNode with ${workers.size} workers"
                )
              }
              context.self ! WorkersTerminated(workers, Some(workerNode))
            }
            ongoingSearch(stateWithMarkedNode)

          case WorkersTerminated(terminatedWorkers, workerNodeOpt) =>
            // Multiple workers terminated at once (typically from a WorkerNode crash)
            val newState =
              ensureEveryoneHasWork(terminatedWorkers.foldLeft(state) { (s, worker) =>
                s.deleteWorkerAndTasks(worker)
              })
            // Clean up the WorkerNode mapping if provided
            val finalState = workerNodeOpt match {
              case Some(node) => newState.deleteWorkerNodeAndWorkers(node)
              case None       => newState
            }
            if (verbosityLevel >= 1) {
              log.info(
                s"Workers terminated: ${terminatedWorkers.size} workers (total workers: ${finalState.workerStatus.size})"
              )
            }
            // Notify search procedure about the updated worker count
            finalState.currentSearchProcedure.foreach(_ ! finalState.statusReport)
            ongoingSearch(finalState)
        }
      } catch {
        case r: Throwable =>
          log.error(s"$r${r.getStackTrace.mkString("\n")}")
          Behaviors.stopped
      }
    }
  }

  private def idle(state: State): Behavior[MessageToSupervisor] = {
    Behaviors.receive { (context, command) =>
      try {
        if (verbosityLevel >= 2) log.info("supervisor idle; got " + command)
        command match {
          case GlobalShutDown =>
            doGlobalShutdown(state)

          case WorkerNodeRegister(workerNode, nbWorkers) =>
            doWorkerNodeRegistration(workerNode, nbWorkers, context)
            Behaviors.same

          case SpawnLocalWorker(searchStructure, testBehaviorOpt) =>
            idle(doSpanNewWorker(state, searchStructure, context, testBehaviorOpt))

          case SpawnSearchActorAndStatusRequest(searchProcedure: Behavior[MessageToSearch]) =>
            ongoingSearch(doSpawnSearchActorAndStatusRequest(searchProcedure, state, context))

          case w @ WorkersDisconnected(disconnectedWorkers) =>
            val newState =
              ensureEveryoneHasWork(disconnectedWorkers.foldLeft(state)({ case (state, worker) =>
                state.deleteWorkerAndTasks(worker)
              }))
            if (verbosityLevel >= 1) {
              log.info(s"$w(total workers:${newState.workerStatus.size})")
            }
            ongoingSearch(newState)

          case command: Command =>
            // these should be errors; since we are in idle mode there is no search procedure running.
            // so we just crash the thing
            command.from ! Crash(new Error("received command from search procedure while idle"))
            state.workerStatus.foreach(_._1 ! WorkerShutdown(Some("Global shutdown due to error")))
            Behaviors.stopped

          case upwards: Upwards =>
            upwards match {
              case WorkerRegister(fromWorker, modelChecksum, workerNode) =>
                idle(doWorkerRegistration(fromWorker, modelChecksum, workerNode, state, context))

              case WorkerTaskFinished(_, _) =>
                // ignore
                Behaviors.same

              case WorkerTaskCancelled(fromWorker, cancelledTaskId) =>
                idle(state.completeTaskCancellation(cancelledTaskId, fromWorker))

              case report: WorkerStatusReport =>
                idle(state.updateWorkerStatus(report))

              case w: WorkerCrash =>
                // We kill all workers because such a crash is a bug in the user code
                state.workerStatus.foreach(
                  _._1 ! WorkerShutdown(Some("global shutdown due to worker crash"))
                )
                workerCrashed(w, state)
            }

          case WorkerTerminated(terminatedWorker) =>
            // Death Watch detected that a worker terminated (crash, network failure, etc.)
            // Check if this worker belongs to a terminated WorkerNode (in which case, ignore)
            if (state.isWorkerFromTerminatedNode(terminatedWorker)) {
              // This worker's WorkerNode has already terminated, we've already handled it
              if (verbosityLevel >= 2) {
                log.info(
                  s"Ignoring WorkerTerminated for $terminatedWorker (WorkerNode already terminated)"
                )
              }
              Behaviors.same
            } else {
              val newState = state.deleteWorkerAndTasks(terminatedWorker)

              if (verbosityLevel >= 1) {
                log.info(
                  s"Worker terminated (Death Watch): $terminatedWorker (total workers: ${newState.workerStatus.size})"
                )
              }

              idle(newState)
            }

          case WorkerNodeTerminated(workerNode) =>
            // Death Watch detected that a WorkerNode terminated (crash, network failure, etc.)
            // Mark the node as terminated and get all workers that belong to it
            val (stateWithMarkedNode, workers) = state.markWorkerNodeTerminated(workerNode)

            if (workers.nonEmpty) {
              // Send WorkersTerminated to self to handle all workers at once
              if (verbosityLevel >= 1) {
                log.info(
                  s"WorkerNode terminated (Death Watch): $workerNode with ${workers.size} workers"
                )
              }
              context.self ! WorkersTerminated(workers, Some(workerNode))
            }
            idle(stateWithMarkedNode)

          case WorkersTerminated(terminatedWorkers, workerNodeOpt) =>
            // Multiple workers terminated at once (typically from a WorkerNode crash)
            val newState = terminatedWorkers.foldLeft(state) { (s, worker) =>
              s.deleteWorkerAndTasks(worker)
            }

            // Clean up the WorkerNode mapping if provided
            val finalState = workerNodeOpt match {
              case Some(node) => newState.deleteWorkerNodeAndWorkers(node)
              case None       => newState
            }

            if (verbosityLevel >= 1) {
              log.info(
                s"Workers terminated: ${terminatedWorkers.size} workers (total workers: ${finalState.workerStatus.size})"
              )
            }

            idle(finalState)
        }
      } catch {
        case r: Throwable =>
          log.error(s"$r${r.getStackTrace.mkString("\n")}")
          Behaviors.stopped
      }
    }
  }

  /** This special state is when there a crash happened there was no current search procedure. It
    * waits for a search procedure so that it can send the crash message
    */
  private def workerCrashed(
    workerCrash: WorkerCrash,
    state: State
  ): Behavior[MessageToSupervisor] = {
    Behaviors.receive { (context, command) =>
      try {
        command match {
          case GlobalShutDown =>
            Behaviors.stopped

          case WorkerNodeRegister(workerNode, _) =>
            // A worker node registered after a crash; shut it down
            workerNode ! ShutdownNode
            Behaviors.same

          case SpawnSearchActorAndStatusRequest(distributedSearchBehavior) =>
            // this will typically happen because we are idle,
            // so we wait for the next search to propagate the crash.
            val currentSearchProcedure =
              context.spawn(distributedSearchBehavior, "search" + state.nextSearchId)

            currentSearchProcedure ! Crash(workerCrash.error)
            Behaviors.stopped
          case command: Command =>
            command.from ! Crash(workerCrash.error)
            Behaviors.stopped
          case _: SpawnLocalWorker =>
            // ignore
            Behaviors.same
          case _: Upwards =>
            Behaviors.same
          case WorkersDisconnected(_) =>
            // ignore
            Behaviors.same
          case WorkerTerminated(_) =>
            // ignore worker terminations in crashed state, we're shutting down anyway
            Behaviors.same
          case WorkerNodeTerminated(_) =>
            // ignore WorkerNode terminations in crashed state, we're shutting down anyway
            Behaviors.same
          case WorkersTerminated(_, _) =>
            // ignore workers terminated in crashed state, we're shutting down anyway
            Behaviors.same
        }
      } catch {
        case r: Throwable =>
          log.error(s"$r${r.getStackTrace.mkString("\n")}")
          Behaviors.stopped
      }
    }
  }
}
