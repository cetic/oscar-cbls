package oscar.cbls.core.distributed.protocol

import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import oscar.cbls.core.distributed.computation.{SearchConnector, Task, TaskResult}
import oscar.cbls.core.distributed.search.TestBehavior

/** The messages that the Supervisor accepts.
  */
sealed abstract class MessageToSupervisor

/** Message sent by a remote WorkerNode to register with the supervisor and request problem data.
  *
  * @param workerNode
  *   the ActorRef of the WorkerNode that is registering
  * @param nbWorkers
  *   the number of workers that the node intends to spawn
  */
final case class WorkerNodeRegister(
  workerNode: ActorRef[MessageToWorkerNode],
  nbWorkers: Int
) extends MessageToSupervisor

/** This message sent to the supervisor will trigger a global shutdown of the supervisor and all
  * workers.
  */
case object GlobalShutDown extends MessageToSupervisor

/** A message send by the Cluster to notify that a worker has been disconnected, e.g. because of
  * internet connexion lost or to OS-level crash. This will not stop the computation since it is not
  * due to a bug in the optimization engine. Rather, the tasks assigned to these workers will be
  * redirected to the remaining workers.
  * @param workers
  *   the workers that were disconnected.
  */
final case class WorkersDisconnected(workers: List[ActorRef[MessageToWorker]])
    extends MessageToSupervisor

/** This message is to ask the supervisor to spawn a new local worker and add it to his own worker
  * team.
  * @param searchStructure
  *   the search structure of the worker; must be different from the one of the supervisor
  */
final case class SpawnLocalWorker(
  searchStructure: SearchConnector,
  testBehaviorOpt: Option[TestBehavior]
) extends MessageToSupervisor

/** The messages that distributed search combinators can send to the supervisor. The search
  * combinators only communicate with the supervisor and will only receive messages from the
  * supervisor as well.
  *
  * @param from
  *   the search procedure combinator that sends the message
  */
abstract sealed class Command(val from: ActorRef[MessageToSearch]) extends MessageToSupervisor

/** Asks the supervisor to spawn a new actor representing a search procedure, and sent it a
  * [[StatusReport]] message.
  * @param distributedSearchBehavior
  *   the behavior for and actor representing a search procedure
  */
case class SpawnSearchActorAndStatusRequest(distributedSearchBehavior: Behavior[MessageToSearch])
    extends Command(null) //this one is null because the actor as not been created yet

/** A message requesting a new unique searchId for a coming search task. This calls for a
  * [[NewUniqueTaskIds]] message response.
  *
  * @param from
  *   the search procedure combinator that sends the message
  */
final case class GetNewUniqueTaskIds(
  override val from: ActorRef[MessageToSearch],
  nbIds: Int = 1,
  answerTo: ActorRef[NewUniqueTaskIds]
) extends Command(from)

/** Creates a new task and asks the supervisor to dispatch it to the proper worker.
  *
  * @param from
  *   the search procedure combinator that sends the message.
  * @param task
  *   the task to execute.
  */
final case class CreateTask(
  override val from: ActorRef[MessageToSearch],
  task: Task,
  answerTo: ActorRef[ResultObtained]
) extends Command(from)

/** Creates a set of tasks and asks the supervisor to dispatch it to the proper worker. This is way
  * more efficient if you need to create many tasks because these tasks will be balanced across
  * workers.
  * @param from
  *   the search procedure combinator that sends the message.
  * @param tasksAndAnswerTo
  *   the tasks to execute and the place here the result must be sent.
  */
final case class CreateTasks(
  override val from: ActorRef[MessageToSearch],
  tasksAndAnswerTo: List[(Task, ActorRef[ResultObtained])]
) extends Command(from)

/** Instructs the supervisor to cancel the task.
  *
  * Note: a task result could be sent after this so the distributed combinator must be ready to
  * ignore it.
  *
  * @param from
  *   the search procedure combinator that sends the message
  * @param taskId
  *   the unique ID of the task to cancel
  */
final case class CancelTask(override val from: ActorRef[MessageToSearch], taskId: Long)
    extends Command(from)

/** Asks the supervisor to cancel all the executing tasks because the distributed combinators has
  * finished its work. Distributed search combinators must send this message.
  *
  * @param from
  *   the search procedure combinator that sends the message
  * @param searchFinished
  *   true means that this search procedure will not send more request, false means that it will
  *   send more requests
  */
final case class CancelAllMyRemainingTasks(
  override val from: ActorRef[MessageToSearch],
  searchFinished: Boolean
) extends Command(from)

/** The messages that a worker can send to the supervisor.
  *
  * @param fromWorker
  *   the worker that sent it
  */
abstract sealed class Upwards(fromWorker: ActorRef[MessageToWorker]) extends MessageToSupervisor

/** When a Worker starts, it contacts the supervisor and enrolls to it so that it can accept work
  * from the supervisor.
  *
  * @param fromWorker
  *   the worker that sent it
  * @param modelChecksum
  *   a checksum that is generated based on the defined model and search procedure and instance data
  *   This serves to ensure that these are identical in all workers and in the supervisor node
  * @param workerNode
  *   optional reference to the WorkerNode that spawned this worker (None for local workers)
  */
final case class WorkerRegister(
    fromWorker: ActorRef[MessageToWorker],
    modelChecksum: Long,
    workerNode: Option[ActorRef[MessageToWorkerNode]] = None
) extends Upwards(fromWorker)

/** A message specifying that a task was finished.
  *
  * @param fromWorker
  *   the worker that sent it
  * @param result
  *   the result of the task that carries the task identification
  */
final case class WorkerTaskFinished(fromWorker: ActorRef[MessageToWorker], result: TaskResult)
    extends Upwards(fromWorker)

/** A message specifying that a task was effectively cancelled, as requested through a KillTask
  * message.
  *
  * @param fromWorker
  *   the worker that sent it
  * @param cancelledTaskId
  *   the id of the task that was cancelled
  */
final case class WorkerTaskCancelled(fromWorker: ActorRef[MessageToWorker], cancelledTaskId: Long)
    extends Upwards(fromWorker)

/** A generic status report that the worker sends as a response to [[WorkerStatusRequest]].
  *
  * @param fromWorker
  *   the worker that sent it
  * @param ongoingTaskAndTimeSpent
  *   the task that is currently running and the time spent on it in milliseconds
  * @param toDoList
  *   the local todoList of the worker
  * @param nbStartedTasks
  *   the number of tasks that the worker has started
  * @param nbFinishedTasks
  *   the number of tasks that the worker has finished
  */
final case class WorkerStatusReport(
  fromWorker: ActorRef[MessageToWorker],
  ongoingTaskAndTimeSpent: Option[(Long, Long)],
  toDoList: List[Long],
  nbStartedTasks: Int,
  nbFinishedTasks: Int
) extends Upwards(fromWorker) {
  def workload: Int = ongoingTaskAndTimeSpent.size + toDoList.size
} //send by the worker upon: task start, task finish, request

/** A message specifying that the worker crashed, typically due to a bug in the model or search
  * procedure. The worker will not restart after this because it is a bug.
  *
  * @param fromWorker
  *   the worker that sent it
  * @param ongoingTaskAndType
  *   the task instance that was running and the amount of time it has been running for in ms
  * @param error
  *   the error
  */
final case class WorkerCrash(
  fromWorker: ActorRef[MessageToWorker],
  ongoingTaskAndType: Option[(Long, Int)],
  error: Throwable
) extends Upwards(fromWorker)

/** A message sent by the Death Watch mechanism when a watched worker terminates unexpectedly.
  * This is used to detect worker crashes or network disconnections in multi-JVM setups.
  * @param terminatedWorker
  *   the worker that terminated
  */
final case class WorkerTerminated(terminatedWorker: ActorRef[MessageToWorker])
    extends MessageToSupervisor

/** A message sent when multiple workers terminate at once, typically because a WorkerNode
  * stopped or crashed. This consolidates multiple worker terminations into a single message.
  * This message takes precedence over individual WorkerTerminated messages for the same workers.
  * @param terminatedWorkers
  *   the list of workers that terminated
  * @param workerNode
  *   the WorkerNode that terminated (optional, for logging purposes)
  */
final case class WorkersTerminated(
    terminatedWorkers: List[ActorRef[MessageToWorker]],
    workerNode: Option[ActorRef[MessageToWorkerNode]] = None
) extends MessageToSupervisor

/** A message sent by the Death Watch mechanism when a watched WorkerNode terminates unexpectedly.
  * This is used by the Supervisor to detect WorkerNode crashes or network disconnections.
  * @param workerNode
  *   the WorkerNode that terminated
  */
final case class WorkerNodeTerminated(workerNode: ActorRef[MessageToWorkerNode])
    extends MessageToSupervisor
