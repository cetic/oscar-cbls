package oscar.cbls.core.distributed.protocol

import oscar.cbls.core.distributed.computation.Task

/** The messages that the Worker accepts.
  */
sealed abstract class MessageToWorker

/** The messages that the supervisor can send to a worker.
  */
abstract sealed class Downwards extends MessageToWorker

/** The command to enqueue a task in its TODO-list, and start it if it is idle.
  *
  * @param task
  *   the considered task
  */
final case class EnqueueTask(task: Task) extends Downwards

/** A command that the supervisor send to a worker to kill a task, has it started or not. if the
  * task finishes and sends result after this or in-between it is not an issue. The goal of the
  * killTask is to recover the computing power of the worker as soon as possible for other tasks.
  *
  * @param taskIDs
  *   the uniqueID's of tasks that must be killed
  */
final case class KillTasks(taskIDs: List[Long]) extends Downwards

/** A message sent by the supervisor to the worker to get status information. This calls for a
  * [[WorkerStatusReport]] answer.
  */
final case class WorkerStatusRequest() extends Downwards

/** Commands to the worker to shut down. This makes sense in distributed optimization setting.
  */
final case class WorkerShutdown(errorOpt: Option[String]) extends Downwards

/** An abstract message so that the worker can send messages to itself.
  */
abstract class InternalMessageToWorker extends MessageToWorker
