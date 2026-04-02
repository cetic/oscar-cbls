package oscar.cbls.core.distributed.protocol

import oscar.cbls.core.distributed.computation.TaskResult

/** Messages that the distributed search combinator will receive. They will always come from the
  * supervisor.
  *
  * Note: the search procedure can also receive messages from workers, however this is defined at
  * the task level; executing a task can involve sending messages to an actor.
  */
abstract sealed class MessageToSearch

/** A set of new unique identifier that can be used to declare new tasks.
  *
  * @param firstTaskId
  *   the first id that can be used (included)
  * @param lastTaskId
  *   the last id that can be used (included)
  */
final case class NewUniqueTaskIds(firstTaskId: Long, lastTaskId: Long) extends MessageToSearch

/** A message describing the available workforce. The supervisor will send this message as a
  * response to [[SpawnSearchActorAndStatusRequest]] and whenever a worker joins or leaves the set
  * of available workers.
  *
  * @param nbWorkers
  *   the number of available workers
  * @param nbBusy
  *   the number of workers that are currently busy (approximation because this is an evolving
  *   situation)
  * @param nbIdle
  *   the number of workers that are currently idle (approximation because this is an evolving
  *   situation)
  */
final case class StatusReport(nbWorkers: Int, nbBusy: Int, nbIdle: Int) extends MessageToSearch { // send upon request or upon crash or new worker arrival{
  override def toString: String =
    s"StatusReport(nbWorkers: $nbWorkers, nbBusy: $nbBusy, nbIdle: $nbIdle)"
}

/** A message specifying that a task has finished and the result it produced.
  *
  * @param result
  *   the result of executing the task
  */
final case class ResultObtained(result: TaskResult) extends MessageToSearch

/** A message to notify that a crash occurred, typically in a
  * [[oscar.cbls.core.distributed.search.RemotelyCallableTask]]. The distributed search procedure
  * should throw this error again
  * @param error
  *   the error that occurred
  */
final case class Crash(error: Throwable) extends MessageToSearch
