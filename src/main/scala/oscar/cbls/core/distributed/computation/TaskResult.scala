package oscar.cbls.core.distributed.computation

/** The result of executing a task.
  *
  * @param taskId
  *   the unique identifier of the task that was completed
  * @param taskClass
  *   the class of the task eg: exploring a defined neighborhood
  * @param timeSpentMs
  *   the amount of time spent executing the task
  * @param result
  *   actual result of the task. This result will be cast to the proper value by the search
  *   procedure
  */
final case class TaskResult(taskId: Long, taskClass: Int, timeSpentMs: Long, result: ActualResult)

/** The algorithmic data result of the task Extend this class if you declare other return type.
  */
abstract class ActualResult

case object Aborted extends ActualResult

/** A result that is a move.
  *
  * @param move
  * the move resulting from the task, in this case it is typically a [[GetMove]] task
  */
case class TaskResultMove(move: StoreIndependentMove) extends ActualResult

case object TaskResultNoMoveFound extends ActualResult
