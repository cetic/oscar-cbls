package oscar.cbls.core.distributed.search

import org.slf4j.Logger
import oscar.cbls.core.distributed.computation.{ActualResult, SearchConnector, TaskParameters}

/** An abstract class that must be extended to declare a task that can be delegated by a search
  * procedure to a worker.
  */
abstract class RemotelyCallableTask {

  /** The method that is called in the computation thread to perform the task.
    * @param description
    *   the data sent by the worker describing the task, typically the start solution
    * @param shouldStop
    *   a procedure to test if the task must be stopped as requested by the search procedure or
    *   because a duplicate has been started.
    * @param computationSupport
    *   the ComputationSupport to use for the task. It mainly contains the store, which might be
    *   handy in this case.
    * @param log
    *   a log to send progress (be very light on it because it consumes CPU)
    * @param verbosityLevel
    *   to reduce the amount of data sent to the log; zero means no data send at all.
    * @return
    *   an [[ActualResult]] that represents the result of the task,or
    *   [[oscar.cbls.core.distributed.protocol.Aborted]]
    */
  def performTask(
    description: TaskParameters,
    shouldStop: () => Boolean,
    computationSupport: SearchConnector,
    log: Logger,
    verbosityLevel: Int,
    taskId: Long,
    taskClass: Int
  ): ActualResult
}
