package oscar.cbls.core.distributed.search

import org.slf4j.Logger
import oscar.cbls.core.distributed.computation._
import oscar.cbls.core.search._
import oscar.cbls.lib.neighborhoods.combinator.StoreIndependentLoadSolutionMove

/** A remotely callable task that is about exploring a standard neighborhood.
  * @param n
  *   the neighborhood that can be explored
  */
case class RemotelySearchableNeighborhood(n: Neighborhood) extends RemotelyCallableTask {

  /** The method that is called in the computation thread to perform the task.
    *
    * @param description
    * the data send by the worker describing the task. this class supports only
    * [[DoAllMoves]] and
    * [[GetMove]] other objects will cause a crash
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
    * @param taskId
    *   the taskID this execution
    * @param taskClass
    *   the class of this remotely callable task
    * @return
    * the result of exploring the move; in this case
    * [[TaskResultMove]] of
    * [[TaskResultNoMoveFound]]
    */
  override def performTask(
    description: TaskParameters,
    shouldStop: () => Boolean,
    computationSupport: SearchConnector,
    log: Logger,
    verbosityLevel: Int,
    taskId: Long,
    taskClass: Int
  ): ActualResult = {

    description match {
      case DoAllMoves(startSolution, objective, sendProgressToOPt) =>
        computationSupport.attachSolutionToStore(startSolution).restoreSolution()
        val attachedObjective = computationSupport.attachObjectiveToStore(objective)

        n.reset()
        val startTimeMs = System.currentTimeMillis()

        val shouldStop: Int => Boolean =
          sendProgressToOPt match {
            case None =>
              _: Int => false
            case Some(listener) =>
              _: Int => {
                listener ! ProgressReport(
                  taskId = taskId,
                  obj = attachedObjective.objValue.value(),
                  runTimeMs = System.currentTimeMillis() - startTimeMs
                )
                false
              }
          }

        val nbMoves = n.doAllMoves(attachedObjective, shouldStop)

        if (nbMoves == 0) {
          TaskResultNoMoveFound
        } else {
          TaskResultMove(
            StoreIndependentLoadSolutionMove(
              computationSupport.saveDetachedSolution,
              attachedObjective.objValue.value(),
              "remote"
            )
          )
        }

      case GetMove(startSolution, objective) =>
        computationSupport.attachSolutionToStore(startSolution).restoreSolution()
        n.getMove(computationSupport.attachObjectiveToStore(objective)) match {
          case NoMoveFound => TaskResultNoMoveFound
          case MoveFound(move) =>
            TaskResultMove(computationSupport.detachMoveFromStore(move))
        }

      case x => throw new Error(s"unsupported command $x on RemotelySearchableNeighborhood")

    }
  }
}
