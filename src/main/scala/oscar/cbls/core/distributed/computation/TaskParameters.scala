package oscar.cbls.core.distributed.computation

import org.apache.pekko.actor.typed.ActorRef

/** Extend this to implement a task description. Such description contains the algorithmic
  * parameters needed to perform a task. You must extend this class to store all the parameters
  * needed. SearchANeighborhood is an example of such task description. If a search procedure needs
  * additional parameter it should be specified here
  *
  * Tasks are declared and get a unique identifier. This identifier does not need to be specified in
  * the taskDescription; just focus on the algorithmic parameters here.
  *
  * Note: if a distributed search combinator needs regular progress report, the TaskDescription can
  * carry an ActorRef to the search procedure actor.
  *
  * Note: this class is not final obviously because you might want to extend it to develop advanced
  * distributed heuristics.
  */
abstract class TaskParameters

/** A [[TaskParameters]] that specifies that a neighborhood must be explored starting from this
  * initial state. The considered neighborhood is already known from its taskClass, which is carried
  * alongside this class.
  *
  * @param startSolution
  *   the solution that must be loaded before exploring the neighborhood
  */
final case class GetMove(
  startSolution: StoreIndependentSolution,
  objective: StoreIndependentObjective
) extends TaskParameters

/** A [[TaskParameters]] that specifies that a neighborhood must be explored starting from this
  * initial state until the neighborhood returns [[oscar.cbls.core.search.NoMoveFound]]. The
  * considered neighborhood is already known from its taskClass, which is carried alongside this
  * class.
  * @param startSolution
  *   The solution that must be loaded before exploring the neighborhood
  * @param objective
  *   The objective to be given to the [[oscar.cbls.Neighborhood]]
  * @param sendProgressToOPt
  *   An optional reference to an actor that can receive progress report messages
  */
final case class DoAllMoves(
  startSolution: StoreIndependentSolution,
  objective: StoreIndependentObjective,
  sendProgressToOPt: Option[ActorRef[ProgressReport]]
) extends TaskParameters

/** A progress report describing the progression of a [[DoAllMoves]] task
  * @param taskId
  *   The ID of the task
  * @param obj
  *   The value of the objective function
  * @param runTimeMs
  *   The amount of time it has been running
  */
case class ProgressReport(taskId: Long, obj: Long, runTimeMs: Long)
