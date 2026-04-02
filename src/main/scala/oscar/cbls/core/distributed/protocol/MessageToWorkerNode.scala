package oscar.cbls.core.distributed.protocol

import org.apache.pekko.actor.typed.ActorRef
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.objective.Objective
import oscar.cbls.core.search.Neighborhood

/** Trait representing the message handled by WorkerNodes in a multi-JVM context
 */
sealed trait MessageToWorkerNode

/** Message sent at beginning of the WorkerNode to start the actual Workers
 */
case object StartWorkers extends MessageToWorkerNode

/** Message sent when the Supervisor actor has been started and its reference resolved,
 *  so the actual Workers can communicate with it
 * @param ref
 *   The reference to the resolved Supervisor actor
 */
case class SupervisorResolved(ref: ActorRef[MessageToSupervisor]) extends MessageToWorkerNode

/** Message sent when the WorkerNode has to be shut down.
 */
case object ShutdownNode extends MessageToWorkerNode

/** Message sent when a watched child worker terminates.
  * Used by WorkerNode to track when all its child workers have terminated.
  * @param worker
  *   The reference to the Worker actor that is terminating
  */
case class NodeWorkerTerminated(worker: ActorRef[MessageToWorker]) extends MessageToWorkerNode

/** Abstract base class for problem statements that can be sent to worker nodes.
  * Subclasses define specific optimization problems and how to build local search models from them.
  *
  * Problem statements are serialized and sent from the supervisor to worker nodes,
  * which then call buildLocalSearchModel() to create their own Store, Objective, and search neighborhoods.
  */
abstract class ProblemStatement extends MessageToWorkerNode {

  /** Builds a local search model from this problem statement.
    * Called by worker nodes to create their own Store, Objective, and search neighborhoods.
    *
    * @return a tuple (Store, Objective, Seq[Neighborhood]) where:
    *         - Store is the computation store for this worker
    *         - Objective is the optimization objective
    *         - Seq[Neighborhood] are the neighborhoods to explore
    */
  def buildLocalSearchModel(): (Store, Objective, Seq[Neighborhood])
}
