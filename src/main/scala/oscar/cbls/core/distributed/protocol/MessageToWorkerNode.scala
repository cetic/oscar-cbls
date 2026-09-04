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
  *
  * ==Determinism==
  *
  * The statement is the ''only'' thing the nodes share: each of them rebuilds its model on its own
  * side, and the solutions that then travel between them ([[oscar.cbls.core.distributed.computation.StoreIndependentSolution]])
  * carry values only, not the model they were computed on. `buildLocalSearchModel` must therefore
  * be a pure function of the statement: two calls with the same statement, in whatever JVM, must
  * produce the same variables, in the same order, with the same initial values. Otherwise the nodes
  * silently disagree on what a solution means, and objectives measured on one node do not match
  * those measured on another.
  *
  * The usual sources of divergence are:
  *   - an unseeded `Random` (or any other entropy source) used to build the initial solution. Put
  *     the seed in the statement and derive the model from it, so that all the nodes draw the same
  *     values. Randomness that only drives the ''exploration'' is fine, and even desirable, since
  *     it diversifies the search without touching the model;
  *   - a clock reading, a `UUID`, a hostname or any other environment-dependent value;
  *   - iterating over a `HashMap`/`HashSet` whose order is not stable, when that order decides in
  *     which order variables are created.
  *
  * Note that this also holds within a single JVM: the supervisor calls `buildLocalSearchModel` once
  * for itself and once per local worker it spawns, and these models must agree just as much as the
  * remote ones do.
  */
abstract class ProblemStatement extends MessageToWorkerNode {

  /** Builds a local search model from this problem statement.
    * Called by worker nodes to create their own Store, Objective, and search neighborhoods.
    *
    * Must be deterministic with respect to this statement; see the class documentation.
    *
    * @return a tuple (Store, Objective, Seq[Neighborhood]) where:
    *         - Store is the computation store for this worker
    *         - Objective is the optimization objective
    *         - Seq[Neighborhood] are the neighborhoods to explore
    */
  def buildLocalSearchModel(): (Store, Objective, Seq[Neighborhood])
}
