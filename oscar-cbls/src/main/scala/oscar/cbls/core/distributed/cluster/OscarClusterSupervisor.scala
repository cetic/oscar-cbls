/**
 * *****************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 * ****************************************************************************
 */
package oscar.cbls.core.distributed.cluster

import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import oscar.cbls.core.distributed.{MessageToSupervisor, MessageToWorker, Supervisor}
import oscar.cbls.core.search.Neighborhood

import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

/**
 * This object defines the behaviour of the Supervisor node in the cluster.
 * It creates the OscaR supervisor actor, communicates the actor reference of the
 * OscaR supervisor to Delegator nodes, who create the OscaR workers, and
 * executes the search procedure
 */
object OscarClusterSupervisor {
  // Messages to Frontend
  sealed trait ClusterSupervisorEvent
  final case object Initialize extends ClusterSupervisorEvent
  private case class TrySearch(searchType: OscarSearchExecutor) extends ClusterSupervisorEvent
  private final case class DelegatorsUpdated(newDelegators: Set[ActorRef[OscarClusterDelegator.DelegateWorker]]) extends ClusterSupervisorEvent
  private final case class DelegateCompleted(workerRef: ActorRef[MessageToWorker]) extends ClusterSupervisorEvent
  private final case class DelegateFailed(why: String) extends ClusterSupervisorEvent
  private final case object Shutdown extends ClusterSupervisorEvent

  def apply(oscarProblem: ()=>OscarWrappedProblem): Behavior[ClusterSupervisorEvent] = Behaviors.setup { ctx =>
    // Setup to subscribe into listening delegators in Receptionist
    val subscriptionAdapter = ctx.messageAdapter[Receptionist.Listing] {
      case OscarClusterDelegator.DelegatorServiceKey.Listing(delegators) =>
        DelegatorsUpdated(delegators)
    }
    ctx.system.receptionist ! Receptionist.Subscribe(OscarClusterDelegator.DelegatorServiceKey, subscriptionAdapter)
    // setting the OscaR Supervisor object
    val wrappedOscarProblem = oscarProblem()
    // spawn the OscaR supervisor actor
    val supervisorActor = ctx.spawn(
      Supervisor.createSupervisorBehavior(
        wrappedOscarProblem.verbose,
        wrappedOscarProblem.hotRestart,
        wrappedOscarProblem.tic
      ),
      "supervisor"
    )
    val supervisor = startSupervisor(
      ctx,
      supervisorActor,
      wrappedOscarProblem.search,
      wrappedOscarProblem.verbose
    )(system = ctx.system)
    clusterSupervisorBehavior(
      ctx,
      supervisor,
      wrappedOscarProblem.searchExecutor,
      IndexedSeq.empty,
      0
    )
  }

  /**
   * This method defines the behaviour of the Supervisor node actor
   * @param ctx he Actor context of the Supervisor Cluster node
   * @param supervisor the OscaR supervisor object
   * @param delegators the sequence of delegators in de cluster (updated by the receptionist)
   * @param delegatedWorkers the number of (successfully) delegated workers
   * @param searchExecutor the reference to the executor of the OscaR solver, given by the OscarWrappedProblem
   * @return the behaviour of this Supervisor node
   */
  private def clusterSupervisorBehavior(ctx: ActorContext[ClusterSupervisorEvent],
                                        supervisor: Supervisor,
                                        searchExecutor: OscarSearchExecutor,
                                        delegators: IndexedSeq[ActorRef[OscarClusterDelegator.DelegateWorker]],
                                        delegatedWorkers: Int): Behavior[ClusterSupervisorEvent] = {
    // how much time can pass before we consider a request failed
    implicit val timeout: Timeout = 1.minute
    Behaviors.receiveMessage {
      case Initialize =>
        ctx.log.info("Initialize Supervisor Cluster")
        // Delegate the supervisor to available workers
        delegators.foreach { delegate =>
          ctx.ask(
            delegate,
            OscarClusterDelegator.DelegateWorker(supervisor.supervisorActor, _)
          ) {
            case Success(delegate) => DelegateCompleted(delegate.workerRef)
            case Failure(exception) => DelegateFailed(s"Delegate failure. Reason: ${exception.getMessage}")
          }
        }
        // Try to do the first search
        ctx.self ! TrySearch(searchExecutor)
        Behaviors.same

      case TrySearch(executor) =>
        if (delegatedWorkers < 6) {
          // Not enough delegate workers. Try again
          ctx.self ! TrySearch(executor)
        } else {
          ctx.log.info("Going to perform the search")
          // perform the actual search
          executor.executeSearch()
          ctx.self ! Shutdown
        }
        Behaviors.same

      case DelegatorsUpdated(newDelegators) =>
        ctx.log.info("List of services registered with the receptionist changed: {}", newDelegators)
        // Delegate the supervisor to new workers. Some of them might be already delegated
        newDelegators.foreach { delegate =>
          ctx.ask(
            delegate,
            OscarClusterDelegator.DelegateWorker(supervisor.supervisorActor, _)
          ) {
            case Success(delegate) => DelegateCompleted(delegate.workerRef)
            case Failure(exception) => DelegateFailed(s"Delegate failure. Reason: ${exception.getMessage}")
          }
        }
        clusterSupervisorBehavior(ctx, supervisor, searchExecutor, newDelegators.toIndexedSeq, delegatedWorkers)

      case DelegateCompleted(workerRef) =>
        ctx.log.info("Delegation completed successfully. Worker actor: {}", workerRef.path)
        // The delegation has been completed. The OscaR worker reference is not really used here since
        // the communication between the OscaR supervisor and workers is done through their protocol
        clusterSupervisorBehavior(ctx, supervisor, searchExecutor, delegators, delegatedWorkers + 1)

      case DelegateFailed(why) =>
        ctx.log.warn("Delegate worker failed. Reason: {}", why)
        // Delegate has failed
        Behaviors.same

      case Shutdown =>
        ctx.log.warn("Shutting down supervisor and cluster node")
        // The supervisor can shut down
        supervisor.shutdown()
        Behaviors.stopped
    }
  }

  /**
   * This method starts the OscaR supervisor object into the Actor system of the
   * Oscar supervisor node in the cluster
   * @param ctx the Actor context of the Supervisor Cluster node
   * @param supervisorActor the OscaR supervisor actor reference
   * @param search the search procedure, as a neighborhood object
   * @param verbose the level of verbosity
   * @param system the implicit Actor system
   * @return a new OscaR supervisor object, configured with the search procedure
   */
  private def startSupervisor(ctx: ActorContext[ClusterSupervisorEvent],
                              supervisorActor: ActorRef[MessageToSupervisor],
                              search: Neighborhood,
                              verbose: Boolean)
                             (implicit system: ActorSystem[_]): Supervisor = {
    val supervisor = Supervisor.wrapSupervisor(supervisorActor, verbose)
    val (nbNRemoteNeighborhood, nbDistributedCombinator, remoteTasks) = search.labelAndExtractRemoteTasks(supervisor)
    ctx.log.info(
      "Analyzed search; Nb Distributed Combinators:{}; Nb Remote Neighborhoods:{}; Tasks:{}",
      nbDistributedCombinator,
      nbNRemoteNeighborhood,
      remoteTasks
    )
    supervisor
  }
}
