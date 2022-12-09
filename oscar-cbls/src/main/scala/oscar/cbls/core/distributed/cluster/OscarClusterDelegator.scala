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

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import oscar.cbls.core.distributed.{MessageToWorker, MessageToSupervisor, WorkerActor}

/**
 * This object acts as an intermediary between the OscaR supervisor actor (created in the supervisor
 * node) and a OscaR worker actor (created in this delegator node)
 */
object OscarClusterDelegator {
  val DelegatorServiceKey: ServiceKey[DelegateWorker] = ServiceKey[OscarClusterDelegator.DelegateWorker]("OscarClusterDelegatorService")

  // Messages to delegators
  sealed trait DelegatorCommand
  final case class DelegateWorker(supervisorRef: ActorRef[MessageToSupervisor],
                                  replyTo: ActorRef[WorkerDelegated]) extends DelegatorCommand with KryoSerializable
  final case class WorkerDelegated(workerRef: ActorRef[MessageToWorker]) extends KryoSerializable

  def apply(oscarProblem: ()=>OscarWrappedProblem): Behavior[DelegatorCommand] = {
    Behaviors.setup { ctx =>
      // each delegator registers themselves with the receptionist
      ctx.log.info("Registering this delegator with receptionist")
      ctx.system.receptionist ! Receptionist.Register(DelegatorServiceKey, ctx.self)
      // Node behaviour
      clusterDelegatorBehavior(ctx, active = false, oscarProblem)
    }
  }

  /**
   * This method defines the behaviour of the Delegator node actor.
   * @param ctx the Actor context of the Delegator Cluster node
   * @param active whether this node has been already delegated by the supervisor
   * @param oscarProblem a function wrapping a OscaR problem
   * @return the behaviour of this Delegator node
   */
  private def clusterDelegatorBehavior(ctx: ActorContext[DelegatorCommand],
                                       active: Boolean,
                                       oscarProblem: ()=>OscarWrappedProblem): Behavior[DelegatorCommand] = {
    Behaviors.receiveMessage {
      case DelegateWorker(supervisorRef, replyTo) =>
        if (active) {
          // the node was already assigned, we ignore this
          ctx.log.info("Delegate worker that is already active: {}", ctx.self.path)
          Behaviors.same
        } else {
          ctx.log.info("Delegate worker to {}", supervisorRef.path)
          // Create the OscaR worker behaviour
          val wrappedOscarProblem = oscarProblem()
          val workerName = "remoteWorker"
          val workerBehavior = WorkerActor.createWorkerBehavior(
            wrappedOscarProblem.search.identifyRemotelySearchableNeighborhoods,
            wrappedOscarProblem.model,
            supervisorRef,
            wrappedOscarProblem.verbose,
            workerName
          )
          // spawn the OscaR worker actor and send the reference of this actor back to the
          // supervisor node
          val workerRef = ctx.spawn(workerBehavior, workerName)
          replyTo ! WorkerDelegated(workerRef)
          clusterDelegatorBehavior(ctx, active = true, oscarProblem)
        }
    }
  }
}
