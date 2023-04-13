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

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.cluster.typed.Cluster

/**
 * This object implements the behaviour of an Akka Cluster which can execute
 * a OscaR.cbls solver.
 *
 * The OscaR cluster has two roles:
 * * Delegator (backend) node: A delegator node creates a given number of delegator actors,
 * which spawn OscaR worker actors bound to a OscaR supervisor actor when the supervisor node
 * * Supervisor (frontend) node: A supervisor node should be unique in the cluster. It spawns
 * a OscaR supervisor actor, and when enough delegators are enrolled to the cluster, it executes
 * the search procedure.
 */
object OscarCluster {
  def apply(oscarProblem: ()=>OscarWrappedProblem): Behavior[Nothing] =
    Behaviors.setup[Nothing] { ctx =>
      val cluster = Cluster(ctx.system)
      // Cluster roles
      // Backend Node : Delegator
      if (cluster.selfMember.hasRole("delegator")) {
        val delegatorsPerNode = ctx.system.settings.config.getInt("transformation.delegators-per-node")
        // start some delegator actors
        (1 to delegatorsPerNode).foreach { n =>
          ctx.spawn(OscarClusterDelegator(oscarProblem), s"OscarClusterDelegator$n")
        }
      }
      // Frontend Node : Supervisor
      if (cluster.selfMember.hasRole("supervisor")) {
        // start the supervisor actor
        val supervisorRef = ctx.spawn(OscarClusterSupervisor(oscarProblem), "OscarSupervisor")
        // tell the supervisor node to initialize
        supervisorRef ! OscarClusterSupervisor.Initialize
      }
      Behaviors.empty
    }
}
