package oscar.cbls.core

import oscar.cbls.core.computation.Store
import oscar.cbls.core.search.Neighborhood

import scala.concurrent.duration.Duration

package object distrib {
  type supervisor = oscar.cbls.core.distrib.Supervisor
  final val supervisor = oscar.cbls.core.distrib.Supervisor

  def startSupervisorAndActorSystem(search: Neighborhood,
                                    verbose: Boolean = false,
                                    hotRestart:Boolean = true,
                                    tic: Duration = Duration.Inf): Supervisor = {
    supervisor.startSupervisorAndActorSystem(search, verbose, hotRestart, tic)
  }

}
