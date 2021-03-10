package oscar.cbls.core

import oscar.cbls.core.computation.Store
import oscar.cbls.core.search.Neighborhood

import scala.concurrent.duration.Duration

package object distrib {
  type supervisor = oscar.cbls.core.distrib.Supervisor
  final val supervisor = oscar.cbls.core.distrib.Supervisor

  def startSupervisorAndActorSystem(store: Store,
                                    search: Neighborhood,
                                    verbose: Boolean = false,
                                    tic: Duration = Duration.Inf): Supervisor = {
    supervisor.startSupervisorAndActorSystem(store, search, verbose, tic)
  }

}
