package oscar.cbls.core

import oscar.cbls.core.search.Neighborhood

import scala.concurrent.duration.Duration

package object distributed {
  def startSupervisorAndActorSystem(search: Neighborhood,
                                    verbose: Boolean = false,
                                    hotRestart:Boolean = true,
                                    tic: Duration = Duration.Inf): Supervisor = {
    Supervisor.startSupervisorAndActorSystem(search, verbose, hotRestart, tic)
  }

}
