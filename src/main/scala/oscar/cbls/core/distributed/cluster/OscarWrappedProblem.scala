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

import oscar.cbls._
import oscar.cbls.core.search.Neighborhood

import scala.concurrent.duration.Duration

/**
 * This class "wraps" the components of a OscaR.cbls problem that allows the solver fot that problem
 * to be executed inside a cluster.
 * @param model the OscaR.cbls store
 * @param search the OscaR.cbls search procedure, as a neighborhood object
 * @param searchExecutor the object reference that executes the actual search
 * @param verbose the level of verbosity of the search execution
 * @param hotRestart flag about whether the OscaR supervisor actor uses hot restart
 * @param tic the duration of the Tic message in the supervisor
 */
case class OscarWrappedProblem(model: Store,
                               search: Neighborhood,
                               searchExecutor: OscarSearchExecutor,
                               verbose: Boolean = false,
                               hotRestart: Boolean = false,
                               tic: Duration = Duration.Inf)
