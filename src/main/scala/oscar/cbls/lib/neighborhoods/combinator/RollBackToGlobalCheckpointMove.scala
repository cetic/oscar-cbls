// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.lib.neighborhoods.combinator

import oscar.cbls.core.computation.GlobalCheckpoint
import oscar.cbls.core.search.Move

/** Move used alongside with GlobalCheckpoint to notify the
  * [[oscar.cbls.core.computation.objective.Objective]] that its value has changed. <p> In some
  * cases, for instance in [[oscar.cbls.lib.neighborhoods.combinator.Atomic]], the combinator has to
  * commit moves. Therefore, if we do not roll back those moves using a Move, the objective won't
  * know that its value has changed again. And it will end in a failed requirement.
  *
  * @param globalCheckpoint
  *   The global checkpoint this Move rolls back to.
  * @param objectiveValueAtCheckpoint
  *   The objective value when the checkpoint was defined.
  */
case class RollBackToGlobalCheckpointMove(
  globalCheckpoint: GlobalCheckpoint,
  objectiveValueAtCheckpoint: Long
) extends Move(objectiveValueAtCheckpoint, "RollBackToGlobalCheckpoint") {

  /** Applies the modifications of this Move. */
  override def commit(): Unit = {
    globalCheckpoint.restoreAndReleaseCheckpoints()
  }

  override def toString: String = {
    s""" $neighborhoodName ${super.toString}
       |Roll back to objective value $objectiveValueAtCheckpoint""".stripMargin
  }
}
