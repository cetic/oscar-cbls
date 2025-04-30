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

package oscar.cbls.core.computation

/** An object that holds a checkpoint for each decision variable of the problem. To roll back to
  * this checkpoint it's preferable to use a
  * [[oscar.cbls.lib.neighborhoods.combinator.RollBackToGlobalCheckpointMove]]. <p>Sets a new
  * checkpoint for each decision variable. If the variable does not have checkpoint mechanism, it
  * will simply save the value at checkpoint and assign it when releasing the checkpoint.
  *
  * @note
  *   This should be used when rolling back to a previous state after doing a lot of modification.
  *   For instance the [[oscar.cbls.lib.neighborhoods.combinator.Atomic]] combinator use that
  *   mechanism.
  *
  * @param savedCheckpoints
  *   The checkpoints corresponding to each decision variable.
  * @param model
  *   The model of those variables.
  */
case class GlobalCheckpoint(savedCheckpoints: Iterable[SavedCheckpoint], model: Store) {

  /** Restores the model in its previous saved state by restoring each decision variable's state.
    *
    * @note
    *   Use this method when you just want to go back to that state.
    */
  def restoreCheckpoints(): Unit = {
    savedCheckpoints.foreach(sc => sc.restoreCheckpoint())
    model.propagate()
  }

  /** Restores the model in its previous saved state by restoring each decision variable's state.
    * Then releases this checkpoint.
    *
    * @note
    *   Use this method when this GlobalCheckpoint won't be necessary anymore.
    */
  def restoreAndReleaseCheckpoints(): Unit = {
    savedCheckpoints.foreach(sc => sc.restoreAndReleaseCheckpoint())
  }

}
