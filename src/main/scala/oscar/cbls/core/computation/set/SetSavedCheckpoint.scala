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

package oscar.cbls.core.computation.set

import oscar.cbls.core.computation.SavedCheckpoint

/** The checkpoint of a [[SetVariable]], which is its value.
  *
  * @param setVariable
  *   The SetVariable whose checkpoint is created
  */
class SetSavedCheckpoint(val setVariable: SetVariable) extends SavedCheckpoint(setVariable) {

  val savedValue: Set[Int] = setVariable.value()

  /** Restores the variable current value to the saved one.
    *
    * @note
    *   Use this method when you just want to go back to that state.
    */
  override def restoreCheckpoint(): Unit = if (!setVariable.isConstant) setVariable := savedValue

  /** Restores the variable at the checkpoint state and releases the variable internal checkpoint
    * (if any).
    *
    * @note
    *   Use this method when the GlobalCheckpoint won't be necessary anymore.
    */
  override def restoreAndReleaseCheckpoint(): Unit = restoreCheckpoint()
}
