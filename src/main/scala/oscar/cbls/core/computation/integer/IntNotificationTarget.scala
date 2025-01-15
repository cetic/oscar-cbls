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

package oscar.cbls.core.computation.integer

/** This trait must be extended by any [[oscar.cbls.core.propagation.PropagationElement]] listening
  * to a IntVariable. Its only method will be used to notify the changes occurring to the listened
  * IntVariable.
  */
trait IntNotificationTarget {

  /** Notifies the listening [[oscar.cbls.core.propagation.PropagationElement]] that the listened
    * [[oscar.cbls.core.computation.integer.IntVariable]] has changed.
    *
    * Implemented by the listening [[oscar.cbls.core.propagation.PropagationElement]]. Called by the
    * listened [[oscar.cbls.core.computation.integer.IntVariable]]
    *
    * @param intVariable
    *   The listened IntVariable
    * @param contextualVarIndex
    *   The optional index of the IntVariable in the context of the listening Invariant. Default -1
    * @param oldVal
    *   The previous value of the variable
    * @param newVal
    *   The new value of the variable
    */
  def notifyIntChanges(
    intVariable: IntVariable,
    contextualVarIndex: Int,
    oldVal: Long,
    newVal: Long
  ): Unit
}
