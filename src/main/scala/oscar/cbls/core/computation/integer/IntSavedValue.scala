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

import oscar.cbls.core.computation.SavedValue
import oscar.cbls.core.distributed.computation.{StoreIndependentIntSavedValue, StoreIndependentSavedValue}

/** A saved state of a [[IntVariable]]
  *
  * @param intVariable
  *   The IntVariable whose state is saved
  * @param savedValue
  *   the saved value
  */
case class IntSavedValue(intVariable: IntVariable, savedValue: Long)
    extends SavedValue(intVariable) {

  override def compare(that: SavedValue): Int = {
    that match {
      case i: IntSavedValue if this.intVariable.id == i.intVariable.id =>
        this.savedValue compare i.savedValue
      case _ => throw new Error("cannot compare saved values from different variables")
    }
  }

  /** Restores the variable current value to the saved one */
  override def restoreValue(): Unit = if (!intVariable.isConstant) intVariable := savedValue

  override def makeStoreIndependent: StoreIndependentSavedValue =
    StoreIndependentIntSavedValue(intVariable.id, savedValue)
}
