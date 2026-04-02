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

import oscar.cbls.core.computation.SavedValue
import oscar.cbls.core.distributed.computation.{StoreIndependentSavedValue, StoreIndependentSetSavedValue}

/** A saved state of a [[SetVariable]]
  *
  * @param setVariable
  *   The SetVariable whose state is saved
  * @param savedValue
  *   the saved value
  */
case class SetSavedValue(setVariable: SetVariable, savedValue: Set[Int])
    extends SavedValue(setVariable) {

  override def compare(that: SavedValue): Int = {
    that match {
      case s: SetSavedValue if s.setVariable == setVariable =>
        if (s.savedValue.size != savedValue.size) {
          s.savedValue.size - savedValue.size
        } else {
          val l1 = s.savedValue.toList.sorted
          val l2 = savedValue.toList.sorted
          for ((a, b) <- l1.zip(l2)) {
            if (a != b) return a - b
          }
          0
        }
      case _ => throw new Error("cannot compare saved values from different variables")
    }
  }

  /** Restores the saved value to the variable */
  def restoreValue(): Unit = if (!setVariable.isConstant) setVariable := savedValue

  override def makeStoreIndependent: StoreIndependentSavedValue =
    StoreIndependentSetSavedValue(setVariable.id, savedValue.toArray)
}
