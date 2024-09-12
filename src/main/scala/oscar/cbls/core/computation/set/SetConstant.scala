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

import oscar.cbls.core.computation.Store

/** Companion object of the [[SetConstant]] class. */
object SetConstant {

  /** Creates a SetConstant.
    *
    * Since it's a constant, invoking methods that attempt to change its value will throw an *
    * exception.
    *
    * @param model
    *   The [[oscar.cbls.core.computation.Store]] to which this variable is linked.
    * @param value
    *   The value of this constant variable.
    */
  def apply(model: Store, value: Set[Int]): SetConstant = new SetConstant(model, value)
}

/** A constant SetVariable.
  *
  * Since it's a constant, invoking methods that attempt to change its value will throw an
  * exception.
  *
  * @param model
  *   The [[oscar.cbls.core.computation.Store]] to which this variable is linked.
  * @param value
  *   The value of this constant variable.
  */
class SetConstant(model: Store, value: Set[Int]) extends SetVariable(model, value, true) {
  override protected def setValue(value: Set[Int]): Unit = {
    require(requirement = false, "The value of a constant variable cannot be changed")
  }

  override protected def add(value: Int): Unit = {
    require(requirement = false, "The value of a constant variable cannot be changed")
  }

  override protected def remove(value: Int): Unit = {
    require(requirement = false, "The value of a constant variable cannot be changed")
  }
}
