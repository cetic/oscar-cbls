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

import oscar.cbls.core.computation.{Invariant, Store}

/** An Invariant that maintains a copy of a given [[IntVariable]].
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this Invariant is linked
  * @param fromValue
  *   The copied IntVariable
  * @param toValue
  *   The copy
  */
class IntIdentityInvariant(model: Store, fromValue: IntVariable, toValue: IntVariable)
    extends Invariant(model)
    with IntNotificationTarget {

  registerStaticallyListenedElement(fromValue)
  fromValue.registerDynamicallyListeningElement(this)
  toValue.setDefiningInvariant(this)

  toValue := fromValue.value()

  override def notifyIntChanges(intVariable: IntVariable, index: Int, oldVal: Long, newVal: Long): Unit = {
    toValue := newVal
  }

  /** This is the debug procedure through which propagation element can redundantly check that the
    * incremental computation they perform through the performPropagation method is correct
    * overriding this method is optional, so an empty body is provided by default
    */
  override def checkInternals(): Unit = {
    require(toValue.value() == fromValue.value())
  }
}
