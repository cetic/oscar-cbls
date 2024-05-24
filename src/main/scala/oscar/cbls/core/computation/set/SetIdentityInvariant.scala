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

import oscar.cbls.core.computation.{Invariant, Store}

/** An Invariant that maintains a copy of a given [[SetVariable]].
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this Invariant is linked
  * @param fromValue
  *   The copied IntVariable
  * @param toValue
  *   The copy
  */
class SetIdentityInvariant(model: Store, fromValue: SetVariable, toValue: SetVariable)
    extends Invariant(model)
    with SetNotificationTarget {

  fromValue.registerStaticallyAndDynamicallyListeningElement(this)
  toValue.setDefiningInvariant(this)

  toValue := fromValue.value()

  def notifySetChanges(
    setVariable: SetVariable,
    index: Int,
    addedElems: Iterable[Int],
    removedElems: Iterable[Int],
    oldValue: Set[Int],
    newValue: Set[Int]
  ): Unit = {
    toValue := newValue
  }

  def checkInternals(): Unit = {
    require(
      toValue.pendingValue == fromValue.value(),
      s"Pending value of $toValue is not equal to value of $fromValue"
    )
  }
}
