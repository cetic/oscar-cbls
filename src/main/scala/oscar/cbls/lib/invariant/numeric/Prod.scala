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

package oscar.cbls.lib.invariant.numeric

import oscar.cbls.core.computation.{IncredibleBulk, Invariant, KeyForRemoval, Store}
import oscar.cbls.core.computation.integer.{IntNotificationTarget, IntVariable}
import oscar.cbls.core.computation.set.{SetNotificationTarget, SetVariable}

class Prod(
  model: Store,
  input: Array[IntVariable],
  listeningVariablesIndices: SetVariable,
  output: IntVariable,
  bulkIdentifier: Option[String] = None,
  name: Option[String] = None
) extends Invariant(model, name)
    with IntNotificationTarget
    with SetNotificationTarget {

  private[this] val keysForRemoval: Array[KeyForRemoval[_]] = new Array(input.length)
  private[this] var numberOfZero: Int                       = 0
  private[this] var nonNullProd: Long                       = 1

  bulkIdentifier match {
    case None =>
      // No bulk is used
      for (vars <- input) this.registerStaticallyListenedElement(vars)
    case Some(bulkId) =>
      // Register static dependency via a bulk
      this.addIncredibleBulk(IncredibleBulk.bulkRegistering(input, bulkId, model))
  }

  for (i <- listeningVariablesIndices.value()) {
    keysForRemoval(i) = input(i).registerDynamicallyListeningElement(this, i)
    if (input(i).value() == 0) numberOfZero += 1
    else nonNullProd *= input(i).value()
  }

  @inline
  override def notifyIntChanges(
    intVariable: IntVariable,
    contextualVarIndex: Int,
    oldVal: Long,
    newVal: Long
  ): Unit = {
    // The update mechanism of an IntVariable is such that oldVal is never equal to newVal
    // So the case oldVal == 0 && newVal == 0 never happens.
    if (oldVal == 0 && newVal != 0) {
      numberOfZero -= 1
      nonNullProd *= newVal
    } else if (oldVal != 0 && newVal == 0) {
      numberOfZero += 1
      nonNullProd /= oldVal
    } else {
      nonNullProd /= oldVal
      nonNullProd *= newVal
    }
    affectOutput()
  }

  override def notifySetChanges(
    setVariable: SetVariable,
    index: Int,
    addedElems: Iterable[Int],
    removedElems: Iterable[Int],
    oldValue: Set[Int],
    newValue: Set[Int]
  ): Unit = ???

  override def checkInternals(): Unit = ???

  @inline
  private[this] def affectOutput(): Unit = {
    if (numberOfZero == 0) output := nonNullProd
    else output                   := 0
  }

  @inline
  private[this] def notifyInsertOn(set: SetVariable, index: Int): Unit = {
    assert(set == listeningVariablesIndices)

    keysForRemoval(index) = input(index).registerDynamicallyListeningElement(this, index)
  }



}
