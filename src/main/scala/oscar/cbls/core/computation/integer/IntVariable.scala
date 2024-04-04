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

import oscar.cbls.algo.dll.DoublyLinkedList
import oscar.cbls.core.computation.{Invariant, SavedValue, Store, Variable}
import oscar.cbls.core.propagation.PropagationElement

class IntVariable(model: Store, initialValue: Long, isConstant: Boolean = false)
    extends Variable(model, isConstant) {

  // The new value of this variable, not propagated yet if different from _oldValue
  private var _newValue: Long = initialValue
  // The old value of this variable
  private var _oldValue: Long = _newValue

  def newValue(): Long = _newValue

  def value(): Long = {
    if (!this.isADecisionVariable) model.performPartialPropagation(this)
    _newValue
  }

  /** Change the newValue of this variable and schedule it for propagation */
  @inline
  protected def setValue(value: Long): Unit = {
    if (value != _newValue) {
      _newValue = value
      scheduleForPropagation()
    }
  }

  /** Assign the given value to this variable */
  def :=(value: Long): Unit = setValue(value)

  /** Add the given value to this variable */
  def :+=(value: Long): Unit = setValue(_newValue + value)

  /** Subtract the given value from this variable */
  def :-=(value: Long): Unit = setValue(_newValue - value)

  /** Multiply this variable with the given value */
  def :*=(value: Long): Unit = setValue(_newValue * value)

  /** Divide this variable with the given value */
  def :/=(value: Long): Unit = setValue(_newValue / value)

  /** Increments this variable */
  def :++(): Unit = setValue(_newValue + 1)

  /** Decrements this variable */
  def :--(): Unit = setValue(_newValue - 1)

  override def save(): SavedValue = new IntSavedValue(this)

  override def performPropagation(): Unit = {
    if (_oldValue != _newValue) {
      val old = _oldValue
      _oldValue = _newValue

      val dynListElements = getDynamicallyListeningElements
      dynListElements.foreach {
        case invariant: IntNotificationTarget =>
          invariant.notifyIntChanged(this, old, _newValue)
        case invariant: Invariant =>
          throw new IllegalArgumentException(
            s"The listening Invariant ($invariant) does not extend IntNotificationTarget, therefore no notification can be send to it."
          )
      }
    }
  }

  override def checkInternals(): Unit = {
    require(
      _oldValue == _newValue,
      Some("error on IntValue:" + this.getClass.toString + " " + this)
    )
    require(
      checkValueWithinDomain(_newValue),
      s"Value is outside defined domain. Domain : ${domain.get} - value : ${_newValue}"
    )
  }

  override def registerDynamicallyListeningElement(
    elem: Invariant
  ): DoublyLinkedList[PropagationElement]#DLLStorageElement = {
    require(
      elem.isInstanceOf[IntNotificationTarget],
      "The listening invariant must extends IntNotificationTarget trait to be able to receive notification upon change"
    )
    super.registerDynamicallyListeningElement(elem)
  }
}
