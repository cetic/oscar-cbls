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

import oscar.cbls.core.computation.{KeyForRemoval, SavedValue, Store, Variable}

object IntVariable{
  def apply(model: Store, initialValue: Long, isConstant: Boolean = false): IntVariable = {
    new IntVariable(model, initialValue, isConstant)
  }
}

class IntVariable(model: Store, initialValue: Long, isConstant: Boolean)
    extends Variable(model, isConstant) {

  override type NotificationTargetType = IntNotificationTarget

  // The new value of this variable, not propagated yet if different from _oldValue
  private var _pendingValue: Long = initialValue
  // The old value of this variable
  private var _value: Long = _pendingValue

  def pendingValue(): Long = _pendingValue

  def value(): Long = {
    (model.propagating, isADecisionVariable) match {
      case (true, _)     => _value
      case (false, true) => _pendingValue
      case (false, false) =>
        model.propagate(Some(this))
        _value
    }
  }

  /** Change the newValue of this variable and schedule it for propagation */
  @inline
  protected def setValue(value: Long): Unit = {
    if (value != _pendingValue) {
      _pendingValue = value
      scheduleForPropagation()
    }
  }

  /** Assign the given value to this variable */
  def :=(value: Long): Unit = setValue(value)

  /** Add the given value to this variable */
  def :+=(value: Long): Unit = setValue(_pendingValue + value)

  /** Subtract the given value from this variable */
  def :-=(value: Long): Unit = setValue(_pendingValue - value)

  /** Multiply this variable with the given value */
  def :*=(value: Long): Unit = setValue(_pendingValue * value)

  /** Divide this variable with the given value */
  def :/=(value: Long): Unit = setValue(_pendingValue / value)

  /** Increments this variable */
  def :++(): Unit = setValue(_pendingValue + 1)

  /** Decrements this variable */
  def :--(): Unit = setValue(_pendingValue - 1)

  override def save(): SavedValue = new IntSavedValue(this)

  override def performPropagation(): Unit = {
    if (_value != _pendingValue) {
      val old = _value
      _value = _pendingValue

      val dynListElements = getDynamicallyListeningElements
      dynListElements.foreach { case (invariant: IntNotificationTarget, index: Int) =>
        invariant.notifyIntChanges(this, index, old, _pendingValue)
      }
    }
  }

  override def checkInternals(): Unit = {
    require(
      _value == _pendingValue,
      Some("error on IntValue:" + this.getClass.toString + " " + this)
    )
    require(
      isValueWithinDomain(_pendingValue),
      s"Value is outside defined domain. Domain : ${domain.get} - value : ${_pendingValue}"
    )
  }

  override def registerDynamicallyListeningElement(
    elem: NotificationTargetType,
    variableIndex: Int = -1
  ): KeyForRemoval[(NotificationTargetType, Int)] = {
    require(
      elem.isInstanceOf[IntNotificationTarget],
      "The listening invariant must extends IntNotificationTarget trait to be able to receive notification upon change"
    )
    super.registerDynamicallyListeningElement(elem, variableIndex)
  }
}
