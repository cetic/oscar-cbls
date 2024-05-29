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

import oscar.cbls.core.computation.{Invariant, KeyForRemoval, SavedValue, Store, Variable}

/** Companion object of IntVariable */
object IntVariable {

  /** Creates an IntVariable.
    *
    * @param model
    *   The Store in which the IntVariable was registered
    * @param initialValue
    *   The initial value of the IntVariable
    * @param isConstant
    *   If the variable is a constant
    * @param name
    *   The name (optional) of your Variable
    */
  def apply(
    model: Store,
    initialValue: Long,
    isConstant: Boolean = false,
    name: Option[String] = None
  ): IntVariable = {
    new IntVariable(model, initialValue, isConstant, name)
  }
}

/** A variable managed by the [[oscar.cbls.core.computation.Store]] whose type is integer.
  *
  * @param model
  *   The Store in which the IntVariable was registered
  * @param initialValue
  *   The initial value of the IntVariable
  * @param isConstant
  *   If the variable is a constant
  * @param name
  *   The name (optional) of your Variable
  */
class IntVariable(
  model: Store,
  initialValue: Long,
  isConstant: Boolean,
  name: Option[String] = None
) extends Variable(model, isConstant, name) {

  override type NotificationTargetType = IntNotificationTarget

  // The pending value (new value) of this variable, not propagated yet if different from _value
  private var _pendingValue: Long = initialValue
  // The actual value of this variable
  // For listening invariants this iS the value of the variable until propagation.
  private var _value: Long = _pendingValue

  /** The new value of the IntVariable. This value is not yet propagated therefore, listening
    * Invariant do not yet know about it.
    */
  def pendingValue: Long = _pendingValue

  /** The value of the IntVariable.
    *
    * \==WARNING==: By calling this you may trigger a propagation. If you want to know the new
    * (pending) value of this IntVariable, use [[pendingValue]] instead.
    *
    * If it's not a decision variable, it will start a propagation (if not yet propagating) of the
    * model up to this IntVariable.
    */
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

  override def registerStaticallyAndDynamicallyListeningElement(
    propagationElement: Invariant with IntNotificationTarget,
    indexToRecallAtNotification: Int
  ): KeyForRemoval[(IntNotificationTarget, Int)] = {
    super.registerDynamicallyListeningElement(propagationElement, indexToRecallAtNotification)
  }

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
      isValueWithinDomain(_pendingValue),
      s"Value is outside defined domain. Domain : ${domain.get} - value : ${_pendingValue}"
    )
  }

  override def toString: String =
    s"${name()} - value : ${value()}"
}
