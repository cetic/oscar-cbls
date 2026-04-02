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

import oscar.cbls.core.computation._
import oscar.cbls.lib.invariant.numeric._
import oscar.cbls.util.Numeric

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
    *   The (optional) name of the Variable.
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
  * @param givenNameOpt
  *   The (optional) name of the Variable.
  */
class IntVariable(
  model: Store,
  initialValue: Long,
  isConstant: Boolean,
  givenNameOpt: Option[String] = None
) extends Variable(model, isConstant, givenNameOpt) {

  override type NotificationTargetType = IntNotificationTarget

  // The pending value (new value) of this variable, not propagated yet if different from _value
  private var _pendingValue: Long = initialValue
  // The actual value of this variable
  // For listening invariants this is the value of the variable until propagation.
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
  protected def setValue(value: Long): Unit = {
    if (value != _pendingValue) {
      _pendingValue = value
      scheduleForPropagation()
    }
  }

  /** Returns the domain of this variable as an iterable collection. Currently, the type of
    * collection used is [[scala.collection.immutable.NumericRange.Inclusive]]. The variable's
    * domain must be defined before invoking.
    *
    * @note
    *   Very large domains can cause performance issues.
    *
    * @throws java.lang.IllegalArgumentException
    *   if the domain was not defined for this variable.
    */
  def iterableDomain: Iterable[Long] = {
    require(domain.isDefined, s"no domain defined for variable $this")
    val (domainMin, domainMax): (Long, Long) = domain.get
    new Iterable[Long] {
      def iterator: Iterator[Long] =
        Iterator.iterate[Long](domainMin) { _ + 1 }.takeWhile(_ <= domainMax)
    }
  }

  /** Assign the given value to this variable */
  def :=(value: Long): Unit = setValue(value)

  /** Set this variable as identical to the given variable. */
  def :<-(that: IntVariable): Unit = new IntIdentityInvariant(this.model, that, this)

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

  /** Returns the sum of this variable and another. */
  def +(that: IntVariable): IntVariable = Sum2.result(this, that)

  /** Returns the sum of this variable and an integer. */
  def +(that: Int): IntVariable = Int2Int(this, _ + that)

  /** Returns the sum of this variable and a long integer. */
  def +(that: Long): IntVariable = Int2Int(this, _ + that)

  /** Returns the difference between this variable and another. */
  def -(that: IntVariable): IntVariable = Minus2.result(this, that)

  /** Returns the difference between this variable and an integer. */
  def -(that: Int): IntVariable = Int2Int(this, _ - that)

  /** Returns the difference between this variable and a long integer. */
  def -(that: Long): IntVariable = Int2Int(this, _ - that)

  /** Returns the product of this variable and another. */
  def *(that: IntVariable): IntVariable = Prod2.result(this, that)

  /** Returns the product between this variable and an integer. */
  def *(that: Int): IntVariable = Int2Int(this, _ * that)

  /** Returns the product between this variable and a long integer. */
  def *(that: Long): IntVariable = Int2Int(this, _ * that)

  /** Returns the quotient of this variable and another. */
  def /(that: IntVariable): IntVariable = Div2.result(this, that)

  /** Returns the quotient between this variable and an integer. */
  def /(that: Int): IntVariable = Int2Int(this, _ / that)

  /** Returns the quotient between this variable and a long integer. */
  def /(that: Long): IntVariable = Int2Int(this, _ / that)

  /** Returns `this` to the power `that`. */
  def ^(that: IntVariable): IntVariable = Pow.result(this, that)

  /** Returns the remainder of the integer division between this variable and another.
    *
    * @note
    *   The scala operator `%` is not exactly a modulo operator.<br>
    *
    * For example, `-1 % 3 == -1`, and `2 % 3 == 2` but `-1 mod 3 == 2`.
    */
  def mod(that: IntVariable): IntVariable = Mod.result(this, that)

  /** Returns the absolute value of this variable. */
  def abs: IntVariable = Abs.result(this)

  /** Returns the opposite value of this variable. */
  def unary_- : IntVariable = Opposite.result(this)

  /** Returns the square of this variable. */
  def square: IntVariable = Square.result(this)

  private def leq(a: Long, b: Long): Long =
    if (a <= b) 0 else Numeric.limitToLong(BigInt(a) - BigInt(b))

  /** Returns a variable that assumes value 0 if this variable is less or equal than another one,
    * and the magnitude of the difference (limited to [[scala.Long.MaxValue]]) otherwise.
    */
  def leq(that: IntVariable): IntVariable = IntInt2Int(this, that, (a, b) => leq(a, b))

  /** Returns a variable that assumes value 0 if this variable is less or equal than an integer, and
    * the magnitude of the difference (limited to [[scala.Long.MaxValue]]) otherwise.
    */
  def leq(that: Int): IntVariable = Int2Int(this, leq(_, that))

  /** Returns a variable that assumes value 0 if this variable is less or equal than a long integer,
    * and the magnitude of the difference (limited to [[scala.Long.MaxValue]]) otherwise.
    */
  def leq(that: Long): IntVariable = Int2Int(this, leq(_, that))

  private def geq(a: Long, b: Long): Long =
    if (a >= b) 0 else Numeric.limitToLong(BigInt(b) - BigInt(a))

  /** Returns a variable that assumes value 0 if this variable is greater or equal than another one,
    * and the magnitude of the difference (limited to [[scala.Long.MaxValue]]) otherwise.
    */
  def geq(that: IntVariable): IntVariable = IntInt2Int(this, that, (a, b) => geq(a, b))

  /** Returns a variable that assumes value 0 if this variable is greater or equal than another one,
    * and the magnitude of the difference (limited to [[scala.Long.MaxValue]]) otherwise.
    */
  def geq(that: Int): IntVariable = Int2Int(this, geq(_, that))

  /** Returns a variable that assumes value 0 if this variable is greater or equal than another one,
    * and the magnitude of the difference (limited to [[scala.Long.MaxValue]]) otherwise.
    */
  def geq(that: Long): IntVariable = Int2Int(this, geq(_, that))

  /** Decrements this variable */
  def :--(): Unit = setValue(_pendingValue - 1)

  /** Swaps this variable's value with other's value */
  def :=:(other: IntVariable): Unit = {
    val tmp = other.pendingValue
    other.setValue(this.pendingValue)
    this.setValue(tmp)
  }

  override def save(): SavedValue = IntSavedValue(this, this.value())

  override def createGlobalCheckpoint(): SavedCheckpoint = new IntSavedCheckpoint(this)

  override def registerDynamicallyListeningElement(
    target: IntNotificationTarget,
    indexToRecallAtNotification: Int
  ): KeyForRemoval[(IntNotificationTarget, Int)] = {
    doRegisterDynamicallyListeningElement(target, indexToRecallAtNotification)
  }

  override def registerStaticallyAndDynamicallyListeningElement(
    propagationElement: Invariant with IntNotificationTarget,
    indexToRecallAtNotification: Int
  ): KeyForRemoval[(IntNotificationTarget, Int)] = {
    doRegisterStaticallyAndDynamicallyListeningElement(
      propagationElement,
      indexToRecallAtNotification
    )
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
    s"${name} - value : ${value()}"
}
