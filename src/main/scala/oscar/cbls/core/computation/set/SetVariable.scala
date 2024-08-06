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

import oscar.cbls.core.computation._
import scala.collection.immutable.HashSet

object SetVariable {

  /** Creates an SetVariable.
    *
    * @param model
    *   The Store in which the SetVariable is registered
    * @param initialValue
    *   The initial value of the SetVariable
    * @param isConstant
    *   Whether the variable is a constant or not
    * @param name
    *   The (optional) name of this variable
    */
  def apply(
    model: Store,
    initialValue: Set[Int],
    isConstant: Boolean = false,
    name: Option[String] = None
  ): SetVariable = {
    new SetVariable(model, initialValue, isConstant, name)
  }
}

/** A variable managed by the [[oscar.cbls.core.computation.Store]] representing a set of integer
  * values.
  *
  * @param model
  *   The Store in which the SetVariable is registered
  * @param initialValue
  *   The initial value of the SetVariable
  * @param isConstant
  *   Whether the variable is a constant or not
  * @param name
  *   The (optional) name of this variable
  */
class SetVariable(
  model: Store,
  initialValue: Set[Int],
  isConstant: Boolean = false,
  name: Option[String] = None
) extends Variable(model, isConstant, name) {

  override type NotificationTargetType = SetNotificationTarget

  // Immutable vars are generally safer than mutable vals. If this proves to be
  // performance critical, consider using mutable vals that are "exported" to
  // immutable counterparts when required.

  // The new value of this variable, not propagated yet if different from oldValue
  private var _pendingValue: HashSet[Int] = HashSet.from(initialValue)
  // The old value of this variable
  private var _value: HashSet[Int] = HashSet.from(initialValue)

  /** The most recent value of the SetVariable. This value is potentially not yet propagated, and as
    * such listening propagations elements might not yet know about it.
    */
  def pendingValue: Set[Int] = _pendingValue

  /** The value of the SetVariable.
    *
    * \==WARNING==: By calling this you may trigger a propagation. If you want to know the new
    * (pending) value of this SetVariable, use [[pendingValue]] instead.
    *
    * If it's not a decision variable, it will start a propagation (if not yet propagating) of the
    * model up to this SetVariable.
    */
  def value(): Set[Int] = {
    (model.propagating, isADecisionVariable) match {
      case (true, _)     => _value
      case (false, true) => _pendingValue
      case (false, false) =>
        model.propagate(Some(this))
        _value
    }
  }

  // Auxiliary collections to keep track of the additions to, and removals from this variable.
  // Optional; using setValue will turn them off until the next propagation phase.
  private[this] var addedValues: Option[HashSet[Int]]   = Some(HashSet.empty)
  private[this] var removedValues: Option[HashSet[Int]] = Some(HashSet.empty)

  /** Alias for `setValue`. */
  def :=(v: Set[Int]): Unit = setValue(v)

  /** Alias for `add`. */
  def :+=(i: Int): Unit = add(i)

  /** Alias for `remove`. */
  def :-=(i: Int): Unit = remove(i)

  /** Changes the value of this variable and schedules it for propagation. */
  protected def setValue(value: Set[Int]): Unit = {
    if (value != _pendingValue) {
      addedValues = None
      removedValues = None
      _pendingValue = HashSet.from(value)
      scheduleForPropagation()
    }
  }

  private def diffException(): Unit =
    throw new IllegalStateException(
      s"Changelists in invalid state. Added: $addedValues Removed: $removedValues"
    )

  /** Adds the given element to this set variable, if not already present. */
  protected def add(i: Int): Unit = {
    if (!_pendingValue.contains(i)) {
      (addedValues, removedValues) match {
        case (Some(added), Some(removed)) =>
          if (removed.contains(i)) removedValues = Some(removed - i)
          else addedValues = Some(added + i)
        case (None, None) =>
        case _            => diffException()
      }
      _pendingValue += i
      scheduleForPropagation()
    }
  }

  /** Removes the given element from this set variable, if present. */
  protected def remove(i: Int): Unit = {
    if (_pendingValue.contains(i)) {
      (addedValues, removedValues) match {
        case (Some(added), Some(removed)) =>
          if (added.contains(i)) addedValues = Some(added - i)
          else removedValues = Some(removed + i)
        case (None, None) =>
        case _            => diffException()
      }
      _pendingValue -= i
      scheduleForPropagation()
    }
  }

  override def save(): SavedValue = new SetSavedValue(this)

  override def performPropagation(): Unit = {
    if (_value != _pendingValue) {
      val old = _value
      _value = _pendingValue

      val listening = getDynamicallyListeningElements
      if (listening.nonEmpty) {
        // auxiliary method to compute the diff lists
        def diff(
          pending: HashSet[Int],
          value: HashSet[Int],
          addedValues: Option[HashSet[Int]],
          removedValues: Option[HashSet[Int]]
        ): (Set[Int], Set[Int]) = {
          (addedValues, removedValues) match {
            case (Some(add), Some(rem)) =>
              // added and removed should be ok, let us just assert their validity
              assert(add.intersect(rem).isEmpty, s"Added $add and Removed $rem intersect")
              assert(add.subsetOf(pending), s"Added $add not subset of Pending $pending")
              assert(pending.intersect(rem).isEmpty, s"Removed $rem and Pending $pending intersect")
              (add, rem)
            case (None, None) =>
              // diff lists were reset because of setValue, need to compute them
              (pending.removedAll(value), value.removedAll(pending))
            case _ =>
              throw new IllegalStateException(
                s"Changelists in invalid state. Added: $addedValues Removed: $removedValues"
              )
          }
        }
        // get the definitive diff list
        val (added, removed) = diff(_pendingValue, old, addedValues, removedValues)

        listening.foreach { case (invariant: SetNotificationTarget, index: Int) =>
          invariant.notifySetChanges(this, index, added, removed, old, _pendingValue)
        }
      }
    }
    addedValues = Some(HashSet.empty)
    removedValues = Some(HashSet.empty)
  }

  override def checkInternals(): Unit = {
    require(
      _value == _pendingValue,
      Some("Error on SetValue:" + this.getClass.toString + " " + this)
    )
  }

  override def registerStaticallyAndDynamicallyListeningElement(
    propagationElement: Invariant with SetNotificationTarget,
    indexToRecallAtNotification: Int
  ): KeyForRemoval[(SetNotificationTarget, Int)] = {
    super.registerDynamicallyListeningElement(propagationElement, indexToRecallAtNotification)
  }

  override def toString: String = s"${name()} - value: ${value()}"
}