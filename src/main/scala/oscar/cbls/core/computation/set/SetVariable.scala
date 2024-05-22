package oscar.cbls.core.computation.set

import oscar.cbls.core.computation._

import scala.collection.immutable.HashSet

class SetVariable(
  model: Store,
  initialValue: Set[Int],
  isConstant: Boolean = false,
  name: Option[String]
) extends Variable(model, isConstant, name) {

  override type NotificationTargetType = SetNotificationTarget

  // Immutable vars are generally safer than mutable vals. If this proves to be
  // performance critical, consider using mutable vals that are "exported" to
  // immutable counterparts when required.

  // The new value of this variable, not propagated yet if different from oldValue
  private var _pendingValue: HashSet[Int] = HashSet.from(initialValue)
  // The old value of this variable
  private var _value: HashSet[Int] = HashSet.from(initialValue)

  def pendingValue: Set[Int] = _pendingValue

  def value(): Set[Int] = {
    (model.propagating, isADecisionVariable) match {
      case (true, _)     => _value
      case (false, true) => _pendingValue
      case (false, false) =>
        model.propagate(Some(this))
        _value
    }
  }

  private[this] var addedValues: Option[HashSet[Int]]   = Some(HashSet.empty)
  private[this] var removedValues: Option[HashSet[Int]] = Some(HashSet.empty)

  /** Alias for `setValue`. */
  def :=(v: Set[Int]): Unit = setValue(v)

  def :+=(i: Int): Unit = add(i)

  def :-=(i: Int): Unit = remove(i)

  /** Changes the value of this variable and schedules it for propagation. */
  @inline
  def setValue(value: Set[Int]): Unit = {
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

  /** Adds the */
  @inline
  def add(i: Int): Unit = {
    if (!_pendingValue.contains(i)) {
      (addedValues, removedValues) match {
        case (Some(added), Some(removed)) =>
          if (removed.contains(i)) removed -= i else added += i
        case (None, None) =>
        case _            => diffException()
      }
      _pendingValue += i
      scheduleForPropagation()
    }
  }

  /** Removes the */
  @inline
  def remove(i: Int): Unit = {
    if (_pendingValue.contains(i)) {
      (addedValues, removedValues) match {
        case (Some(added), Some(removed)) =>
          if (added.contains(i)) added -= i else removed += i
        case (None, None) =>
        case _            => diffException()
      }
      _pendingValue -= i
      scheduleForPropagation()
    }
  }

  /** Save the state of this variable */
  override def save(): SavedValue = new SetSavedValue(this)

  /** This is the propagation method that should be overridden by propagation elements. Notice that
    * it is only called in a propagation wave if:
    *
    *   1. It has been registered for propagation since the last time it was propagated;
    *   1. It is included in the propagation wave: partial propagation waves do not propagate all
    *      propagation elements, but only the predecessors of the targeted element.
    *
    * Overriding this method is optional, so an empty body is provided by default.
    */
  override def performPropagation(): Unit = {
    if (_value != _pendingValue) {
      val listening = getDynamicallyListeningElements
      if (listening.isEmpty) _value = _pendingValue
      else {
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
        val (added, removed) = diff(_pendingValue, _value, addedValues, removedValues)

        val old = _value
        _value = _pendingValue

        listening.foreach { case (invariant: SetNotificationTarget, index: Int) =>
          invariant.notifySetChanges(this, index, added, removed, old, _pendingValue)
        }
      }
    }
    addedValues = Some(HashSet.empty)
    removedValues = Some(HashSet.empty)
  }

  /** This is the debug procedure through which propagation element can redundantly check that the
    * incremental computation they perform through the performPropagation method is correct.
    *
    * Overriding this method is optional, so an empty body is provided by default.
    */
  def checkInternals(): Unit = {
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
}
