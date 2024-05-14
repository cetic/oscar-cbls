package oscar.cbls.core.computation.set

import oscar.cbls.core.computation._

import scala.collection.immutable.HashSet
import scala.collection.mutable.{HashSet => MutSet}

class SetVariable(
  model: Store,
  initialValue: Set[Int],
  isConstant: Boolean = false,
  name: Option[String]
) extends Variable(model, isConstant, name) {

  // The new value of this variable, not propagated yet if different from oldValue
  private var pendingValue: HashSet[Int] = HashSet.from(initialValue)
  // The old value of this variable
  private var _oldValue: HashSet[Int] = HashSet.from(initialValue)

  private[this] var addedValues: Option[MutSet[Int]]   = None
  private[this] var removedValues: Option[MutSet[Int]] = None

  /** Alias for `setValue`. */
  def :=(v: Set[Int]): Unit = setValue(v)

  def :+=(i: Int): Unit = add(i)

  def :-=(i: Int): Unit = remove(i)

  /** Changes the value of this variable and schedules it for propagation. */
  @inline
  def setValue(value: Set[Int]): Unit = {
    // TODO according the old version, set changelists to None
    //  diff is manually computed in performSetPropagation
    if (value != pendingValue) {
      pendingValue = HashSet.from(value)
      scheduleForPropagation()
    }
  }

  /** Adds the */
  @inline
  def add(i: Int): Unit = {
    if (!pendingValue.contains(i)) {}
  }

  /** Removes the */
  @inline
  def remove(i: Int): Unit = {}

  private def removeValuePreviouslyIn(i: Int): Unit = {}

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
  override def performPropagation(): Unit = { // TODO check out performSetPropagation
//    if (_oldValue != pendingValue) {
//      val old = _oldValue
//      _oldValue = pendingValue
//
//      val dynListElements = getDynamicallyListeningElements
//      dynListElements.foreach {
//        case (invariant: SetNotificationTarget, index: Int) =>
//          invariant.notifySetChanges(this, index, old, pendingValue)
//        case (invariant: Invariant, _) =>
//          throw new IllegalArgumentException(
//            s"The listening Invariant ($invariant) does not extend SetNotificationTarget," +
//              s"therefore no notification can be send to it."
//          )
//      }
//    }
  }

  /** This is the debug procedure through which propagation element can redundantly check that the
    * incremental computation they perform through the performPropagation method is correct.
    *
    * Overriding this method is optional, so an empty body is provided by default.
    */
  def checkInternals(): Unit = {
    require(
      _oldValue == pendingValue,
      Some("error on SetValue:" + this.getClass.toString + " " + this)
    )
//    require(
//      checkValueWithinDomain(pendingValue),
//      s"Value is outside defined domain. Domain : ${domain.get} - value : ${pendingValue}"
//    )
  }

  override def registerDynamicallyListeningElement(
    elem: Invariant,
    variableIndex: Int = -1
  ): DoublyLinkedList[(PropagationElement, Int)]#DLLStorageElement = {
    require(
      elem.isInstanceOf[SetNotificationTarget],
      "The listening invariant must extend SetNotificationTarget trait " +
        "to be able to receive notification upon change"
    )
    registerDynamicallyListeningElement(elem, variableIndex)
  }

}
