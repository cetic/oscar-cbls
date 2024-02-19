package oscar.cbls.core.computation.integer

import oscar.cbls.algo.dll.DoublyLinkedList
import oscar.cbls.core.computation.{SavedValue, Store, Variable}
import oscar.cbls.core.propagation.PropagationElement

class IntVariable(model: Store, initialValue: Long, isConstant: Boolean = false)
    extends Variable(model, isConstant) {
  require(model != null, "Model (Store) must be defined")
  model.registerVariable(this)

  private var _newValue: Long = initialValue
  private var _oldValue: Long = _newValue

  def newValue(): Long = _newValue

  def value(): Long = {
    if (!this.isADecisionVariable) model.propagate(this)
    _newValue
  }

  @inline
  protected def setValue(value: Long): Unit = {
    if (value != _newValue) {
      _newValue = value
      // TODO : check if it's the correct way of doing it
      scheduleForPropagation()
    }
  }

  def :=(value: Long): Unit = setValue(value)

  def :+=(value: Long): Unit = setValue(_newValue + value)

  def :-=(value: Long): Unit = setValue(_newValue - value)

  def :*=(value: Long): Unit = setValue(_newValue * value)

  def :/=(value: Long): Unit = setValue(_newValue / value)

  def ++(): Unit = setValue(_newValue + 1)

  def --(): Unit = setValue(_newValue - 1)

  override def save(): SavedValue = new SavedIntValue(this)

  /** this is the propagation method that should be overridden by propagation elements. notice that
   * it is only called in a propagation wave if: 1L: it has been registered for propagation since
   * the last time it was propagated 2L: it is included in the propagation wave: partial
   * propagation wave do not propagate all propagation elements; it only propagates the ones that
   * come in the predecessors of the targeted propagation element overriding this method is
   * optional, so an empty body is provided by default
   */
  override def performPropagation(): Unit = {
    if (_oldValue != _newValue) {
      val old = _oldValue
      _oldValue = _newValue

      val dynListElements = getDynamicallyListeningElements
      dynListElements.foreach {
        case invariant: IntNotificationTarget =>
          invariant.notifyIntChanged(this, old, _newValue)
        case _ => throw new Exception("Should not happened")
      }
    }
  }

  /** This is the debug procedure through which propagation element can redundantly check that the
   * incremental computation they perform through the performPropagation method is correct
   * overriding this method is optional, so an empty body is provided by default
   */
  override def checkInternals(): Unit = {
    require(
      _oldValue == _newValue,
      Some("error on IntValue:" + this.getClass.toString + " " + this)
    )
    require(
      checkValueWithinDomain(_newValue),
      s"Value is outside defined domain. Domain : ${domain.get} - value : $_newValue"
    )
  }

  override def registerDynamicallyListeningElement(
                                                    elem: PropagationElement
                                                  ): DoublyLinkedList[PropagationElement]#DLLStorageElement = {
    require(
      elem.isInstanceOf[IntNotificationTarget],
      s"Element listening an IntVariable must implement IntNotificationTarget."
    )
    super.registerDynamicallyListeningElement(elem)
  }

  override def registerDynamicallyListeningElements(
                                                     elems: Iterable[PropagationElement]
                                                   ): Iterable[DoublyLinkedList[PropagationElement]#DLLStorageElement] = {
    require(
      elems.forall(_.isInstanceOf[IntNotificationTarget]),
      s"Elements listening an IntVariable must implement IntNotificationTarget."
    )
    super.registerDynamicallyListeningElements(elems)
  }
}
