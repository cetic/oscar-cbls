package oscar.cbls.test.core.computation

import oscar.cbls.algo.dll.DoublyLinkedList
import oscar.cbls.core.computation.{Invariant, SavedValue, Store, Variable}
import oscar.cbls.core.propagation.PropagationElement

case class TestConstantVariable(store: Store) extends Variable(store, true) {

  /** Save the state of this variable */
  override def save(): SavedValue = ???

  /** this is the propagation method that should be overridden by propagation elements. notice that
    * it is only called in a propagation wave if: 1L: it has been registered for propagation since
    * the last time it was propagated 2L: it is included in the propagation wave: partial
    * propagation wave do not propagate all propagation elements; it only propagates the ones that
    * come in the predecessors of the targeted propagation element overriding this method is
    * optional, so an empty body is provided by default
    */
  override def performPropagation(): Unit = ???

  /** This is the debug procedure through which propagation element can redundantly check that the
    * incremental computation they perform through the performPropagation method is correct
    * overriding this method is optional, so an empty body is provided by default
    */
  override def checkInternals(): Unit = ???

  def getTestDynamicallyListeningElements: DoublyLinkedList[PropagationElement] = {
    this.getDynamicallyListeningElements
  }
}

case class TestVaryingVariable(store: Store) extends Variable(store, false) {

  /** Save the state of this variable */
  override def save(): SavedValue = ???

  /** this is the propagation method that should be overridden by propagation elements. notice that
    * it is only called in a propagation wave if: 1L: it has been registered for propagation since
    * the last time it was propagated 2L: it is included in the propagation wave: partial
    * propagation wave do not propagate all propagation elements; it only propagates the ones that
    * come in the predecessors of the targeted propagation element overriding this method is
    * optional, so an empty body is provided by default
    */
  override def performPropagation(): Unit = ???

  /** This is the debug procedure through which propagation element can redundantly check that the
    * incremental computation they perform through the performPropagation method is correct
    * overriding this method is optional, so an empty body is provided by default
    */
  override def checkInternals(): Unit = ???

  def getTestDynamicallyListeningElements: DoublyLinkedList[PropagationElement] = {
    this.getDynamicallyListeningElements
  }
}

case class TestInvariant(store: Store) extends Invariant(store) {


  def addDefinedVariable(variable: TestVaryingVariable): Unit = {
    variable.setDefiningInvariant(this)
  }

  def addListenedVariable(variable: TestVaryingVariable): Unit = {
    this.registerStaticallyListenedElement(variable)
  }

  /** this is the propagation method that should be overridden by propagation elements. notice that
   * it is only called in a propagation wave if: 1L: it has been registered for propagation since
   * the last time it was propagated 2L: it is included in the propagation wave: partial
   * propagation wave do not propagate all propagation elements; it only propagates the ones that
   * come in the predecessors of the targeted propagation element overriding this method is
   * optional, so an empty body is provided by default
   */
  override def performPropagation(): Unit = ???

  /** This is the debug procedure through which propagation element can redundantly check that the
   * incremental computation they perform through the performPropagation method is correct
   * overriding this method is optional, so an empty body is provided by default
   */
  override def checkInternals(): Unit = ???
}
