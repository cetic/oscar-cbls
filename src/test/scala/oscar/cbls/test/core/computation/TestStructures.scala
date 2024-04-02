package oscar.cbls.test.core.computation

import oscar.cbls.algo.dll.DoublyLinkedList
import oscar.cbls.core.computation.{
  IncredibleBulk,
  Invariant,
  KeyForRemoval,
  SavedValue,
  Store,
  Variable
}
import oscar.cbls.core.propagation.PropagationElement

// Test class for a variable that cannot change its value
case class TestConstantVariable(store: Store, value: Int = 0) extends Variable(store, true) {

  /** Save the state of this variable */
  override def save(): SavedValue = {
    require(requirement = false, "Should not reach this")
    null
  }

  /** this is the propagation method that should be overridden by propagation elements. notice that
    * it is only called in a propagation wave if: 1L: it has been registered for propagation since
    * the last time it was propagated 2L: it is included in the propagation wave: partial
    * propagation wave do not propagate all propagation elements; it only propagates the ones that
    * come in the predecessors of the targeted propagation element overriding this method is
    * optional, so an empty body is provided by default
    */
  override def performPropagation(): Unit = {}

  /** This is the debug procedure through which propagation element can redundantly check that the
    * incremental computation they perform through the performPropagation method is correct
    * overriding this method is optional, so an empty body is provided by default
    */
  override def checkInternals(): Unit = {}

  def getTestDynamicallyListeningElements: DoublyLinkedList[PropagationElement] = {
    this.getDynamicallyListeningElements
  }
}

// Test class that saves the value of an TestVaryingVariable and restores it.
case class TestSavedValue(testVaryingVariable: TestVaryingVariable)
    extends SavedValue(testVaryingVariable) {

  val savedValue: Int = testVaryingVariable.value

  /** Restores the variable current value to the saved one */
  override def restoreValue(): Unit = testVaryingVariable.setValue(savedValue)
}

// Test class for a variable that can change its value (basically an IntVar with only a set method)
case class TestVaryingVariable(store: Store, startValue: Int = 0) extends Variable(store, false) {
  private var newValue: Int = startValue
  private var oldValue: Int = startValue

  // For first propagation
  scheduleForPropagation()

  def setValue(newValue: Int): Unit = {
    this.newValue = newValue
    scheduleForPropagation()
  }

  def value: Int = newValue

  def getTestDynamicallyListeningElements: DoublyLinkedList[PropagationElement] = {
    this.getDynamicallyListeningElements
  }

  /** Save the state of this variable */
  override def save(): SavedValue = TestSavedValue(this)

  /** this is the propagation method that should be overridden by propagation elements. notice that
    * it is only called in a propagation wave if: 1L: it has been registered for propagation since
    * the last time it was propagated 2L: it is included in the propagation wave: partial
    * propagation wave do not propagate all propagation elements; it only propagates the ones that
    * come in the predecessors of the targeted propagation element overriding this method is
    * optional, so an empty body is provided by default
    */
  override def performPropagation(): Unit = {
    getDynamicallyListeningElements.foreach(pe =>
      pe.asInstanceOf[TestVariableNotificationTarget]
        .notifyTestVariableChanged(this, oldValue, newValue)
    )
    oldValue = newValue
  }

  /** This is the debug procedure through which propagation element can redundantly check that the
    * incremental computation they perform through the performPropagation method is correct
    * overriding this method is optional, so an empty body is provided by default
    */
  override def checkInternals(): Unit = {}
}

// Test trait to simulate the propagation of a variable's value
trait TestVariableNotificationTarget {
  def notifyTestVariableChanged(
    testVaryingVariable: TestVaryingVariable,
    oldValue: Int,
    newValue: Int
  ): Unit
}

// Test class for an Invariant that maintains the sum of its input variables.
case class TestInvariant(
  store: Store,
  inputVariables: List[Variable],
  outputVariable: Option[Variable]
) extends Invariant(store)
    with TestVariableNotificationTarget {
  if (outputVariable.isDefined) outputVariable.get.setDefiningInvariant(this)
  var keysForRemoval: Array[KeyForRemoval] =
    inputVariables
      .map(inputVariable => registerDynamicallyAndStaticallyListenedElement(inputVariable))
      .toArray

  private var outputValue: Int =
    inputVariables.map(_.asInstanceOf[TestVaryingVariable].startValue).sum

  def addIncredibleBulk(incredibleBulk: IncredibleBulk): Unit = {
    this.registerStaticallyListenedElement(incredibleBulk)
  }

  override def notifyTestVariableChanged(
    testVaryingVariable: TestVaryingVariable,
    oldValue: Int,
    newValue: Int
  ): Unit = {
    outputValue = outputValue - oldValue + newValue
    outputVariable.get.asInstanceOf[TestVaryingVariable].setValue(outputValue)
  }

  /** This is the debug procedure through which propagation element can redundantly check that the
    * incremental computation they perform through the performPropagation method is correct
    * overriding this method is optional, so an empty body is provided by default
    */
  override def checkInternals(): Unit = {}
}
