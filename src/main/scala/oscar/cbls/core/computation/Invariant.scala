package oscar.cbls.core.computation

import oscar.cbls.algo.dll.DoublyLinkedList
import oscar.cbls.core.propagation._

/** Abstract structure for Invariant definition.
  *
  * An invariant is a propagation element that maintains the value of output variable given
  * modification of the input variable(s). For instance, a identity invariant that take a
  * IntVariable as input and output will ensure that the output variable is equal to the input
  * variable.
  *
  * This abstract class provides an "empty" implementation of the method performPropagation. There
  * is two way of propagating invariant result to it's output variable. When you receive a
  * notification of modification from the input variable you ...
  *   - ... directly set the new value of the output variable. In that case you do not need to
  *     override performPropagation()
  *   - ... do some computation and SCHEDULE the invariant FOR PROPAGATION. In that case you need to
  *     override performPropagation()
  *
  * @param propagationStructure
  *   The propagation structure to which the element is attached
  */
abstract class Invariant(propagationStructure: PropagationStructure)
    extends PropagationElement(propagationStructure) {

  // Dynamically listened elements, this invariant will receive notification sent by those element
  private val dynamicallyListenedElements: DoublyLinkedList[(PropagationElement, Int)] =
    new DoublyLinkedList[(PropagationElement, Int)]()

  /** Registers the Variable as dynamically listened by this Invariant.
    *
    * Used during the search so that the variable knows which invariant needs to be notified.
    *
    * @param variable
    *   The listened variable
    */
  def registerDynamicallyListenedElement(variable: Variable, index: Int = -1): KeyForRemoval = {
    // TODO : Prevent user to register several times the same variable
    KeyForRemoval(
      variable.registerDynamicallyListeningElement(this),
      dynamicallyListenedElements.insertStart((variable, index))
    )
  }

  /** Registers the Variable as statically and dynamically listened by this Invariant.
    *
    * Used when building the propagation graph (statically) and during the search so that the
    * variable knows which invariant needs to be notified.
    *
    * @param variable
    *   The listened variable
    */
  def registerDynamicallyAndStaticallyListenedElement(
    variable: Variable,
    index: Int = -1
  ): KeyForRemoval = {
    super.registerStaticallyListenedElement(variable)
    registerDynamicallyListenedElement(variable, index)
  }

  def getDynamicallyListenedElements: DoublyLinkedList[(PropagationElement, Int)] =
    dynamicallyListenedElements
}
