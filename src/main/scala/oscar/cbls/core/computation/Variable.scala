package oscar.cbls.core.computation

import oscar.cbls.algo.dll.DoublyLinkedList
import oscar.cbls.core.propagation._

abstract class Variable(propagationStructure: PropagationStructure)
    extends PropagationElement(propagationStructure) {

  private var definingInvariant: Option[Invariant] = None
  // Dynamically listening elements, overrides it for constant variables
  private val dynamicallyListeningElements: DoublyLinkedList[PropagationElement] =
    new DoublyLinkedList[PropagationElement]()

  def model: Store = propagationStructure.asInstanceOf[Store]

  def save(): SavedValue

  /** Sets the invariant as the structure defining the value of this variable.
   *
   * @param invariant
   *   The defining Invariant
   */
  def setDefiningInvariant(invariant: Invariant): Unit =
    definingInvariant = Some(invariant)

  /** Whether or not this variable is a decision variable. A decision variable is a variable that is
   * not defined by any invariant.
   */
  def isADecisionVariable: Boolean = definingInvariant.isEmpty

  override def registerDynamicallyListeningElement(
    elem: PropagationElement
  ): DoublyLinkedList[PropagationElement]#DLLStorageElement =
    dynamicallyListeningElements.insertStart(elem)

  /** Returns dynamically listening propagation elements.
    *
    * Useful when performing propagation.
    */
  protected[core] final def getDynamicallyListeningElements: DoublyLinkedList[PropagationElement] =
    dynamicallyListeningElements

  /** this is the propagation method that should be overridden by propagation elements. notice that
    * it is only called in a propagation wave if: 1L: it has been registered for propagation since
    * the last time it was propagated 2L: it is included in the propagation wave: partial
    * propagation wave do not propagate all propagation elements; it only propagates the ones that
    * come in the predecessors of the targeted propagation element overriding this method is
    * optional, so an empty body is provided by default
    */
  override def performPropagation(): Unit

  /** This is the debug procedure through which propagation element can redundantly check that the
    * incremental computation they perform through the performPropagation method is correct
    * overriding this method is optional, so an empty body is provided by default
    */
  override def checkInternals(): Unit
}
