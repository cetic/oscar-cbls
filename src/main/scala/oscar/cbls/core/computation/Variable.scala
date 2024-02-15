package oscar.cbls.core.computation

import oscar.cbls.algo.dll.DoublyLinkedList
import oscar.cbls.core.propagation._

abstract class Variable(propagationStructure: PropagationStructure, isConstant: Boolean)
    extends PropagationElement(propagationStructure) {

  protected var domainMin: Int = Int.MinValue
  protected var domainMax: Int = Int.MaxValue
  private[core] var definingInvariant: Option[Invariant] = None
  // Dynamically listening elements, upon update this variable must noticed it's listening element.
  private val dynamicallyListeningElements: DoublyLinkedList[PropagationElement] =
    if (isConstant) null else new DoublyLinkedList[PropagationElement]()

  def model: Store = propagationStructure.asInstanceOf[Store]

  /** Limits the values of the variable to this domain. ONLY USED IN DEBUG MODE */
  def setDomain(min: Int, max: Int): Unit = {
    domainMin = min
    domainMax = max
  }

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

  /** Registers the [[Iterable]] of [[PropagationElement]] as a listening elements. Whenever the Variable updates it's
   * value, the listening elements will be noticed.
   *
   * NOTE : Keep the returned value to be able to remove them from the listening [[DoublyLinkedList]]
   * using their delete method.
   * @param elems
   *   An iterable of listening elements
   * @return
   *   An iterable of element in the DLL
   */
  def registerDynamicallyListeningElements(
    elems: Iterable[PropagationElement]
  ): Iterable[DoublyLinkedList[PropagationElement]#DLLStorageElement] = {
    require(
      !isConstant,
      "Constant variable does not propagate, no need to keep track of listening element."
    )
    elems.map(dynamicallyListeningElements.insertStart)
  }

  /** Registers the PropagationElement as a listening element. Whenever the Variable updates it's
    * value, the listening element will be noticed.
    *
    * NOTE : Keep the returned value to be able to remove it from the listening [[DoublyLinkedList]]
    * using it's delete method.
    * @param elem
    *   The new listening element
    * @return
    *   The element in the DLL
    */
  def registerDynamicallyListeningElement(
    elem: PropagationElement
  ): DoublyLinkedList[PropagationElement]#DLLStorageElement = {
    require(
      !isConstant,
      "Constant variable does not propagate, no need to keep track of listening element."
    )
    dynamicallyListeningElements.insertStart(elem)
  }

  /** Returns dynamically listening propagation elements.
    *
    * Useful when performing propagation.
    */
  protected[core] final def getDynamicallyListeningElements: DoublyLinkedList[PropagationElement] =
    dynamicallyListeningElements
}
