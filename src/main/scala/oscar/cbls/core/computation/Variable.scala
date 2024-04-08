package oscar.cbls.core.computation

import oscar.cbls.algo.dll.DoublyLinkedList
import oscar.cbls.core.propagation._

/** Abstract structure for Variable definition.
  *
  * It adds some functionalities like, domain definition, dynamically listening to other propagation
  * element and saving the state of the variable.
  *
  * @param propagationStructure
  *   The propagation structure to which the element is attached
  * @param isConstant
  *   If the variable is a constant
  */
abstract class Variable(
  propagationStructure: PropagationStructure,
  isConstant: Boolean,
  name: Option[String] = None
) extends PropagationElement(propagationStructure) {
  require(propagationStructure != null, "The propagation structure must be defined")

  def name(): String = name.getOrElse(s"Variable_$id")

  private var _domain: Option[(Long, Long)]              = None
  private[core] var definingInvariant: Option[Invariant] = None
  // Dynamically listening elements, upon update this variable must noticed it's listening element.
  private val dynamicallyListeningElements: DoublyLinkedList[(PropagationElement, Int)] =
    if (isConstant) null else new DoublyLinkedList[(PropagationElement, Int)]()

  /** Limits the values of the variable to this domain. ONLY USED IN DEBUG MODE */
  def setDomain(min: Long, max: Long): Unit = _domain = Some((min, max))
  def domain: Option[(Long, Long)]          = _domain

  /** Save the state of this variable */
  def save(): SavedValue

  /** Sets the invariant as the structure defining the value of this variable.
    *
    * @param invariant
    *   The defining Invariant
    */
  def setDefiningInvariant(invariant: Invariant): Unit = {
    definingInvariant = Some(invariant)
    registerStaticallyListenedElement(invariant)
  }

  /** Whether or not this variable is a decision variable. A decision variable is a variable that is
    * not defined by any invariant.
    */
  def isADecisionVariable: Boolean = definingInvariant.isEmpty || isConstant

  /** Registers dynamically the PropagationElement as a listening element. Whenever the Variable
    * updates it's value, the listening element will be noticed.
    *
    * NOTE : Keep the returned value to be able to remove it from the listening
    * [[oscar.cbls.algo.dll.DoublyLinkedList]] using it's delete method.
    * @param invariant
    *   The new listening element
    * @param variableIndex
    *   The variable index within invariant context (default -1 if not necessary)
    * @return
    *   A key to ease the removal of this element
    */
  private[computation] def registerDynamicallyListeningElement(
    invariant: Invariant,
    variableIndex: Int
  ): DoublyLinkedList[(PropagationElement, Int)]#DLLStorageElement = {
    require(
      !isConstant,
      "Constant variable does not propagate, no need to keep track of listening element."
    )
    dynamicallyListeningElements.insertStart((invariant, variableIndex))
  }

  /** Returns dynamically listening propagation elements.
    *
    * Useful when performing propagation.
    */
  protected final def getDynamicallyListeningElements: DoublyLinkedList[(PropagationElement, Int)] =
    dynamicallyListeningElements

  /** Checks if the given value is within the domain */
  def checkValueWithinDomain(value: Long): Boolean = {
    domain match {
      case None             => true
      case Some((min, max)) => value >= min && value <= max
    }
  }

  override def toString: String = this.name()
}
