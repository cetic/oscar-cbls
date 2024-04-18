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
  * @param name
  *   The name (optional) of your Variable
  */
abstract class Variable(
  propagationStructure: PropagationStructure,
  isConstant: Boolean,
  name: Option[String] = None
) extends PropagationElement(propagationStructure) {
  require(propagationStructure != null, "The propagation structure must be defined")

  /** The trait type that the [[Invariant]] must extends in order to receive notifications. This
    * type MUST be overridden when defining a new Variable
    */
  type NotificationTargetType

  def name(): String = name.getOrElse(s"Variable_$id")

  private var _domain: Option[(Long, Long)]              = None
  private[core] var definingInvariant: Option[Invariant] = None
  // Dynamically listening elements, upon update this variable must noticed it's listening element.
  private val dynamicallyListeningElements: DoublyLinkedList[(NotificationTargetType, Int)] =
    if (isConstant) null else new DoublyLinkedList[(NotificationTargetType, Int)]()

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
    require(!isConstant, "A constant Variable cannot have a defining Invariant")
    definingInvariant = Some(invariant)
    registerStaticallyListenedElement(invariant)
  }

  /** Whether or not this variable is a decision variable. A decision variable is a variable that is
    * not defined by any invariant.
    */
  def isADecisionVariable: Boolean = definingInvariant.isEmpty

  /** Registers dynamically the [[oscar.cbls.core.propagation.PropagationElement]] as a listening element. Whenever the Variable
    * updates its value, the listening element will be noticed.
    *
    * NOTE : Keep the returned value to be able to remove it from the listening
    * [[oscar.cbls.algo.dll.DoublyLinkedList]] using it's delete method.
    *
    * @param target
    *   The new listening [[oscar.cbls.core.propagation.PropagationElement]]
    * @param indexToRecallAtNotification
    *   The index that the variable will recall when notifying the PropagationElement about changes
    * @return
    *   A key to ease the removal of this element
    */
  def registerDynamicallyListeningElement(
    target: NotificationTargetType,
    indexToRecallAtNotification: Int = -1
  ): KeyForRemoval[(NotificationTargetType, Int)] = {
    require(
      !isConstant,
      "Constant variable does not propagate, no need to keep track of listening element."
    )
    KeyForRemoval(dynamicallyListeningElements.insertStart((target, indexToRecallAtNotification)))
  }

  /** Registers statically and dynamically the PropagationElement as a listening element. Whenever
    * the Variable updates its value, the listening element will be noticed.
    *
    * NOTE : Keep the returned value to be able to remove it from the listening
    * [[oscar.cbls.algo.dll.DoublyLinkedList]] using it's delete method.
    *
    * @param propagationElement
    *   The new listening PropagationElement
    * @param indexToRecallAtNotification
    *   The index that the variable will recall when notifying the invariant about changes
    * @return
    *   A key to ease the removal of this element
    */
  def registerStaticallyAndDynamicallyListeningElement(
    propagationElement: Invariant with NotificationTargetType,
    indexToRecallAtNotification: Int = -1
  ): KeyForRemoval[(NotificationTargetType, Int)] = {
    registerStaticallyListeningElement(propagationElement)
    registerDynamicallyListeningElement(propagationElement, indexToRecallAtNotification)
  }

  /** Returns dynamically listening propagation elements.
    *
    * Useful when performing propagation.
    */
  protected final def getDynamicallyListeningElements
    : DoublyLinkedList[(NotificationTargetType, Int)] =
    dynamicallyListeningElements

  /** Checks if the given value is within the domain */
  def isValueWithinDomain(value: Long): Boolean = {
    domain match {
      case None             => true
      case Some((min, max)) => value >= min && value <= max
    }
  }

  override def toString: String = this.name()
}
