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
package oscar.cbls.core.propagation

/** An interface that provides the methods to handle propagation for the element of the propagation
  * graph
  *
  * @param propagationStructure
  *   The propagation structure to which the element is attached
  */
abstract class PropagationElement(private val propagationStructure: PropagationStructure) {

  private[propagation] var staticallyListenedElements: List[PropagationElement] = List()

  private[propagation] var staticallyListeningElements: List[PropagationElement] = List()

  private val id_ : Int = propagationStructure.registerAndGenerateId(this)

  private[core] def id: Int = id_

  private var scheduled: Boolean = false

  private var layer_ : Int = -1

  private[propagation] def layer: Int = layer_

  private[propagation] def layer_=(layer: Int): Unit = layer_ = layer

  /** Schedules the propagation element in the next propagation waves
    */
  final def scheduleForPropagation(): Unit = {
    if (!scheduled) {
      scheduled = true
      propagationStructure.scheduleElementForPropagation(this)
    }
  }

  /** Register an element as listened by this propagation element
    *
    * When an element listens to another element, it means that this element depends on the one it
    * listens. If a listened element is modified, this element has to be modified by the propagation
    * wave.
    *
    * @param elem
    *   The element to insert
    */
  protected[this] def registerStaticallyListenedElement(elem: PropagationElement): Unit = {
    require(
      this.propagationStructure == elem.propagationStructure,
      "Two elements that listen from each other shall be in the same propagation structure"
    )
    require(
      !propagationStructure.closed,
      "Cannot add a listened element when the structure is closed"
    )

    staticallyListenedElements = elem :: staticallyListenedElements
    elem.registerStaticallyListeningElement(this)
  }

  private[propagation] def registerStaticallyListeningElement(elem: PropagationElement): Unit = {
    staticallyListeningElements = elem :: staticallyListeningElements
  }

  private[core] def propagateElement(): Unit = {
    performPropagation()
    scheduled = false
  }

  /** Is called when the element is allowed to propagate.
    *
    * This method is only called in a propagation wave if: 1/ the element has been registered for
    * propagation since the last time it was propagated and 2/ it is included in the propagation
    * wave (partial propagation wave do not propagate all propagation elements). Overriding this
    * method is optional (the element can update their values immediately when they are notified), so
    * an empty body is provided by default
    */
  def performPropagation(): Unit = {
    require(false,"The element has been scheduled for propagation but the method performPropagation has not been overridden")
  }

  /** Allows to check and debug propagation elements.
    *
    * This method can be called after the propagation according to the debug level of the
    * propagation structure (see [[PropagationStructure]]). It can be used to check if the invariant
    * worked properly by, for example, recomputing the value from scratch.
    */
  def checkInternals(): Unit
}
