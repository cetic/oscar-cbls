package oscar.cbls.core.propagation

import oscar.cbls.algo.dll.DoublyLinkedList

/** An interface that provides the method to handle propagation for the element of the propopagation
  * graph
  *
  * @param propagationStructure
  *   The propagation structure to which the element is attached
  */
abstract class PropagationElement(propagationStructure: PropagationStructure) {

  private[propagation] var staticallyListenedElements: List[PropagationElement] = List()

  private[propagation] var staticallyListeningElements: List[PropagationElement] = List()

  private val id_ : Int = propagationStructure.registerAndGenerateId(this)

  private[core] def id: Int = id_

  private var layer_ : Int = -1

  private[propagation] def layer: Int = layer_

  private[propagation] def layer_=(layer: Int): Unit = layer_ = layer

  /** Schedules the propagation element in the next propagation waves
    */
  final def scheduleForPropagation(): Unit = {
    propagationStructure.scheduleElementForPropagation(this)
  }

  /** Register an element as listened by this propagation element
    *
    * When a element listen another element, it means that this element dependes on the one it
    * listens. If a listened element is modified, this element has to be modified by the propagation
    * wave
    *
    * @param elem
    *   The element to insert
    */
  protected def registerStaticallyListenedElement(elem: PropagationElement): Unit = {
    staticallyListenedElements = elem :: staticallyListenedElements
    elem.registerStaticallyListeningElement(this)
  }

  private[propagation] def registerStaticallyListeningElement(elem: PropagationElement): Unit = {
    staticallyListeningElements = elem :: staticallyListeningElements
  }

  /** this is the propagation method that should be overridden by propagation elements. notice that
    * it is only called in a propagation wave if: 1L: it has been registered for propagation since
    * the last time it was propagated 2L: it is included in the propagation wave: partial
    * propagation wave do not propagate all propagation elements; it only propagates the ones that
    * come in the predecessors of the targeted propagation element overriding this method is
    * optional, so an empty body is provided by default
    */
  def performPropagation(): Unit

  /** This is the debug procedure through which propagation element can redundantly check that the
    * incremental computation they perform through the performPropagation method is correct
    * overriding this method is optional, so an empty body is provided by default
    */
  def checkInternals(): Unit
}
