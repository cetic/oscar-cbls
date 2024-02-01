package oscar.cbls.core.propagation

/** An interface that provides the method to handle propagation for the element of the propopagation
  * graph
  *
  * @param propagationStructure
  *   The propagation structure to which the element is attached
  */
abstract class PropagationElement(propagationStructure: PropagationStructure) {

  /** Schedules the propagation element in the next propagation waves
    */
  final def scheduleForPropagation(): Unit = ???

  /** Register an element as listened by this propagation element
    *
    * When a element listen another element, it means that this element dependes on the one it
    * listens. If a listened element is modified, this element has to be modified by the propagation
    * wave
    *
    * @param elem
    *   The element to insert
    */
  protected def registerStaticallyListenedElement(elem: PropagationElement): Unit = ???

  /** Add an element in the set of dynamically listened element
    *
    * Some element may not always listened for the same elements (e.g. the "element" invariant).
    * This methods allow to add an element in the set of element that are listened by an element. It
    * returns a key that allows to remove the element from the set of listened element in constant
    * time.
    *
    * @param elem
    *   The element to insert
    * @return
    *   The key to perform the remove
    */
  protected def registerDynamicallyListenedElement(elem: PropagationElement): KeyForRemoval =
    ???

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
