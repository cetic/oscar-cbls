package oscar.cbls.core.propagation

/**
 * it does not changes it listened elements
 * however, its listening elements might change, and a proper list must therefore be kept.
 */
abstract class PropagationElement(propagationStructure : PropagationStructure) {

  private var layer_ : Int = 0

  def layer = layer_

  def layer_=(newLayer : Int) = layer_ = newLayer

  private val id_ = propagationStructure.generateId()

  private[propagation] def uniqueId = id_

  /**to invoque to force inclusion of the propagation element in the current or next propagation wave. */
  final def scheduleForPropagation(): Unit = ???

  /**
   * this is the propagation method that should be overridden by propagation elements.
   * notice that it is only called in a propagation wave if:
   * 1L: it has been registered for propagation since the last time it was propagated
   * 2L: it is included in the propagation wave: partial propagation wave do not propagate all propagation elements;
   *    it only propagates the ones that come in the predecessors of the targeted propagation element
   *  overriding this method is optional, so an empty body is provided by default
   */
  def performPropagation(): Unit

  /**
   * This is the debug procedure through which propagation element can redundantly check
   * that the incremental computation they perform through the performPropagation method is correct
   * overriding this method is optional, so an empty body is provided by default
   */
  def checkInternals(): Unit


}
