package oscar.cbls.core.computation.integer

import oscar.cbls.core.computation.{Invariant, Store}

/** An Invariant that maintains a copy of a given [[IntVariable]].
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this Invariant is linked
  * @param fromValue
  *   The copied IntVariable
  * @param toValue
  *   The copy
  */
class IdentityIntInvariant(model: Store, fromValue: IntVariable, toValue: IntVariable)
    extends Invariant(model)
    with IntNotificationTarget {

  registerStaticallyListenedElement(fromValue)
  toValue.setDefiningInvariant(this)

  toValue := fromValue.value()

  override def notifyIntChanged(intVariable: IntVariable, oldVal: Long, newVal: Long): Unit = {
    toValue := newVal
  }

  /** this is the propagation method that should be overridden by propagation elements. notice that
   * it is only called in a propagation wave if: 1L: it has been registered for propagation since
   * the last time it was propagated 2L: it is included in the propagation wave: partial
   * propagation wave do not propagate all propagation elements; it only propagates the ones that
   * come in the predecessors of the targeted propagation element overriding this method is
   * optional, so an empty body is provided by default
   */
  override def performPropagation(): Unit = ???

  /** This is the debug procedure through which propagation element can redundantly check that the
   * incremental computation they perform through the performPropagation method is correct
   * overriding this method is optional, so an empty body is provided by default
   */
  override def checkInternals(): Unit = ???
}
