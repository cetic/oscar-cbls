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
}
