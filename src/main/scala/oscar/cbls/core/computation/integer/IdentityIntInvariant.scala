package oscar.cbls.core.computation.integer

import oscar.cbls.core.computation.{Invariant, Store}

class IdentityIntInvariant(model: Store, fromValue: IntVariable, toValue: IntVariable) extends Invariant(model) with IntNotificationTarget {

  registerStaticallyListenedElement(fromValue)
  toValue.setDefiningInvariant(this)

  toValue := fromValue.value()

  override def notifyIntChanged(intVariable: IntVariable, oldVal: Long, newVal: Long): Unit = {
    toValue := newVal
  }
}
