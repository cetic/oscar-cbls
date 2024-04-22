package oscar.cbls.lib.invariant

import oscar.cbls.core.computation.{Invariant, Store}
import oscar.cbls.core.computation.integer.{IntVariable, IntNotificationTarget}

/**A general invariant that maintains the value of a function on two integer
 *
 * @param model
 *    The [[oscar.cbls.core.propagation.PropagationStructure]] to which this Invariant is linked
 * @param a
 *    The first [[IntVariable]] input
 * @param b
 *    The second [[IntVariable]] input
 * @param toValue
 *    The [[IntVariable]] that contains f(a, b)
 * @param f
 *    The two integer function to maintain
 */
class IntIntToIntInvariant(model: Store,
                           a: IntVariable,
                           b: IntVariable,
                           toValue : IntVariable,
                           f: (Long, Long) => Long
                           )
  extends Invariant(model) with  IntNotificationTarget {

  a.registerDynamicallyListeningElement(this)
  b.registerDynamicallyListeningElement(this)

  toValue.setDefiningInvariant(this)
  toValue := f(a.value(), b.value())


  override def notifyIntChanges(intVariable: IntVariable, index: Int, oldVal: Long, newVal: Long): Unit = {
    toValue := f(a.value(), b.value())
  }

  override def checkInternals(): Unit = {
    require(toValue.value() == f(a.value(), b.value()))
  }
}
