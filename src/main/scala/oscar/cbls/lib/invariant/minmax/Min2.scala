package oscar.cbls.lib.invariant.minmax

import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.lib.invariant.IntIntToIntInvariant

/** The invariant that maintains the minimum of two integer
 *
 * @param model
 *    The [[oscar.cbls.core.propagation.PropagationStructure]] to which this Invariant is linked
 * @param a
 *    The first [[IntVariable]] input
 * @param b
 *    The second [[IntVariable]] input
 * @param toValue
 *    The [[IntVariable]] that contains min(a, b)
 */
private class Min2(model : Store, a: IntVariable, b: IntVariable, toValue : IntVariable) extends
  IntIntToIntInvariant(model, a, b, toValue, (x: Long, y: Long) => x.min(y)){}

object Min2{
  def apply(model : Store, a: IntVariable, b: IntVariable, toValue : IntVariable): Unit = {
    new Min2(model, a, b, toValue)
  }
}
