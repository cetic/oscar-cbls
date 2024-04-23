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
 * @param output
 *    The [[IntVariable]] that contains f(a, b)
 * @param f
 *    The two integer function to maintain
 * @param name
 *    The (optional) name of the invariant
 */
class IntIntToIntInvariant(model: Store,
                           a: IntVariable,
                           b: IntVariable,
                           output : IntVariable,
                           f: (Long, Long) => Long,
                           name: Option[String] = None
                           )
  extends Invariant(model, name) with  IntNotificationTarget {

  a.registerStaticallyAndDynamicallyListeningElement(this)
  b.registerStaticallyAndDynamicallyListeningElement(this)

  output.setDefiningInvariant(this)
  output := f(a.value(), b.value())


  override def notifyIntChanges(intVariable: IntVariable, index: Int, oldVal: Long, newVal: Long): Unit = {
    output := f(a.value(), b.value())
  }

  override def checkInternals(): Unit = {
    require(output.value() == f(a.value(), b.value()))
  }
}
