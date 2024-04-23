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

package oscar.cbls.lib.invariant.minmax

import oscar.cbls.algo.heap.BinaryHeapWithMoveIntItem
import oscar.cbls.core.computation.{Invariant, KeyForRemoval, Store}
import oscar.cbls.core.computation.integer.{IntNotificationTarget, IntVariable}

//TODO: manage condition on considered variables when SetVariable will be available

/**Abstract Invariant that maintains Extremum(vars(i) | i in cond)
 * Exact ordering is specified by implementing abstract method of the class.
 * Update is in O(log(n))
 *
 * @param model
 *    The [[oscar.cbls.core.propagation.PropagationStructure]] to which this Invariant is linked
 * @param vars
 *    An [[IndexedSeq]] of [[IntVariable]]
 * @param output
 *    The output [[IntVariable]]
 * @param default
 *    The default value of the extremum
 * @param name
 *   The name (optional) of your Invariant
 */
abstract class Extremum(model: Store,
                        vars: IndexedSeq[IntVariable],
                        output: IntVariable,
                        default: Long,
                        name: Option[String] = None)
  extends Invariant(model, name) with IntNotificationTarget {

  private[this] val keysForRemoval: Array[KeyForRemoval[_]] = new Array(vars.length)

  //Use to stock the indices of the listened variables. All operation are in O(log(n))
  private[this] val h: BinaryHeapWithMoveIntItem = BinaryHeapWithMoveIntItem((i: Int) => ord(vars(i)),
                                                                          vars.length, vars.length)

  for(i <- vars.indices) {
    h.insert(i)
    keysForRemoval(i) = vars(i).registerDynamicallyListeningElement(this, i)
  }

  h.getFirst match {
    case None => output := default
    case Some(i) => output := vars(i).value()
  }

  def ord(v: IntVariable) : Long

  override def notifyIntChanges(intVariable: IntVariable, index: Int, oldVal: Long, newVal: Long): Unit = {
    h.notifyChange(index) //Update the heap
    output := vars(h.getFirst.get).value() //The extremum has possibly change. We update it
  }


}
