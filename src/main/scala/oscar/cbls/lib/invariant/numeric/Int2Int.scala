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

package oscar.cbls.lib.invariant.numeric

import oscar.cbls.core.computation.{Invariant, Store}
import oscar.cbls.core.computation.integer.{IntNotificationTarget, IntVariable}


/** An helper to define an [[Invariant]] from an Int -> Int function.
 * This invariant is not incremental. So, it should be only use for very simple functions.
 * It maintains toValue = fun(fromValue)
 *
 * @param model
 *  The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
 * @param fromValue
 *  The listened [[IntVariable]].
 * @param toValue
 *  The [[IntVariable]] which contains fun(fromValue)
 * @param fun
 *  The function to maintain. It is supposed not yo listen to any variable in the model
 * @param cached
 *  Set to true to have a cache of size1. Set to false to have no cache.
 *  A cache can provide speedup if fun is time-consuming.
 * @param name
 *   The name (optional) of your Invariant
 */
class Int2Int (model: Store,
               fromValue: IntVariable,
               toValue: IntVariable,
               fun: Long => Long,
               cached: Boolean = false,
               name: Option[String] = None)
extends Invariant(model, name) with IntNotificationTarget
{
  fromValue.registerStaticallyAndDynamicallyListeningElement(this)
  toValue.setDefiningInvariant(this)

  toValue := fun(fromValue.value())

  private[this] var cachedIn: Long = fromValue.value()
  private[this] var cachedOut: Long = toValue.value()

  @inline
  override def notifyIntChanges(intVariable: IntVariable,
                                contextualVarIndex: Int,
                                oldVal: Long,
                                newVal: Long): Unit = {
    if(cached){
      if (newVal == cachedIn){
        val tmp = cachedOut
        cachedIn = oldVal
        cachedOut = toValue.value()
        toValue := tmp
      } else{
        cachedIn = oldVal
        cachedOut = toValue.value()
        toValue := fun(newVal)
      }
    } else {
      toValue := fun(newVal)
    }

  }

  override def checkInternals(): Unit = {
    require(toValue.value() == fun(fromValue.value()), s"toValue is not equal to fun(fromValue). " +
      s"fromValue: $fromValue - toValue: $toValue")
  }
}
