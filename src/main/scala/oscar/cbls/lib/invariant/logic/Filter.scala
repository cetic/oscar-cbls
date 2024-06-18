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

package oscar.cbls.lib.invariant.logic

import oscar.cbls.core.computation.{IncredibleBulk, Invariant, KeyForRemoval, Store}
import oscar.cbls.core.computation.integer.{IntNotificationTarget, IntVariable}
import oscar.cbls.core.computation.set.SetVariable

class Filter(
  model: Store,
  input: Array[IntVariable],
  output: SetVariable,
  predicate: Long => Boolean = _ > 0,
  bulkIdentifier: Option[String] = None,
  name: Option[String] = None
) extends Invariant(model, name)
    with IntNotificationTarget {

  bulkIdentifier match {
    case None =>
      // No bulk is used
      for (vars <- input) this.registerStaticallyListenedElement(vars)
    case Some(bulkId) =>
      // Register static dependency via a bulk
      this.addIncredibleBulk(IncredibleBulk.bulkRegistering(input, bulkId, model))
  }

  output := Set.empty
  for (i <- input.indices) {
    val v = input(i)
    v.registerDynamicallyListeningElement(this, i)
    if (predicate(v.value())) output :+= v.value().toInt
  }

  output.setDefiningInvariant(this)

  override def notifyIntChanges(
    intVariable: IntVariable,
    contextualVarIndex: Int,
    oldVal: Long,
    newVal: Long
  ): Unit = ???

  override def checkInternals(): Unit = ???
}
