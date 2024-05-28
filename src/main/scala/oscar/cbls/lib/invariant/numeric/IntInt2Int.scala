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

class IntInt2Int(model: Store,
                 a: IntVariable,
                 b: IntVariable,
                 output: IntVariable,
                 fun: (Long, Long) => Long,
                 name: Option[String] = None)
extends Invariant(model, name) with IntNotificationTarget {


  a.registerStaticallyAndDynamicallyListeningElement(this)
  b.registerStaticallyAndDynamicallyListeningElement(this)

  output.setDefiningInvariant(this)
  output := fun(a.value(), b.value())

  @inline
  override def notifyIntChanges(intVariable: IntVariable,
                                contextualVarIndex: Int,
                                oldVal: Long,
                                newVal: Long): Unit = {
    output := fun(a.value(), b.value())
  }

  override def checkInternals(): Unit ={
    require(output.value() == fun(a.value(), b.value()),
      s"output != fun(a, b). output: $output - a: $a - b: $b")
  }
}
