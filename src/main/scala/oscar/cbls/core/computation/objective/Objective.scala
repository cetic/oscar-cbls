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

package oscar.cbls.core.computation.objective

import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable

object Objective {
  def apply(objectiveValue: IntVariable): Objective = {
    require(!objectiveValue.isConstant, "An Objective value can not be constant")
    new Objective(objectiveValue.model, objectiveValue)
  }
}

class Objective(model: Store, objectiveValue: IntVariable)
    extends AbstractObjective(model) {

  model.registerForPartialPropagation(objectiveValue)

  override def value: Long = objectiveValue.value()

  override def detailedString(short: Boolean): String = objectiveValue.toString
}
