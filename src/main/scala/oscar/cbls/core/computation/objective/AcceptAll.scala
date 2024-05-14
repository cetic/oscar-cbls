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

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.search.{Move, MoveFound}

/** Companion object of AcceptAll */
object AcceptAll {
  def apply(solutionValue: IntVariable): AcceptAll = {
    solutionValue.model.registerForPartialPropagation(solutionValue)
    new AcceptAll(solutionValue)
  }
}

/** This particular implementation of Objective accepts all movements no matter what.
  *
  * It can be used in particular cases. For instance, in the DynAndThen Combinator you want to
  * evaluate the composition of two movements, hence the first one must be accepted no matter what.
  * @param objective
  *   The objective value
  */
class AcceptAll(objective: IntVariable) extends Objective {

  override def newExploration: Exploration = new Exploration {
    override def checkNeighbor(buildMove: Long => Move): Unit = {
      val newValue = objective.value()
      _toReturn = MoveFound(buildMove(newValue))
    }
  }
}
