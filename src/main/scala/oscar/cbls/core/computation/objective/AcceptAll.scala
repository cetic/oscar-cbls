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
import oscar.cbls.core.search.profiling.{NeighborhoodProfiler, SearchProfiler}
import oscar.cbls.core.search.{Move, MoveFound, SimpleNeighborhood}

/** Companion object of AcceptAll */
object AcceptAll {
  def apply(objValue: IntVariable): AcceptAll = {
    new AcceptAll(objValue)
  }
}

/** This particular implementation of Objective accepts all movements no matter what.
  *
  * It can be used in particular cases. For instance, in the DynAndThen Combinator you want to
  * evaluate the composition of two movements, hence the first one must be accepted no matter what.
  * @param objValue
  *   The objective value
  */
class AcceptAll(objValue: IntVariable) extends Objective(objValue) {

  override lazy val worstValue: Long = 0L

  override def isValueNewBest(currentBest: Long, newValue: Long): Boolean = true

  override def newExploration[M <: Move](searchProfilerOpt: Option[NeighborhoodProfiler]): Exploration[M] =
    new Exploration[M](currentObjValue(), searchProfilerOpt) {
      override def checkNeighbor(buildMove: Long => M): Unit = {
        val newValue = objValue.value()
        _toReturn = MoveFound(buildMove(newValue))
        verboseMode.moveExplored(() => buildMove(newValue), valid = true, saved = true)
        super.checkNeighbor(buildMove)
      }
    }

  override def toString: String = "Accept all new value"
}
