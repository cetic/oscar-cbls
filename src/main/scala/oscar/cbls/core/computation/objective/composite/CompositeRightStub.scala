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

package oscar.cbls.core.computation.objective.composite

import oscar.cbls.core.computation.objective.{Exploration, Objective}
import oscar.cbls.core.search.Move
import oscar.cbls.core.search.profiling.NeighborhoodProfiler

/** Objective dedicated to [[oscar.cbls.lib.neighborhoods.combinator.DynAndThen]]'s right
  * neighborhood.
  *
  * __How does it work ?__
  *   - Right Neighborhood uses this custom Objective to get its Exploration instance.
  *   - Knowing that a left move has been performed, it checks if performing a right move in
  *     addition improve the base objective.
  *   - The [[oscar.cbls.core.search.SearchResult]] is saved to construct a composite move.
  *
  * @param baseObj
  *   The Objective defined by the user used to guide the search.
  * @param baseObjExplorer
  *   An Exploration instance based on baseObj. Will be used to eval the
  *   [[oscar.cbls.lib.neighborhoods.combinator.CompositeMove]].
  * @param leftMove
  *   A Move instantiate by the left neighborhood. It'll be used to generate the CompositeMove.
  */
private[objective] class CompositeRightStub(
  baseObj: Objective,
  baseObjExplorer: Exploration[Move],
  leftMove: Move
) extends Objective(baseObj.objValue, baseObj.mustBeZero) {

  override lazy val worstValue: Long = baseObj.worstValue

  override def newExploration[M <: Move](
    searchProfilerOpt: Option[NeighborhoodProfiler]
  ): Exploration[M] = {
    new Exploration[M](baseObjExplorer.oldObj, searchProfilerOpt) {
      override def checkNeighbor(buildMove: Long => M): Unit = {
        val newValue  = objValue.value()
        val rightMove = buildMove(newValue)
        baseObjExplorer.checkNeighbor(_ => rightMove)
        _toReturn = baseObjExplorer.toReturn
      }
    }
  }

  override def isValueNewBest(currentBest: Long, newValue: Long): Boolean = {
    baseObj.isValueNewBest(currentBest, newValue)
  }
}
