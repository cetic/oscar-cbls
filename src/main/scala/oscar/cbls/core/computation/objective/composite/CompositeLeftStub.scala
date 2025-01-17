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
import oscar.cbls.core.search.{Move, Neighborhood}
import oscar.cbls.core.search.profiling.{CompositionProfiler, NeighborhoodProfiler}
import oscar.cbls.lib.neighborhoods.combinator.CompositeMove

/** Companion object of CompositeLeftStub. */
object CompositeLeftStub {
  def apply(
    baseObj: Objective,
    baseObjExplorer: Exploration[CompositeMove],
    right: Move => Neighborhood,
    compositionProfiler: Option[CompositionProfiler]
  ): CompositeLeftStub = {
    new CompositeLeftStub(baseObj, baseObjExplorer, right, compositionProfiler)
  }
}

/** Objective dedicated to [[oscar.cbls.lib.neighborhoods.combinator.DynAndThen]]'s left
  * neighborhood.
  *
  * __How does it work ?__
  *   - Left Neighborhood uses this custom Objective to get its Exploration instance.
  *   - Instead of checking a left move, we instantiate the move and use it to generate the right
  *     Neighborhood with its own custom [[CompositeRightStub]] Objective.
  *   - Finally, the result of this search is given by the right neighborhood when asking a move
  *     from it.
  *
  * @param baseObj
  *   The Objective defined by the user used to guide the search.
  * @param baseObjExplorer
  *   An Exploration instance based on baseObj. Will be used to eval the
  *   [[oscar.cbls.lib.neighborhoods.combinator.CompositeMove]].
  * @param right
  *   A function returning a right neighborhood based on a left move.
  * @param compositionProfiler
  *   The profiler of the DynAndThen.
  */
class CompositeLeftStub(
  baseObj: Objective,
  baseObjExplorer: Exploration[CompositeMove],
  right: Move => Neighborhood,
  compositionProfiler: Option[CompositionProfiler]
) extends Objective(baseObj.objValue, baseObj.mustBeZero) {

  override lazy val worstValue: Long = baseObj.worstValue

  override def newExploration[M <: Move](
    searchProfilerOpt: Option[NeighborhoodProfiler]
  ): Exploration[M] = {
    new Exploration[M](baseObjExplorer.oldObj, searchProfilerOpt) {
      override def checkNeighbor(buildMove: Long => M): Unit = {
        val newValue = objValue.value()
        val leftMove = buildMove(newValue)
        compositionProfiler.foreach(_.leftExplorationPaused())

        // Instantiates current right.
        val rightNeighborhood: Neighborhood = right(leftMove)
        if (compositionProfiler.nonEmpty) rightNeighborhood.profileSearch()
        compositionProfiler.foreach(_.setCurrentRight(rightNeighborhood.searchProfiler().get))

        // The composite move is the result of asking a move from the right Neighborhood.
        val compositeMove =
          rightNeighborhood.getMove(new CompositeRightStub(baseObj, baseObjExplorer, leftMove))
        compositionProfiler.foreach(_.mergeDynProfiler())
        _toReturn = compositeMove
        compositionProfiler.foreach(_.leftExplorationResumed())
      }
    }
  }

  override def isValueNewBest(currentBest: Long, newValue: Long): Boolean = {
    require(requirement = false, "isValueNewBest can not be called on an Objective stub")
    false
  }
}
