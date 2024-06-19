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

package oscar.cbls.core.search

import oscar.cbls.core.computation.objective.Objective
import oscar.cbls.core.search.profiling.{CombinatorProfiler, SearchProfiler}

/** An interface that provides methods to create a NeighborhoodCombinator.
  *
  * @param neighborhoodCombinatorName
  *   The name of the neighborhood combinator
  * @param subNeighborhoods
  *   The Neighborhood that this combinator handles. Those can be [[SimpleNeighborhood]] or other
  *   NeighborhoodCombinator.
  */
abstract class NeighborhoodCombinator(
  neighborhoodCombinatorName: String,
  val subNeighborhoods: List[Neighborhood]
) extends Neighborhood(neighborhoodCombinatorName) {

  protected var _searchProfilerOpt: Option[CombinatorProfiler] = None

  override def searchProfiler(): Option[CombinatorProfiler] = _searchProfilerOpt

  override def profileSearch(): Unit = _searchProfilerOpt match {
    case None =>
      _searchProfilerOpt = Some(new CombinatorProfiler(this))
      subNeighborhoods.foreach(_.profileSearch())
    case _ => ;
  }

  override final def getMove(objective: Objective): SearchResult = {
    objective.explorationStarted(this)
    val explorationResult = exploreCombinator(objective)
    objective.explorationEnded(this, explorationResult)
    explorationResult
  }

  /** Explores this Combinator. This is where you put the logic of your Combinator.
    *
    * @param objective
    *   The Objective of the search
    * @return
    *   The search result, either [[MoveFound]] or [[NoMoveFound]]
    */
  protected[this] def exploreCombinator(objective: Objective): SearchResult

  override def reset(): Unit = {
    for (n <- subNeighborhoods) n.reset()
  }

  override def toString: String = this.getClass.getSimpleName

}
