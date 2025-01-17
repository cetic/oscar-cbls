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

package oscar.cbls.lib.neighborhoods.combinator

import oscar.cbls.core.computation.objective.Objective
import oscar.cbls.core.search._
import oscar.cbls.core.search.profiling.SelectionProfiler

/** Companion object of the [[Best]] class. */
object Best {

  /** Creates a Best combinator which selects the best move among the input neighborhoods.
    *
    * '''N.B:''' The semantic of the profiler is different from other combinator. The `found` column
    * contains the number of ''all'' found move from sub-neighborhoods, including not commited ones.
    * In the same way, the `sumGain` column contains the gains obtained if we commit ''all'' the
    * found moves from a sub-neighborhood. So, the `found` and `sumGain` values of Best is ''not
    * equal'' to the sum of the value of sub-neighborhoods. They computed only from committed moves.
    *
    * @param subNeighborhoods
    *   The Neighborhoods that this combinator handles. Those can be
    *   [[oscar.cbls.core.search.SimpleNeighborhood]] or other NeighborhoodCombinator.
    * @param neighborhoodCombinatorName
    *   The name of the neighborhood combinator
    */
  def apply(
    subNeighborhoods: List[Neighborhood],
    neighborhoodCombinatorName: String = "Best"
  ): Best = new Best(subNeighborhoods, neighborhoodCombinatorName)
}

/** Combinator which selects the best move among the input neighborhoods.
  *
  * '''N.B:''' The semantic of the profiler is different from other combinator. The `found` column
  * contains the number of ''all'' found move from sub-neighborhoods, including not commited ones.
  * In the same way, the `sumGain` column contains the gains obtained if we commit ''all'' the found
  * moves from a sub-neighborhood. So, the `found` and `sumGain` values of Best is ''not equal'' to
  * the sum of the value of sub-neighborhoods. They computed only from committed moves.
  *
  * @param subNeighborhoods
  *   The Neighborhoods that this combinator handles. Those can be
  *   [[oscar.cbls.core.search.SimpleNeighborhood]] or other NeighborhoodCombinator.
  * @param neighborhoodCombinatorName
  *   The name of the neighborhood combinator
  */
class Best(subNeighborhoods: List[Neighborhood], neighborhoodCombinatorName: String = "Best")
    extends NeighborhoodCombinator(neighborhoodCombinatorName, subNeighborhoods) {

  override def profileSearch(): Unit = {
    _searchProfilerOpt match {
      case None =>
        subNeighborhoods.foreach(_.profileSearch())
        _searchProfilerOpt = Some(new SelectionProfiler(this, subNeighborhoods))
      case _ => ;
    }
  }

  override protected[this] def exploreCombinator(objective: Objective): SearchResult = {
    // Gets the potential found moves from each neighborhood
    val moves: List[MoveFound] = subNeighborhoods.flatMap(n =>
      n.getMove(objective) match {
        case NoMoveFound  => None
        case m: MoveFound => Some(m)
      }
    )

    if (moves.isEmpty) NoMoveFound
    else {
      // Finds the best move according to the objective.
      val move: MoveFound = moves.reduceLeft((m1, m2) => {
        if (objective.isValueNewBest(m1.objAfter(), m2.objAfter())) m2
        else m1
      })

      move
    }
  }
}
