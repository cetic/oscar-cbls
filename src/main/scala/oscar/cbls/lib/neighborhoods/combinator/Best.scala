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
import oscar.cbls.core.search.profiling.{CombinatorProfiler, SelectionProfiler}
import oscar.cbls.core.search.{
  MoveFound,
  Neighborhood,
  NeighborhoodCombinator,
  NoMoveFound,
  SearchResult
}

import scala.collection.mutable

/** Companion object of the [[Best]] class. */
object Best {

  /** Creates a Best combinator which selects the best move among the input neighborhoods.
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
    val moves: List[(MoveFound, Neighborhood)] = subNeighborhoods.flatMap(n =>
      n.getMove(objective) match {
        case NoMoveFound  => None
        case m: MoveFound => Some((m, n))
      }
    )

    if (moves.isEmpty) NoMoveFound
    else {
      // By default, the profiler assumes that all the found moves are commited. So all found
      // moves update the profiler. However, here, only one move will be commited. So we keep
      // track of the non-selected moves to correct the profiling.
      val nonSelectedNeighborhood: mutable.Queue[Neighborhood] = mutable.Queue()
      // Finds the best move according to the objective.
      val (move: MoveFound, _) = moves.reduceLeft((m1, m2) => {
        if (objective.isValueNewBest(m1._1.objAfter(), m2._1.objAfter())) {
          nonSelectedNeighborhood.enqueue(m1._2)
          m2
        } else {
          nonSelectedNeighborhood.enqueue(m2._2)
          m1
        }
      })
      // Corrects the profiling for unselected moves
      nonSelectedNeighborhood.foreach(_.searchProfiler().foreach(_.explorationNotSelected()))

      move
    }
  }
}
