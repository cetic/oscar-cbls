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
import oscar.cbls.core.search.{
  MoveFound,
  Neighborhood,
  NeighborhoodCombinator,
  NoMoveFound,
  SearchResult
}

import scala.annotation.tailrec

/** Companion object of the [[Exhaust]] class. */
object Exhaust {

  /** Creates an Exhaust neighborhood combinator which returns the result of the `first`
    * [[oscar.cbls.core.search.Neighborhood]] until it returns
    * [[oscar.cbls.core.search.NoMoveFound]]. It then switches to the `second` Neighborhood. It does
    * not come back to the first one after the second one is exhausted.
    *
    * @param first
    *   The first Neighborhood to be exhausted by this combinator. It can be
    *   [[oscar.cbls.core.search.SimpleNeighborhood]] or other NeighborhoodCombinator.
    * @param second
    *   The second Neighborhood to be exhausted by this combinator. It can be
    *   [[oscar.cbls.core.search.SimpleNeighborhood]] or other NeighborhoodCombinator.
    * @param neighborhoodCombinatorName
    *   The name of the neighborhood combinator.
    */
  def apply(
    first: Neighborhood,
    second: Neighborhood,
    neighborhoodCombinatorName: String
  ): Exhaust = new Exhaust(first, second, neighborhoodCombinatorName)
}

/** Combinator which returns the result of the `first` [[oscar.cbls.core.search.Neighborhood]] until
  * it returns [[oscar.cbls.core.search.NoMoveFound]]. It then switches to the `second`
  * Neighborhood. It does not come back to the first one after the second one is exhausted.
  *
  * @param first
  *   The first Neighborhood to be exhausted by this combinator. It can be
  *   [[oscar.cbls.core.search.SimpleNeighborhood]] or other NeighborhoodCombinator.
  * @param second
  *   The second Neighborhood to be exhausted by this combinator. It can be
  *   [[oscar.cbls.core.search.SimpleNeighborhood]] or other NeighborhoodCombinator.
  * @param neighborhoodCombinatorName
  *   The name of the neighborhood combinator.
  */
class Exhaust(
  first: Neighborhood,
  second: Neighborhood,
  neighborhoodCombinatorName: String = "Exhaust"
) extends NeighborhoodCombinator(neighborhoodCombinatorName, List(first, second)) {

  private[this] var currentIsFirst: Boolean = true
  private[this] var startTime: Long         = 0L

  override protected[this] def exploreCombinator(objective: Objective): SearchResult = {
    if (startTime == 0L) startTime = System.currentTimeMillis()

    @tailrec
    def explore(): SearchResult = {
      val current = if (currentIsFirst) first else second
      current.getMove(objective) match {
        case NoMoveFound =>
          if (currentIsFirst) {
            currentIsFirst = false
            if (searchProfiler().nonEmpty)
              searchProfiler().get
                .minMeanMaxAddValue("ExhaustTime (ms)", System.currentTimeMillis() - startTime)
            explore()
          } else NoMoveFound
        case m: MoveFound => m
      }
    }
    explore()
  }

  override def reset(): Unit = {
    currentIsFirst = true
    startTime = 0L
    super.reset()
  }
}
