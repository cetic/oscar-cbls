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

object ExhaustBack {

  /** Creates an ExhaustBack combinator which returns the result of a
    * [[oscar.cbls.core.search.Neighborhood]] until it returns
    * [[oscar.cbls.core.search.NoMoveFound]]. It switches then to the other Neighborhood.
    *
    * @param first
    *   The first Neighborhood to be exhausted by this combinator. It can be
    *   [[oscar.cbls.core.search.SimpleNeighborhood]] or other NeighborhoodCombinator.
    * @param second
    *   The second Neighborhood to be exhausted by this combinator. It can be
    *   [[oscar.cbls.core.search.SimpleNeighborhood]] or other NeighborhoodCombinator.
    * @param neighborhoodCombinatorName
    *   The name of the neighborhood combinator.s
    */
  def apply(
    first: Neighborhood,
    second: Neighborhood,
    neighborhoodCombinatorName: String = "ExhaustBack"
  ): ExhaustBack = new ExhaustBack(first, second, neighborhoodCombinatorName)
}

/** Combinator which returns the result of a [[oscar.cbls.core.search.Neighborhood]] until it
  * returns [[oscar.cbls.core.search.NoMoveFound]]. It switches then to the other Neighborhood. This
  * combinator is stateful.
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
class ExhaustBack(
  first: Neighborhood,
  second: Neighborhood,
  neighborhoodCombinatorName: String = "ExhaustBack"
) extends NeighborhoodCombinator(neighborhoodCombinatorName, List(first, second)) {

  private[this] var currentIsFirst: Boolean = true
  private[this] var startTime: Long         = 0L

  override protected[this] def exploreCombinator(objective: Objective): SearchResult = {
    if (startTime == 0L) startTime = System.currentTimeMillis()

    // Set to true when a neighborhood doesn't find a move and we must change to the other.
    var lastIsNoMoveFound: Boolean = false
    @tailrec
    def explore(): SearchResult = {
      val current = if (currentIsFirst) first else second

      current.getMove(objective) match {
        case m: MoveFound =>
          lastIsNoMoveFound = false
          m
        case NoMoveFound =>
          // The previous and the current neighborhood doesn't find a move in a row.
          if (lastIsNoMoveFound) NoMoveFound
          // The current neighborhood doesn't find a move. We have to change to the other.
          else {
            lastIsNoMoveFound = true
            if (currentIsFirst) {
              if (searchProfiler().nonEmpty)
                searchProfiler().get
                  .minMeanMaxAddValue("ExhaustTime (ms)", System.currentTimeMillis() - startTime)
              currentIsFirst = false
              second.reset()
            } else {
              currentIsFirst = true
              if (searchProfiler().nonEmpty)
                searchProfiler().get.summedValuePlus("nbBack", 1L)
              first.reset()
            }
            explore()
          }
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
