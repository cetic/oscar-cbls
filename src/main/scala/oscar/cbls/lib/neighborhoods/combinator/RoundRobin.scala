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

/** companion object of the [[RoundRobin]] class. */
object RoundRobin {

  /** Creates a RoundRobin combinator which performs a round robin on a list of
    * [[oscar.cbls.core.search.Neighborhood]]. The combinator changes of explored neighborhood after
    * the current has been invoked `k` times (where `k` is given in input) or if it is exhausted.
    *
    * @param robins
    *   An array of tuples `(n, k)` where `n` is a Neighborhood to be explored and `k` is the number
    *   of times `n` can be explored in a row.
    * @param tabu
    *   The number of times, a neighborhood cannot be used in a row.
    * @param neighborhoodCombinatorName
    *   The name of the neighborhood combinator.
    */
  def apply(
    robins: Array[(Neighborhood, Int)],
    tabu: Int = 1,
    neighborhoodCombinatorName: String = "RoundRobin"
  ): RoundRobin = new RoundRobin(robins, tabu, neighborhoodCombinatorName)
}

/** Combinator which performs a round robin on a list of [[oscar.cbls.core.search.Neighborhood]].
  * The combinator changes of explored neighborhood after the current has been invoked `k` times
  * (where `k` is given in input) or if it is exhausted.
  *
  * @param robins
  *   An array of tuples `(n, k)` where `n` is a Neighborhood to be explored and `k` is the number
  *   of times `n` can be explored in a row.
  * @param tabu
  *   The number of times, a neighborhood cannot be used in a row.
  * @param neighborhoodCombinatorName
  *   The name of the neighborhood combinator.
  */
class RoundRobin(
  robins: Array[(Neighborhood, Int)],
  tabu: Int = 1,
  neighborhoodCombinatorName: String = "RoundRobin"
) extends NeighborhoodCombinator(neighborhoodCombinatorName, robins.map(_._1).toList) {

  private[this] var currentRobin: Int                = 0
  private[this] var nbExplorationOnCurrentRobin: Int = 0
  private[this] var firstFailedRobinInRow: Int       = -1
  private[this] var currentCycle: Int                = 0
  private[this] val cycleOfLastFail: Array[Int]      = Array.fill(robins.length)(Int.MinValue)

  override protected[this] def exploreCombinator(objective: Objective): SearchResult = {
    // While we have not cycle around whole set of failed robins
    while (currentRobin != firstFailedRobinInRow) {
      robins(currentCycle)._1.getMove(objective) match {
        case NoMoveFound =>
          if (firstFailedRobinInRow == -1) firstFailedRobinInRow = currentRobin
          nbExplorationOnCurrentRobin = robins(currentRobin)._2
          cycleOfLastFail(currentRobin) = currentCycle
          selectNextRobin()
        case m: MoveFound =>
          firstFailedRobinInRow = -1
          nbExplorationOnCurrentRobin += 1
          return m
      }
    }
    NoMoveFound
  }

  override def reset(): Unit = {
    currentRobin = 0
    nbExplorationOnCurrentRobin = 0
    firstFailedRobinInRow = -1
    currentCycle = 0
    for (i <- cycleOfLastFail.indices) cycleOfLastFail(i) = Int.MinValue
    super.reset()
  }

  // Method used to select the next robin
  @inline
  final private[this] def selectNextRobin(): Unit = {
    val prevRobin: Int                 = currentRobin
    var nextNeighborhoodFound: Boolean = false

    if (nbExplorationOnCurrentRobin >= robins(currentRobin)._2) {
      nextRobin()
    }
    while (!nextNeighborhoodFound) {
      // If the tabu memory allow us to use the current robin
      if (cycleOfLastFail(currentRobin) + tabu < currentCycle)
        nextNeighborhoodFound = true
      else {
        nextRobin()
        // We have cycled over all the robins. We can stop the selection.
        if (prevRobin == currentRobin) nextNeighborhoodFound = true
      }
    }
  }

  // Moves to the next robin
  @inline
  final private[this] def nextRobin(): Unit = {
    currentRobin += 1
    if (currentRobin == robins.length) {
      currentRobin = 0
      currentCycle += 1
    }
    nbExplorationOnCurrentRobin = 0
  }

}
