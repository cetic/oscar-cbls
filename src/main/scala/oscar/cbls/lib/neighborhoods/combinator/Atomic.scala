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

/** Companion object of the [[Atomic]] class. */
object Atomic {

  /** Returns an Atomic neighborhood combinator that exhausts the input neighborhood and squashes
    * the performed moves into a single move.
    *
    * @note
    *   The atomic combinator is not designed to be used as a left neighborhood of a [[DynAndThen]]
    *   combinator. However, you can achieve the same combined neighborhood by performing a search
    *   with your right neighborhood after performing a search with your atomic neighborhood.<br> In
    *   addition, if you set the `aggregateIntoSingleMove` to `true`, it is not compatible at all
    *   with the DynAndThen.
    *
    * @param n
    *   The neighborhood to squash into a single move.
    * @param aggregateIntoSingleMove
    *   Whether the moves must be aggregated into a single one or not. If not, the returned
    *   neighborhood is a [[EjectionChains]] that uses only the input neighborhood. By default, set
    *   to `false`.
    * @param shouldStop
    *   Given the number of performed moves, determines whether we should continue searching for new
    *   moves.
    * @param neighborhoodCombinatorName
    *   The name of the neighborhood combinator.
    */
  def apply(
    n: Neighborhood,
    aggregateIntoSingleMove: Boolean = false,
    shouldStop: Int => Boolean = _ => false,
    neighborhoodCombinatorName: String = "Atomic"
  ): Neighborhood = {
    if (aggregateIntoSingleMove) new Atomic(n, shouldStop, neighborhoodCombinatorName)
    else {
      val returnN: (Int, List[Move]) => Option[(Int, Neighborhood)] = (moveCount, _) => {
        if (shouldStop(moveCount)) {
          None
        } else {
          Some(moveCount + 1, n)
        }
      }
      EjectionChains.fold(0)(returnN, name = neighborhoodCombinatorName)
    }
  }
}

/** Neighborhood combinator that exhausts the input neighborhood and squashes the performed moves
  * into a single move.
  *
  * For example, let's consider the route `0 -> 4 -> 3 -> 5 -> 2 -> 1 -> 9 -> 6 -> 8 -> 7`. Using a
  * [[oscar.cbls.lib.neighborhoods.routing.TwoOpt]] neighborhood on this sequence can lead to 4
  * moves:
  *   1. Flip `5 -> 2` giving: `0 -> 4 -> 3 -> 2 -> 5 -> 1 -> 9 -> 6 -> 8 -> 7`
  *   1. Flip `4 -> 3 -> 2` giving: `0 -> 2 -> 3 -> 4 -> 5 -> 1 -> 9 -> 6 -> 8 -> 7`
  *   1. Flip `6 -> 8` giving: `0 -> 2 -> 3 -> 4 -> 5 -> 1 -> 9 -> 8 -> 6 -> 7`
  *   1. Flip `6 -> 7` giving: `0 -> 2 -> 3 -> 4 -> 5 -> 1 -> 9 -> 8 -> 7 -> 6`
  *
  * `Atomic(TwoOpt)` will jump directly to the result the fourth move; given the move "change `0 ->`
  * `4 -> 3 -> 5 -> 2 -> 1 -> 9 -> 6 -> 8 -> 7` into `0 -> 2 -> 3 -> 4 -> 5 -> 1 -> 9 -> 8 -> 7 ->`
  * `6`".
  *
  * This implementation aggregates all the move performed by the input neighborhood into one using
  * [[LoadSolutionMove]].
  *
  * @note
  *   The use of [[LoadSolutionMove]] makes this combinator incompatible with the [[DynAndThen]]
  *
  * @param n
  *   The neighborhood to squash into a single move.
  * @param shouldStop
  *   Given the number of performed moves, determines whether we should continue searching for new
  *   moves.
  * @param neighborhoodCombinatorName
  *   The name of the neighborhood combinator.
  */
class Atomic(n: Neighborhood, shouldStop: Int => Boolean, neighborhoodCombinatorName: String)
    extends NeighborhoodCombinator(neighborhoodCombinatorName, List(n)) {

  override protected[this] def exploreCombinator(objective: Objective): SearchResult = {
    val objValue = objective.objValue

    val startCheckpoint = objValue.model.createCheckpoint()
    val startObjective  = objValue.value()

    n.reset()
    val nbMoves = n.doAllMoves(objective, shouldStop)
    // doAllMoves sets objective at the same verbosity level that n
    setObjectiveVerboseMode(objective, verbosityLevel)

    if (nbMoves == 0)
      NoMoveFound
    else {
      val endObjValue = objValue.pendingValue
      val endSolution = objValue.model.extractSolution()
      rollBackToGlobalCheckpoint(startCheckpoint, objective, startObjective)
      MoveFound(LoadSolutionMove(endSolution, endObjValue, neighborhoodCombinatorName))
    }
  }
}
