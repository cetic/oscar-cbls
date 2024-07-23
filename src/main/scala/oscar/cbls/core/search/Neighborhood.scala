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
import oscar.cbls.core.search.profiling.SearchProfiler
import oscar.cbls.visual.profiling.ProfilingConsole

/** An interface that provides methods to handle search procedure.
  *
  * It has mainly two extensions :
  *   - [[NeighborhoodCombinator]] => Combines Neighborhoods
  *   - [[SimpleNeighborhood]] ==> A "leaf" neighborhood that does modification on variables.
  *
  * A search procedure is a Neighborhood or a combination of Neighborhood.
  *
  * For instance :
  *   - AssignNeighborhood => Upon calling [[Neighborhood.doAllMoves]], this search procedure will
  *     assign new values to a variable. Until no value are left to test.
  *
  *   - AssignNeighborhood(A: IntVariable) dynAndThen AssignNeighborhood(B: IntVariable) ==> This
  *     one will assign a value to A and then to B and evaluate the combined movement.
  *
  * @param _name
  *   The name of the Neighborhood (logging and profiling purposes)
  */
abstract class Neighborhood(_name: String) {

  // VerboseMode stuff. See VerboseMode class for more information.
  private[core] var _verboseMode: VerboseMode = VerboseMode(0)
  private var _verbosityLevel: Int            = 0
  def verbosityLevel: Int                     = _verbosityLevel

  /** Sets the new verbosity level and [[VerboseMode]] */
  def verbosityLevel_=(verbosityLevel: Int): Unit = {
    _verbosityLevel = verbosityLevel
    _verboseMode = VerboseMode(verbosityLevel)
  }

  // Profiling stuff. Sea SearchProfiler class for more information.
  def searchProfiler(): Option[SearchProfiler]

  /** Activates search profiling through all search procedure.
    * ==WARNING :==
    * Be sure to call it at the root of your final search procedure to have complete search
    * profiling
    */
  def profileSearch(): Unit

  /** Displays the result of the search profiling in the console */
  def displayProfiling(): Unit =
    searchProfiler().foreach(_searchProfiler =>
      ProfilingConsole(_searchProfiler, _searchProfiler.collectThisProfileHeader)
    )

  /** Resets the internal state of the neighborhood */
  def reset(): Unit

  /** Tries to find a new move following the Objective restriction.
    *
    * @param objective
    *   The Objective of the search (minimizing, maximizing...)
    * @return
    *   [[MoveFound]] if a move has been found [[NoMoveFound]] otherwise.
    */
  def getMove(objective: Objective): SearchResult

  /** Does at most one improving move.
    *
    * @param objective
    *   The Objective of the search (minimizing, maximizing...)
    * @return
    *   True if one move has been performed, false otherwise
    */
  def doImprovingMove(objective: Objective): Boolean =
    0L != doAllMoves(objective, _ >= 1L)

  /** Does as much moves as possible or until the shouldStop condition is met.
    *
    * @param objective
    *   The Objective of the search (minimizing, maximizing...)
    * @param shouldStop
    *   Given the number of performed moves, determines whether or not we should continue searching
    *   for new moves.
    * @return
    *   The total number of performed moves
    */
  def doAllMoves(objective: Objective, shouldStop: Int => Boolean = _ => false): Int = {
    var moveCount: Int      = 0
    var noMoreMove: Boolean = false
    objective.verboseMode_=(this._verboseMode)
    objective.startSearch()

    while (!shouldStop(moveCount) && !noMoreMove) {
      val getMoveResult: SearchResult = getMove(objective)
      getMoveResult match {
        case NoMoveFound =>
          noMoreMove = true
          objective.noMoreMove(moveCount)
        case mf: MoveFound =>
          moveCount += 1
          objective.commitMove(mf.move)
          profileCommittedMove(mf.move)
      }
    }
    moveCount
  }

  /** Returns the name of the Neighborhood */
  def name: String = _name

  override def toString: String = name

  protected def profileCommittedMove(move: Move): Unit =
    move.neighborhood.searchProfiler().foreach(_.moveCommitted(move.objAfter()))
}
