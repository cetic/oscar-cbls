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

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.Objective
import oscar.cbls.core.search.profiling.SearchProfiler
import oscar.cbls.visual.profiling.ProfilingConsole

abstract class Neighborhood(_name: String) {

  // Verbose
  protected var _verboseMode: VerboseMode         = VerboseMode(0)
  private var _verbosityLevel: Int = 0

  // Profiling
  private[search] val _searchProfiler: SearchProfiler = new SearchProfiler(this)
  def displayProfiling(): Unit =
    ProfilingConsole(_searchProfiler, _searchProfiler.collectThisProfileHeader)

  /** Resets the internal state of the neighborhood */
  def reset(): Unit

  /** Tries to find a new move following the Objective restriction.
    *
    * @param objective
    *   The Objective of the search (minimizing, maximizing...)
    * @param objValue
    *   The value that must be minimized, maximized...
    * @return
    *   [[MoveFound]] if a move has been found [[NoMoveFound]] otherwise.
    */
  def getMove(objective: Objective, objValue: IntVariable): SearchResult

  /** Does at most one improving move.
    *
    * @param objective
    *   The Objective of the search (minimizing, maximizing...)
    * @param objValue
    *   The value that must be minimized, maximized...
    * @return
    *   True if one move has been performed, false otherwise
    */
  def doImprovingMove(objective: Objective, objValue: IntVariable): Boolean =
    0L != doAllMoves(objective, objValue, _ >= 1L)

  /** Does as much moves as possible or until the shouldStop condition is met.
    *
    * @param objective
    *   The Objective of the search (minimizing, maximizing...)
    * @param objValue
    *   The value that must be minimized, maximized...
    * @param shouldStop
    *   Given the number of performed moves, determines whether or not we should continue searching
    *   for new moves.
    * @return
    *   The total number of performed moves
    */
  def doAllMoves(
    objective: Objective,
    objValue: IntVariable,
    shouldStop: Int => Boolean = _ => false
  ): Int = {
    var bestObj: Long       = objective.worstValue
    var moveCount: Int      = 0
    var noMoreMove: Boolean = false
    objective.verboseMode = _verboseMode

    _verboseMode.searchStarted(objective,objValue)
    while (!shouldStop(moveCount) && !noMoreMove) {
      val latestObjValue              = objValue.value()
      val getMoveResult: SearchResult = getMove(objective, objValue)
      require(
        objValue.value == latestObjValue,
        "Neighborhood did not restore the model after exploration"
      )
      getMoveResult match {
        case NoMoveFound => noMoreMove = true
        case mf: MoveFound =>
          moveCount += 1
          val newBestValue = objective.isValueNewBest(bestObj, mf.objAfter())
          if (newBestValue) bestObj = mf.objAfter()
          mf.commit()

          if (objValue.value() == Long.MaxValue)
            println(
              "Warning : objective value == MaxLong. You may have some violated strong constraint"
            )
          require(
            objValue.value() == mf.objAfter,
            s"Neighborhood was lying ! : " + mf + " got " + objValue
          )
          _verboseMode.moveTaken(
            mf.move,
            objValue.value(),
            latestObjValue,
            bestObj,
            newBestValue
          )
      }
    }
    _verboseMode.searchEnded(objValue.value(), moveCount)
    moveCount
  }

  def verbosityLevel: Int = _verbosityLevel

  /** Sets the new SearchDisplay */
  def verbosityLevel_=(verbosityLevel: Int): Unit = {
    _verbosityLevel = verbosityLevel
    _verboseMode = VerboseMode(verbosityLevel)
  }

  def name: String = _name
}
