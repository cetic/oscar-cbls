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

abstract class Neighborhood(_name: String) {

  protected var _searchDisplay: SearchDisplay = SearchDisplay(0)
  private[search] val _searchProfiler: SearchProfiler = new SearchProfiler(this)

  def reset(): Unit

  def resetStatistics(): Unit

  def getMove(objective: Objective): SearchResult

  def doAllMoves(
    objective: Objective,
    objValue: IntVariable,
    shouldStop: Int => Boolean = _ => false
  ): Int = {
    var bestObj: Long  = objective.worstValue
    var prevObj: Long  = objective.worstValue
    var moveCount: Int = 0

    _searchDisplay.searchStarted(objective.toString)
    while (!shouldStop(moveCount)) {
      val latestObjValue = objValue.value()
      getMove(objective) match {
        case NoMoveFound =>
          require(
            objValue.value == latestObjValue,
            "Neighborhood did not restore the model after exploration"
          )
          _searchDisplay.searchEnded(objValue.value(), moveCount)
          return moveCount
        case mf: MoveFound =>
          require(
            objValue.value == latestObjValue,
            "Neighborhood did not restore the model after exploration"
          )
          prevObj = mf.objAfter()
          val newBestValue =
            if (objective.isValueNewBest(bestObj, mf.objAfter())) {
              bestObj = mf.objAfter()
              true
            } else false
          mf.commit()

          if (objValue.value() == Long.MaxValue)
            println(
              "Warning : objective value == MaxLong. You may have some violated strong constraint"
            )
          require(
            mf.objAfter() == Long.MaxValue || objValue.value() == mf.objAfter,
            s"Neighborhood was lying ! : " + mf + " got " + objValue
          )
          _searchDisplay.moveTaken(
            mf.move.neighborhoodName,
            mf.move,
            objValue.value(),
            prevObj,
            bestObj,
            newBestValue
          )
      }
      moveCount += 1
    }
    _searchDisplay.searchEnded(objValue.value(), moveCount)
    moveCount
  }

  def searchDisplay: SearchDisplay = _searchDisplay

  /** Sets the new SearchDisplay */
  def searchDisplay_=(searchDisplay: SearchDisplay): Unit = _searchDisplay = searchDisplay

  def name: String = _name
}
