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

package oscar.cbls.core.computation.objective

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.search._
import oscar.cbls.core.search.profiling.NeighborhoodProfiler

/** An Objective defines the conditions of acceptation of a new objective value during the search.
  * Any optimisation has at least one Objective usually to minimize or maximize a defined value.
  * Most of the time, this value represents the value of a solution (a defined state of the
  * problem).
  *
  * During the exploration, the search procedure tries some modifications of the solution (called
  * [[oscar.cbls.core.search.Move]]) leading to the modification of the objective value. To accept
  * those [[oscar.cbls.core.search.Move]] the Objective has to checks if the new value meets the
  * Objective's conditions.
  */
abstract class Objective(val objValue: IntVariable) {

  /** Returns the worst value that the objective value could have considering the concrete
    * [[Objective]] logic.
    *
    * ==WARNING:==
    * When overriding this value make sure to mark it as "lazy" so that the _currentObjValue
    * initiate with the correct value. If it's not marked as lazy, the default value of
    * [[scala.Long]] will be used (0L).
    */
  val worstValue: Long

  private var _verboseMode: VerboseMode                           = VerboseMode(0)
  private[core] def verboseMode_=(verboseMode: VerboseMode): Unit = _verboseMode = verboseMode
  protected def verboseMode: VerboseMode                          = _verboseMode

  private var bestObj: Long          = worstValue
  private var _currentObjValue: Long = worstValue

  /** Returns the current objective value.
    *
    * Initiated at worstValue, updated with each taken move and force updated when it's value is
    * equal to worstValue. The
    * @return
    *   The current objective value
    */
  protected def currentObjValue(): Long = {
    if (_currentObjValue == worstValue) _currentObjValue = objValue.value()
    _currentObjValue
  }

  // Verbose and profiling methods
  private[core] def startSearch(): Unit = {
    _verboseMode.searchStarted(this, objValue)
  }

  private[core] def explorationStarted(neighborhood: Neighborhood): Unit = {
    neighborhood.searchProfiler().foreach(x => x.explorationStarted(currentObjValue()))
    _verboseMode.explorationStarted(neighborhood.name)
  }

  private[core] def explorationEnded(
    neighborhood: Neighborhood,
    explorationResult: SearchResult
  ): Unit = {
    neighborhood.searchProfiler().foreach(x => x.explorationEnded(explorationResult))
    _verboseMode.explorationEnded(neighborhood, explorationResult)
  }

  /** Checks the state of the objective value and ends the search */
  private[core] def noMoreMove(moveCount: Int): Unit = {
    require(
      objValue.value() == currentObjValue(),
      s"Neighborhood did not restore the model after exploration. Got ${objValue
          .value()} should be ${currentObjValue()}"
    )
    _verboseMode.searchEnded(objValue.value(), moveCount)
  }

  /** Creates a new Exploration instance. Must be called when starting an exploration.
    *
    * @param searchProfilerOpt
    *   An optional NeighborhoodProfiler for profiling purposes only
    * @return
    *   A concrete Exploration object used to explore a neighborhood.
    */
  def newExploration[M <: Move](
    searchProfilerOpt: Option[NeighborhoodProfiler] = None
  ): Exploration[M]

  /** Returns true if newValue is a better value than currentBest.
    *
    * Depending on used Objective this information may vary
    *
    * @param currentBest
    *   The current best value (has to be given by the caller)
    * @param newValue
    *   The considered new value
    * @return
    *   True if newValue is better than currentBest
    */
  def isValueNewBest(currentBest: Long, newValue: Long): Boolean

  /** Checks the state of the objective value and commits the new move */
  private[core] def commitMove(move: Move): Unit = {
    require(
      objValue.value() == currentObjValue(),
      s"Neighborhood did not restore the model after exploration. Got ${objValue
          .value()} should be ${currentObjValue()}"
    )
    val newBestValue = isValueNewBest(bestObj, move.objAfter())
    if (newBestValue) bestObj = move.objAfter()
    move.commit()

    if (objValue.value() == worstValue)
      println(
        s"Warning : objective value == $worstValue. You may have some violated strong constraint"
      )
    require(objValue.value() == move.objAfter(), s"Neighborhood was lying ! : $move got $objValue")
    _verboseMode.moveTaken(move, objValue.value(), currentObjValue(), bestObj, newBestValue)
    _currentObjValue = move.objAfter()
  }
}

/** An Exploration is used by the neighborhood to find and keep the best new objective value during
  * the exploration phase.
  *
  * Depending of the concrete implementation of the Exploration, the behavior and thus the kept
  * objective value may vary.
  */
abstract class Exploration[M <: Move](
  val oldObj: Long,
  searchProfilerOpt: Option[NeighborhoodProfiler]
) {

  /** Keeps the best move found during this exploration. Initialized at NoMoveFound. */
  protected var _toReturn: SearchResult = NoMoveFound

  /** Returns the best move found during this exploration */
  def toReturn: SearchResult = _toReturn

  /** Checks if the candidate solution match the acceptance conditions
    *
    * @param buildMove
    *   A function linking the solution value to the Move that leads to it (must be provided by the
    *   calling Neighborhood)
    */
  def checkNeighborWP(buildMove: Long => M): Unit = {
    checkNeighbor(buildMove)
    searchProfilerOpt.foreach(x => x.neighborExplored())
  }

  protected def checkNeighbor(buildMove: Long => M): Unit
}
