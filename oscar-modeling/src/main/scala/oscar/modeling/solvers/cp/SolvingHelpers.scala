/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

package oscar.modeling.solvers.cp

import oscar.algo.search.Branching
import oscar.modeling.misc.{SPSearchStatistics, SearchStatistics}
import oscar.modeling.models.{ModelDeclaration, ModelDeclarationProxy}
import Branchings._

import scala.collection.mutable.ListBuffer
//import scala.spores.NullarySpore

/**
  * A trait for object that would want to watch solving
  * @tparam RetVal
  */
trait Watcher[RetVal] {
  /**
    * Called when a subproblem just started
    *
    * @param spid id of the subproblem
    */
  def startedSubproblem(spid: Int): Unit

  /**
    * Called when a subproblem ends
    *
    * @param spid id of the subproblem
    * @param timeTaken time taken, in nanosecs
    * @param searchStats search stats of the subproblem
    */
  def endedSubproblem(spid: Int, timeTaken: Double, searchStats: SPSearchStatistics): Unit

  /**
    * Called when a new solution is found (during the search)
    *
    * @param solution new solution
    * @param newBound possible new bound
    */
  def newSolution(solution: RetVal, newBound: Option[Int]): Unit

  /**
    * Called when a solver give a recap of the found solution (after the search, no duplicates with newSolution)
    *
    * @param nbSolutions the number of solutions
    * @param solutions only filled if RetVal is not Unit
    */
  def solutionRecap(nbSolutions: Int, solutions: List[RetVal]): Unit

  /**
    * Called when solving is completely done and all solutions have been sent. No more function calls will be made
    * @param completed: True if the timeout was not reached
    */
  def allDone(completed: Boolean): Unit
}

/**
  * A watcher multiplexer
  * @param watchers
  * @tparam RetVal
  */
class WatcherMultiplexer[RetVal](watchers: Iterable[Watcher[RetVal]]) extends Watcher[RetVal]{
  /**
    * Called when a subproblem just started
    *
    * @param spid id of the subproblem
    */
  override def startedSubproblem(spid: Int): Unit = watchers.foreach(_.startedSubproblem(spid))

  /**
    * Called when a subproblem ends
    *
    * @param spid        id of the subproblem
    * @param timeTaken   time taken, in nanosecs
    * @param searchStats search stats of the subproblem
    */
  override def endedSubproblem(spid: Int, timeTaken: Double, searchStats: SPSearchStatistics): Unit = watchers.foreach(_.endedSubproblem(spid, timeTaken, searchStats))

  /**
    * Called when a new solution is found (during the search)
    *
    * @param solution new solution
    * @param newBound possible new bound
    */
  override def newSolution(solution: RetVal, newBound: Option[Int]): Unit = watchers.foreach(_.newSolution(solution, newBound))

  /**
    * Called when a solver give a recap of the found solution (after the search, no duplicates with newSolution)
    *
    * @param nbSolutions the number of solutions
    * @param solutions   only filled if RetVal is not Unit
    */
  override def solutionRecap(nbSolutions: Int, solutions: List[RetVal]): Unit = watchers.foreach(_.solutionRecap(nbSolutions, solutions))

  /**
    * Called when solving is completely done and all solutions have been sent. No more function calls will be made
    *
    * @param completed : True if the timeout was not reached
    */
  override def allDone(completed: Boolean): Unit = watchers.foreach(_.allDone(completed))
}

/**
  * Statistics generation
  * @tparam RetVal
  */
class StatisticsWatcher[RetVal] extends Watcher[RetVal] {
  var currentStatistics = new SearchStatistics(0, 0, 0, false, 0, 0, 0, 0)
  val results = ListBuffer[RetVal]()
  var startTime: Long = 0

  def get = (currentStatistics, results.toList)

  def start(): Unit = startTime = System.currentTimeMillis()

  override def startedSubproblem(spid: Int): Unit = {}

  override def newSolution(solution: RetVal, newBound: Option[Int]): Unit = {
    currentStatistics = new SearchStatistics(nNodes = currentStatistics.nNodes,
      nFails = currentStatistics.nFails,
      time = currentStatistics.time,
      completed = false,
      timeInTrail = currentStatistics.timeInTrail,
      maxTrailSize = currentStatistics.maxTrailSize,
      nSols = currentStatistics.nSols+1,
      timeToLastSolution = System.currentTimeMillis()-startTime
    )
    results += solution
  }

  override def solutionRecap(nbSolutions: Int, solutions: List[RetVal]): Unit = {
    currentStatistics = new SearchStatistics(nNodes = currentStatistics.nNodes,
      nFails = currentStatistics.nFails,
      time = currentStatistics.time,
      completed = currentStatistics.completed,
      timeInTrail = currentStatistics.timeInTrail,
      maxTrailSize = currentStatistics.maxTrailSize,
      nSols = currentStatistics.nSols+nbSolutions,
      timeToLastSolution = currentStatistics.timeToLastSolution
    )
    results ++= solutions
  }

  override def allDone(completed: Boolean): Unit = {
    currentStatistics = new SearchStatistics(nNodes = currentStatistics.nNodes,
      nFails = currentStatistics.nFails,
      time = currentStatistics.time,
      completed = completed,
      timeInTrail = currentStatistics.timeInTrail,
      maxTrailSize = currentStatistics.maxTrailSize,
      nSols = currentStatistics.nSols,
      timeToLastSolution = currentStatistics.timeToLastSolution
    )
  }

  override def endedSubproblem(spid: Int, timeTaken: Double, searchStats: SPSearchStatistics): Unit = {
    currentStatistics = SearchStatistics(nNodes = currentStatistics.nNodes+searchStats.nNodes,
      nFails = currentStatistics.nFails+searchStats.nFails,
      time = currentStatistics.time + searchStats.time,
      completed = false,
      timeInTrail = currentStatistics.timeInTrail+searchStats.timeInTrail,
      maxTrailSize = Math.max(currentStatistics.maxTrailSize, searchStats.maxTrailSize),
      nSols = currentStatistics.nSols,
      timeToLastSolution = currentStatistics.timeToLastSolution
    )
  }
}

/**
  * Proxy most functions to an underlying model
  *
  * @param modelDeclaration the model to proxy to
  */
class CPModelProxy[CPModelType <: CPSolve[Retval], Retval](modelDeclaration: ModelDeclaration with CPModelType) extends ModelDeclarationProxy {
  implicit val md: ModelDeclaration = modelDeclaration

  def getSearch: BranchingInstantiator = modelDeclaration.getSearch
  def setSearch(b: Branching): Unit = modelDeclaration.setSearch(b)
  def setSearch(b: => Seq[Alternative]): Unit = modelDeclaration.setSearch(b)
  def setSearch(b: BranchingInstantiator): Unit = modelDeclaration.setSearch(b)

  def onSolution = modelDeclaration.onSolution
  def onSolution(s: => Retval): Unit = modelDeclaration.onSolution(s)
  //def onSolution(s: NullarySpore[Retval]): Unit = modelDeclaration.onSolution(s())
}