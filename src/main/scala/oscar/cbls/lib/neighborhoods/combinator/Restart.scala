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

import oscar.cbls.core.computation.Solution
import oscar.cbls.core.computation.objective.{AcceptAll, Objective}
import oscar.cbls.core.search._
import oscar.cbls.core.search.profiling.CombinatorProfiler

/** Companion object of the [[Restart]] class. */
object Restart {

  /** Creates a metaheuristic that performs a restart of the search for given number of time.
    *
    * This combinator exhaust first neighborhood `n` and the save the best solution encountered.
    * Then it query a move from `neighborhoodForRestart` that modify the current solution (or the
    * best one if needed). Finally, `n` restart a search from the modified solution.
    *
    * If no more restart is permitted, the best solution is reloaded.
    *
    * @param n
    *   The neighborhood that performs the search.
    * @param neighborhoodForRestart
    *   The neighborhood used to modify the current solution. The move returned by this neighborhood
    *   is always commited.
    * @param maxConsecutiveRestartWithoutImprovement
    *   Maximum times in a row a cycle (restart + search) can end up without improvement of the
    *   objective function. When this value is reached, the best solution found is restored.
    * @param maxNumberOfRestartInTotal
    *   How many restart (improving or not) can be performed in total.
    * @param restartFromBest
    *   Whether the best solution has to be reloaded before a restart.
    * @param name
    *   The name of the combinator.
    */
  def apply(
    n: Neighborhood,
    neighborhoodForRestart: Neighborhood,
    maxConsecutiveRestartWithoutImprovement: Int,
    maxNumberOfRestartInTotal: Int,
    restartFromBest: Boolean = false,
    name: String = "Restart"
  ): Restart =
    new Restart(
      n,
      neighborhoodForRestart,
      maxConsecutiveRestartWithoutImprovement,
      maxNumberOfRestartInTotal: Int,
      restartFromBest,
      name
    )

}

/** Metaheuristic that performs a restart of the search for given number of time.
  *
  * This combinator exhaust first neighborhood `n` and the save the best solution encountered. Then
  * it query a move from `neighborhoodForRestart` that modify the current solution (or the best one
  * if needed). Finally, `n` restart a search from the modified solution.
  *
  * If no more restart is permitted, the best solution is reloaded.
  *
  * @note
  *   The solution proposed by the restart neighborhood always respect strong constraints. The
  *   `acceptanceCriterionForRestart` input can be used to specify how to test if constraint are
  *   respected.
  *
  * @param n
  *   The neighborhood that performs the search.
  * @param neighborhoodForRestart
  *   The neighborhood used to modify the current solution. The move returned by this neighborhood
  *   is always commited.
  * @param maxConsecutiveRestartWithoutImprovement
  *   Maximum times in a row a cycle (restart + search) can end up without improvement of the
  *   objective function. When this value is reached, the best solution found is restored.
  * @param maxNumberOfRestartInTotal
  *   How many restart (improving or not) can be performed in total.
  * @param restartFromBest
  *   Whether the best solution has to be reloaded before a restart.
  * @param name
  *   The name of the combinator.
  */
class Restart(
  n: Neighborhood,
  neighborhoodForRestart: Neighborhood,
  maxConsecutiveRestartWithoutImprovement: Int,
  maxNumberOfRestartInTotal: Int,
  restartFromBest: Boolean,
  name: String
) extends NeighborhoodCombinator(name, List(n, neighborhoodForRestart)) {

  // The best solution overall the moves
  private var bestObj: Long                  = _
  private var bestSolution: Option[Solution] = None

  private var numConsecutiveRestartWithoutImprovement: Int = 0
  private var bestSolutionThisRestart: Boolean             = false
  private var numRestartInTotal: Int                       = 0

  private var _compositionProfilerOpt: Option[CombinatorProfiler] = None

  override def searchProfiler(): Option[CombinatorProfiler] = _compositionProfilerOpt

  override def profileSearch(): Unit = {
    searchProfiler() match {
      case None =>
        _compositionProfilerOpt = Some(new CombinatorProfiler(this))
        n.profileSearch()
        neighborhoodForRestart.profileSearch()
      case _ => ;
    }
  }

  override def verbosityLevel_=(verbosityLevel: Int): Unit = {
    super.verbosityLevel_=(verbosityLevel)
    n.verbosityLevel = verbosityLevel
    neighborhoodForRestart.verbosityLevel = verbosityLevel
  }

  override protected[this] def exploreCombinator(objective: Objective): SearchResult = {
    val model = objective.objValue.model
    n.getMove(objective) match {
      case mf: MoveFound =>
        if (bestSolution.isEmpty || objective.isValueNewBest(bestObj, mf.objAfter())) {
          // Extracts the solution before the move
          val currentSolution = Some(model.extractSolution())
          val currentObj      = objective.objValue.value()

          // Gets the solution after the commited move
          commitMove(objective, mf.move)

          bestObj = mf.objAfter()
          bestSolution = Some(model.extractSolution())
          bestSolutionThisRestart = true

          // Rollbacks the solution
          loadSolution(objective, currentSolution, currentObj)

        }
        searchProfiler().foreach(_.positiveEventPercentagePushEvent("Restart", isPositive = false))
        mf
      case NoMoveFound =>
        numConsecutiveRestartWithoutImprovement =
          if (bestSolutionThisRestart) 0 else numConsecutiveRestartWithoutImprovement + 1

        n.reset()
        if (
          numConsecutiveRestartWithoutImprovement < maxConsecutiveRestartWithoutImprovement
          && numRestartInTotal < maxNumberOfRestartInTotal
        ) {
          searchProfiler().foreach(_.positiveEventPercentagePushEvent("Restart", isPositive = true))

          bestSolutionThisRestart = false
          numRestartInTotal += 1
          if (restartFromBest) loadSolution(objective, bestSolution, bestObj)

          // The move returned by the restart must always be accepted
          val acceptAll =
            AcceptAll(objective.objValue, objective.mustBeZero)
          neighborhoodForRestart.getMove(acceptAll)
        } else {
          // No more restart can be performed. The best solution is reloaded
          loadSolution(objective, bestSolution, bestObj)
          NoMoveFound
        }
    }
  }

  private def loadSolution(objective: Objective, s: Option[Solution], objAtSolution: Long): Unit =
    s match {
      case Some(s) =>
        setObjectiveVerboseMode(
          objective,
          0
        ) // The loading do not have to be displayed in the profiling
        commitMove(objective, LoadSolutionMove(s, objAtSolution, "Load best solution found."))
        setObjectiveVerboseMode(objective, verbosityLevel)
      case None => ;
    }
}
