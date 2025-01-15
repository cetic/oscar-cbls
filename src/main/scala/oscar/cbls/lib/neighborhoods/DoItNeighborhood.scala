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

package oscar.cbls.lib.neighborhoods

import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.objective.{AcceptAll, Exploration, Objective}
import oscar.cbls.core.search.{SearchResult, SimpleNeighborhood}
import oscar.cbls.lib.neighborhoods.combinator.LoadSolutionMove

/** Companion object of the [[DoItNeighborhood]] class. */
object DoItNeighborhood {

  /** Returns a DoItNeighborhood that ''always'' returns move that performs a given unit function
    * when it is commited.
    *
    * @param model
    *   Model attached to the search.
    * @param doIt
    *   The method to performs when commiting a move.
    * @param neighborhoodName
    *   The name of the Neighborhood
    */
  def apply(model: Store, doIt: () => Unit, neighborhoodName: String = "DoIt"): DoItNeighborhood =
    new DoItNeighborhood(model, doIt, neighborhoodName)
}

/** Neighborhoods that ''always'' returns move that performs a given unit function when it is
  * commited.
  *
  * @param model
  *   Model attached to the search.
  * @param doIt
  *   The method to performs when commiting a move.
  * @param neighborhoodName
  *   The name of the Neighborhood
  */
class DoItNeighborhood(model: Store, doIt: () => Unit, neighborhoodName: String)
    extends SimpleNeighborhood[LoadSolutionMove](neighborhoodName) {

  override def getMove(objective: Objective): SearchResult = {
    val accept = AcceptAll(objective.objValue) // A move must always be performed
    super.getMove(accept)
  }

  override protected def exploreNeighborhood(exploration: Exploration[LoadSolutionMove]): Unit = {
    val solution = model.extractSolution()
    doIt()
    val newSolution = model.extractSolution()
    exploration.checkNeighborWP(objValue =>
      LoadSolutionMove(newSolution, objValue, neighborhoodName)
    )
    solution.restoreSolution()
  }

  override def doMove(move: LoadSolutionMove): Unit = move.commit()

  override def reset(): Unit = {}
}
