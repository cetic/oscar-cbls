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
import oscar.cbls.core.computation.objective.composite.CompositeLeftStub
import oscar.cbls.core.search.profiling.CompositionProfiler
import oscar.cbls.core.search.{Move, Neighborhood, NeighborhoodCombinator, SearchResult}

/** Companion object of DynAndThen */
object DynAndThen {

  /** Generates a specialized DynAndThen where the right move does not depend on the left one.
    *
    * @param left
    *   The first neighborhood to be explored.
    * @param right
    *   The second neighborhood to be explored.
    * @return
    *   A DynAndThen combinator.
    */
  def apply(left: Neighborhood, right: Neighborhood): DynAndThen = {
    new DynAndThen(left, _ => right)
  }

  /** This Combinator allows to build [[CompositeMove]]s.
    *
    * A [[CompositeMove]] is a move composed of two sub-[[oscar.cbls.core.search.Move]]s. The key point is that the two
    * moves are evaluated as one, meaning that only the composite move is required to improve the
    * objective function, not the sub-moves. The main mechanism is implemented in the
    * [[oscar.cbls.core.computation.objective.composite]] package.
    *
    * Usage:
    *   - Explore complex sub-moves, where the left one worsens the solution and the second one
    *     improves it.
    *   - Pickup and Delivery example: we have to insert/move/remove two nodes (pickup and delivery)
    *     at the same time, otherwise the hard constraint is violated.
    *
    * @param left
    *   The left neighborhood; all moves delivered by this one will be considered.
    * @param right
    *   The moves returned by this Neighborhood must improve the main Objective, taking into account
    *   that a left move has been already applied.
    */
  def apply(left: Neighborhood, right: Move => Neighborhood): DynAndThen = {
    new DynAndThen(left, right)
  }
}

/** This Combinator allows to build [[CompositeMove]]s.
  *
  * A [[CompositeMove]] is a move composed of two sub-[[oscar.cbls.core.search.Move]]s. The key point is that the two moves
  * are evaluated as one, meaning that only the composite move is required to improve the objective
  * function, not the sub-moves. The main mechanism is implemented in the
  * [[oscar.cbls.core.computation.objective.composite]] package.
  *
  * Usage:
  *   - Explore complex sub-moves, where the left one worsens the solution and the second one
  *     improves it.
  *   - Pickup and Delivery example: we have to insert/move/remove two nodes (pickup and delivery)
  *     at the same time, otherwise the hard constraint is violated.
  *
  * @param left
  *   The left neighborhood; all moves delivered by this one will be considered.
  * @param right
  *   The moves returned by this Neighborhood must improve the main Objective, taking into account
  *   that a left move has been already applied.
  */
class DynAndThen(left: Neighborhood, right: Move => Neighborhood)
    extends NeighborhoodCombinator("DynAndThen", List(left)) {
  private var _compositionProfilerOpt: Option[CompositionProfiler] = None

  override def searchProfiler(): Option[CompositionProfiler] = _compositionProfilerOpt

  override def profileSearch(): Unit = {
    searchProfiler() match {
      case None =>
        _compositionProfilerOpt = Some(CompositionProfiler(this, Some(left)))
        left.profileSearch()
      case _ => ;
    }
  }

  /** Explores this Combinator. This is where you put the logic of your Combinator.
    *
    * @param objective
    *   The Objective of the search
    * @return
    *   The search result, either [[oscar.cbls.core.search.MoveFound]] or
    *   [[oscar.cbls.core.search.NoMoveFound]]
    */
  override protected[this] def exploreCombinator(objective: Objective): SearchResult = {
    val baseObjExplorer = objective.newExploration[CompositeMove]()
    val searchResult =
      left.getMove(CompositeLeftStub(objective, baseObjExplorer, right, _compositionProfilerOpt))
    searchResult
  }

}
