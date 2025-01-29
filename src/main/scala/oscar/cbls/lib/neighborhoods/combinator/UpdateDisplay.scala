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
import oscar.cbls.visual.OscaRDisplay

object UpdateDisplay {

  /** Creates an instance of a combinator that triggers the redrawing of the display after a move is
    * performed or when no move where found (forced redrawing).
    *
    * @param n
    *   the Neighborhood used to explore the solution space.
    * @param display
    *   the display used to visualize the problem.
    */
  def apply(n: Neighborhood, display: OscaRDisplay): UpdateDisplay = {
    new UpdateDisplay(n, display)
  }
}

/** Combinator that triggers the redrawing of the display after a move is performed or when no move
  * where found (forced redrawing).
  *
  * @param n
  *   the Neighborhood used to explore the solution space.
  * @param display
  *   the display used to visualize the problem.
  */
class UpdateDisplay(n: Neighborhood, display: OscaRDisplay)
    extends DoOnMove(
      n,
      None,
      Some(_ => display.redraw()),
      Some(() => display.redraw(true)),
      "UpdateDisplay"
    )
