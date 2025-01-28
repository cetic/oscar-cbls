package oscar.cbls.modeling.search

import oscar.cbls.core.search.Neighborhood
import oscar.cbls.lib.neighborhoods.combinator.{RoundRobin, UpdateDisplay}
import oscar.cbls.visual.OscaRDisplay

/** This trait collects methods used to construct combinators, which are mechanisms used to combine
  * neighborhoods in order to construct complex search procedures. To allow chaining operations,
  * combinators themselves extend the [[Neighborhood]] type.
  */
trait Combinator {

  /** Constructs a combinator which performs a round-robin metaheuristic on a list of given
    * neighborhoods. This metaheuristic changes the currently explored neighborhood after it has
    * been invoked k times (where k is given as input) or if it is exhausted.
    *
    * @param robins
    *   A sequence of tuples (n, k) where n is a Neighborhood to be explored and k is the number of
    *   times n can be explored in a row.
    * @param tabu
    *   The number of iterations a neighborhood has to wait before being explored again (after a
    *   failure).
    * @param name
    *   The name of the neighborhood combinator.
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.combinator.RoundRobin]] combinator.
    */
  def roundRobin(
    robins: Seq[(Neighborhood, Int)],
    tabu: Int = 1,
    name: String = "RoundRobin"
  ): RoundRobin = {
    RoundRobin(robins.toArray, tabu, name)
  }

  /** Constructs a combinator that triggers the redrawing of the display after a move is performed.
    *
    * @param n
    *   the Neighborhood used to explore the solution space.
    * @param display
    *   the display used to visualize the problem.
    * @return
    *   an instance of an [[oscar.cbls.lib.neighborhoods.combinator.UpdateDisplay]] combinator.
    */
  def updateDisplay(n: Neighborhood, display: OscaRDisplay): UpdateDisplay = {
    UpdateDisplay(n, display)
  }
}
