package oscar.cbls.lib.neighborhoods.combinator

import oscar.cbls.core.computation.objective.Objective
import oscar.cbls.core.search.{Neighborhood, NeighborhoodCombinator, NoMoveFound, SearchResult}

/** Companion object of the [[MaxMoves]] class. */
object MaxMoves {

  /** A neighborhood combinator that stops the search after a maximal number of explorations. The
    * combinator can be reset through the reset method.
    *
    * @param neighborhood
    *   The neighborhood to explore.
    * @param maxMoves
    *   The maximal number of times the neighborhood can be explored. Passed this number, this
    *   combinator will return NoMoveFound without exploring this neighborhood.
    */
  def apply(neighborhood: Neighborhood, maxMoves: Int): MaxMoves =
    new MaxMoves(neighborhood, maxMoves)
}

/** A neighborhood combinator that stops the search after a maximal number of explorations. The
  * combinator cna be reset through the reset method.
  *
  * @param neighborhood
  *   The neighborhood to explore.
  * @param maxMoves
  *   The maximal number of times the neighborhood can be explored. Passed this number, this
  *   combinator will return NoMoveFound without exploring this neighborhood.
  */
class MaxMoves(neighborhood: Neighborhood, maxMoves: Int)
    extends NeighborhoodCombinator("MaxMoves", List(neighborhood)) {

  private[this] var remainingMoves = maxMoves

  override protected[this] def exploreCombinator(objective: Objective): SearchResult = {

    if (remainingMoves <= 0) NoMoveFound
    else {
      remainingMoves -= 1
      neighborhood.getMove(objective)
    }
  }

  override def reset(): Unit = {
    remainingMoves = maxMoves
    super.reset()
  }
}
