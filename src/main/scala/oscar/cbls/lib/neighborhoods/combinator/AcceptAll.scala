package oscar.cbls.lib.neighborhoods.combinator

import oscar.cbls.{Neighborhood, Objective}
import oscar.cbls.core.search.{NeighborhoodCombinator, SearchResult}

/** A combinator that modifies the objective function into an acceptAll (except constraint
  * violations, depending on the parameter)
  * @param n
  *   the base neighborhood
  * @param allowsConstrainViolation
  *   true to also accept violation of string constraints (but it is a bad idea)
  */
case class AcceptAll(n: Neighborhood, allowsConstrainViolation: Boolean = false)
    extends NeighborhoodCombinator("AcceptAll", List(n)) {
  override protected[this] def exploreCombinator(objective: Objective): SearchResult =
    n.getMove(
      oscar.cbls.core.computation.objective
        .AcceptAll(objective.objValue, objective.mustBeZero, allowsConstrainViolation)
    )
}
