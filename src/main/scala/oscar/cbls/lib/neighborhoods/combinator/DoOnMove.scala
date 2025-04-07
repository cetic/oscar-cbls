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
  Move,
  MoveFound,
  Neighborhood,
  NeighborhoodCombinator,
  NoMoveFound,
  SearchResult
}

/** Companion object of the [[DoOnMove]] class. */
object DoOnMove {

  /** Creates a DoOnMove combinator that attach custom unit functions to a given neighborhood. The
    * custom functions can be called before or after the commit of a move.
    *
    * @param n
    *   A neighborhood.
    * @param procedureBeforeMove
    *   The procedure to execute just before the move is taken.
    * @param procedureAfterMove
    *   The procedure to execute just after the move is taken.
    * @param procedureOnNoMoveFound
    *   The procedure to execute when the called neighborhood returns no move.
    * @param name
    *   The name of the neighborhood combinator.
    */
  def apply(
    n: Neighborhood,
    procedureBeforeMove: Move => Unit,
    procedureAfterMove: Move => Unit,
    procedureOnNoMoveFound: () => Unit,
    name: String = "DoOnMove"
  ): DoOnMove = {
    new DoOnMove(
      n,
      Some(procedureBeforeMove),
      Some(procedureAfterMove),
      Some(procedureOnNoMoveFound),
      name
    )
  }

  /** Creates a variant of DoOnMove combinator. This variant can only execute a custom unit function
    * ''before'' the move is committed.
    *
    * @param n
    *   A neighborhood.
    * @param procedureBeforeMove
    *   The procedure to execute just before the move is taken.
    * @param name
    *   The name of the neighborhood combinator.
    */
  def beforeMove(
    n: Neighborhood,
    procedureBeforeMove: Move => Unit,
    name: String = "BeforeMove"
  ): DoOnMove = new DoOnMove(n, Some(procedureBeforeMove), None, None, name)

  /** Creates a variant of DoOnMove combinator. This variant can only execute a custom unit function
    * ''after'' the move is committed.
    *
    * @param n
    *   A neighborhood.
    * @param procedureAfterMove
    *   The procedure to execute just after the move is taken.
    * @param name
    *   The name of the neighborhood combinator.
    */
  def afterMove(
    n: Neighborhood,
    procedureAfterMove: Move => Unit,
    name: String = "AfterMove"
  ): DoOnMove = new DoOnMove(n, None, Some(procedureAfterMove), None, name)
}

/** Combinator that attach custom unit functions to a given neighborhood. The custom functions can
  * be called before or after the commit of a move.
  *
  * @param n
  *   A neighborhood.
  * @param procedureBeforeMove
  *   The procedure to execute just before the move is taken.
  * @param procedureAfterMove
  *   The procedure to execute just after the move is taken.
  * @param procedureOnNoMoveFound
  *   The procedure to execute when the called neighborhood returns no move.
  * @param neighborhoodCombinatorName
  *   The name of the neighborhood combinator.
  */
class DoOnMove(
  n: Neighborhood,
  procedureBeforeMove: Option[Move => Unit],
  procedureAfterMove: Option[Move => Unit],
  procedureOnNoMoveFound: Option[() => Unit],
  neighborhoodCombinatorName: String
) extends NeighborhoodCombinator(neighborhoodCombinatorName, List(n)) {

  override protected[this] def exploreCombinator(objective: Objective): SearchResult = {
    n.getMove(objective) match {
      case mf: MoveFound =>
        val im = InstrumentedMove(
          mf.move,
          Some(() => procedureBeforeMove.foreach(f => f(mf.move))),
          Some(() => procedureAfterMove.foreach(f => f(mf.move)))
        )
        MoveFound(im)
      case NoMoveFound =>
        procedureOnNoMoveFound.foreach(f => f())
        NoMoveFound
    }
  }
}
