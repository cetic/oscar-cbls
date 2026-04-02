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

import oscar.cbls.core.search.Move

/** A move composed of a List of moves.
  *
  * @param moves
  *   The list of moves to be applied.
  * @param objValueAfter
  *   The objective value of the neighbor. Used for comparison and validation.
  * @param neighborhoodName
  *   The name of the [[oscar.cbls.core.search.Neighborhood]] that generated this Move.
  */
case class CompositeMovesWithList(
                                   moves: List[Move],
                                   override val objValueAfter: Long,
                                   override val neighborhoodName: String
) extends Move(objValueAfter, neighborhoodName) {

  override def commit(): Unit = {
    moves.foreach(_.commit())
  }

  override def toString: String = {
    var toReturn: String = s"$neighborhoodName \nComposes:\n"
    toReturn += moves.map(_.toString.split("objValue after").head).mkString("\t-", "\t-", "")
    toReturn += super.toString + "\n"

    toReturn
  }
}
