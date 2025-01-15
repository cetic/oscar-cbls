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

/** A move composed of two sub-moves
  *
  * @param leftMove
  *   The first move to be applied.
  * @param rightMove
  *   The second move to be applied.
  * @param objValueAfter
  *   The objective value of the neighbor. Used for comparison and validation.
  * @param compositeNeighborhoodName
  *   The name of this move.
  */
case class CompositeMove(
  leftMove: Move,
  rightMove: Move,
  override val objValueAfter: Long,
  compositeNeighborhoodName: String
) extends Move(objValueAfter, compositeNeighborhoodName) {

  /** Commits this move. */
  override def commit(): Unit = {
    leftMove.commit()
    rightMove.regularize().commit()
  }

  override def toString: String = {
    val leftWithoutObj  = leftMove.toString.split("objValue after").head
    val rightWithoutObj = rightMove.toString.split("objValue after").head
    s"Composite move [$leftWithoutObj and then $rightWithoutObj] objValue after $objValueAfter"
  }

  override def regularize(): Move = {
    CompositeMove(
      leftMove.regularize(),
      rightMove.regularize(),
      objValueAfter,
      compositeNeighborhoodName
    )
  }
}
