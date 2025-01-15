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

/** A move that can call a unit function before and/or after the commit of an initial move.
  *
  * @param initialMove
  *   The move to commit.
  * @param beforeMove
  *   An optional function to call before the initial move is committed.
  * @param afterMove
  *   An optional function to call after the initial move is committed.
  */
case class InstrumentedMove(
  initialMove: Move,
  beforeMove: Option[() => Unit],
  afterMove: Option[() => Unit]
) extends Move(initialMove.objValueAfter, initialMove.neighborhoodName) {

  override def commit(): Unit = {
    beforeMove.foreach(f => f())
    initialMove.commit()
    afterMove.foreach(f => f())
  }

  override def toString: String = initialMove.toString
}
