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

package oscar.cbls.core.search

/** The result of an exploration * */
sealed abstract class SearchResult {}

/** The result of an exploration where no valid move were found * */
object NoMoveFound extends SearchResult

/** Companion object of MoveFound */
object MoveFound {
  def apply(move: Move): MoveFound = {
    new MoveFound(move)
  }

  def unapply(moveFound: MoveFound): Move = {
    moveFound.move
  }

}

/** The result of an exploration were a valid move has been found * */
class MoveFound(val move: Move) extends SearchResult {

  /** Commits the validated move. */
  def commit(): Unit = {
    move.commit()
  }
  def objAfter(): Long          = move.objAfter()
  override def toString: String = s"Move found : $move"
}
