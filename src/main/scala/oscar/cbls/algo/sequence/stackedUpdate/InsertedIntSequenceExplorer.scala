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

package oscar.cbls.algo.sequence.stackedUpdate

import oscar.cbls.algo.sequence.IntSequenceExplorer
import oscar.cbls.algo.sequence.concrete.RootIntSequenceExplorer

/** A stacked explorer dedicated for [[InsertedIntSequence]].
  *
  * It's used to explore an [[InsertedIntSequence]]. It uses the original explorer, except when the
  * explorer is at the position of the inserted point. Reaching this particular point, the original
  * explorer doesn't move so that we can still reach next/prev in the original sequence.
  *
  * @param seq
  *   The [[InsertedIntSequence]]
  * @param position
  *   The current position in the sequence
  * @param explorerInOriginalSequence
  *   The original explorer, that is without the inserted point
  */
class InsertedIntSequenceExplorer(
  seq: InsertedIntSequence,
  val position: Int,
  val explorerInOriginalSequence: IntSequenceExplorer
) extends IntSequenceExplorer {
  private val atInsertedValue = position == seq.insertAfterPos + 1
  override val value: Int =
    if (atInsertedValue) seq.insertedValue else explorerInOriginalSequence.value

  override def next: Option[IntSequenceExplorer] = {
    val nextPosition = position + 1
    if (atInsertedValue) {
      // we are leaving the inserted position
      explorerInOriginalSequence.next match {
        case None => None
        case Some(nextOriginalExplorer) =>
          Some(new InsertedIntSequenceExplorer(seq, nextPosition, nextOriginalExplorer))
      }
    } else {
      if (nextPosition == seq.insertAfterPos + 1) {
        // Getting into the inserted position
        // Original explorer doesn't change
        Some(new InsertedIntSequenceExplorer(seq, nextPosition, explorerInOriginalSequence))
      } else {
        // nothing special
        explorerInOriginalSequence.next match {
          case None => None
          case Some(nextOriginalExplorer) =>
            Some(new InsertedIntSequenceExplorer(seq, nextPosition, nextOriginalExplorer))
        }
      }
    }
  }

  override def prev: Option[IntSequenceExplorer] = {
    if(position == 0) return Some(new RootIntSequenceExplorer(seq))
    val prevPosition = position - 1
    if (atInsertedValue) {
          Some(new InsertedIntSequenceExplorer(seq, prevPosition, explorerInOriginalSequence))
    } else {
      explorerInOriginalSequence.prev match {
        case None => None
        case Some(prevOriginalExplorer) =>
          Some(new InsertedIntSequenceExplorer(seq, prevPosition, prevOriginalExplorer))
      }
    }
  }
}
