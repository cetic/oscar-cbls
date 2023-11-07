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
  * @param atInsertedValue
  *   Whether or not the explorer is at the inserted value
  * @param originalExplorerIsAtInsertionPosition
  *   Whether or not the original explorer is at insertion position
  */
class InsertedIntSequenceExplorer(
                                   seq: InsertedIntSequence,
                                   val position: Int,
                                   explorerInOriginalSequence: Option[IntSequenceExplorer],
                                   atInsertedValue: Boolean,
                                   originalExplorerIsAtInsertionPosition: Boolean
) extends IntSequenceExplorer {
  override val value: Int =
    if (atInsertedValue) seq.insertedValue else explorerInOriginalSequence.get.value

  override def next: Option[IntSequenceExplorer] = {
    val nextPosition = position+1
    if (atInsertedValue) {
      // we are leaving the inserted position
      explorerInOriginalSequence match {
        case None => None
        case Some(p) =>
          if (originalExplorerIsAtInsertionPosition) {
            // Original explorer already at position+1
            Some(
              new InsertedIntSequenceExplorer(
                seq,
                nextPosition,
                explorerInOriginalSequence,
                atInsertedValue = false,
                originalExplorerIsAtInsertionPosition = false
              )
            )
          } else {
            // Need to get next original explorer
            p.next match {
              case None => None
              case nextOriginalExplorer =>
                Some(
                  new InsertedIntSequenceExplorer(
                    seq,
                    nextPosition,
                    nextOriginalExplorer,
                    atInsertedValue = false,
                    originalExplorerIsAtInsertionPosition = false
                  )
                )
            }
          }
      }
    } else {
      if (nextPosition == seq.pos) {
        // Getting into the inserted position
        // Original explorer doesn't change
        Some(
          new InsertedIntSequenceExplorer(
            seq,
            nextPosition,
            explorerInOriginalSequence,
            atInsertedValue = true,
            originalExplorerIsAtInsertionPosition = false
          )
        )
      } else {
        // nothing special
        explorerInOriginalSequence.get.next match {
          case None => None
          case nextOriginalExplorer =>
            Some(
              new InsertedIntSequenceExplorer(
                seq,
                nextPosition,
                nextOriginalExplorer,
                atInsertedValue = false,
                originalExplorerIsAtInsertionPosition = false
              )
            )
        }
      }
    }
  }

  override def prev: Option[IntSequenceExplorer] = {
    val prevPosition = position-1
    if (atInsertedValue) {
      explorerInOriginalSequence match {
        case None    => None
        case Some(p) =>
          // Leaving the inserted position
          if (!originalExplorerIsAtInsertionPosition) {
            // Original explorer is not above so already at insertion position minus 1
            Some(
              new InsertedIntSequenceExplorer(
                seq,
                prevPosition,
                explorerInOriginalSequence,
                atInsertedValue = false,
                originalExplorerIsAtInsertionPosition = false
              )
            )
          } else {
            // Original explorer is above, so at insertion position ==> need to get prev
            p.prev match {
              case None => None
              case prevOriginalExplorer =>
                Some(
                  new InsertedIntSequenceExplorer(
                    seq,
                    prevPosition,
                    prevOriginalExplorer,
                    atInsertedValue = false,
                    originalExplorerIsAtInsertionPosition = false
                  )
                )
            }
          }
      }
    } else {
      if (prevPosition == seq.pos) {
        // Getting into the inserted position from above it
        // Not moving the explorer ==> original explorer is above
        Some(
          new InsertedIntSequenceExplorer(
            seq,
            prevPosition,
            explorerInOriginalSequence,
            atInsertedValue = true,
            originalExplorerIsAtInsertionPosition = true
          )
        )
      } else {
        // Nothing special
        explorerInOriginalSequence.get.prev match {
          case None => None
          case prevOriginalExplorer =>
            Some(
              new InsertedIntSequenceExplorer(
                seq,
                prevPosition,
                prevOriginalExplorer,
                atInsertedValue = false,
                originalExplorerIsAtInsertionPosition = false
              )
            )
        }
      }
    }
  }
}
