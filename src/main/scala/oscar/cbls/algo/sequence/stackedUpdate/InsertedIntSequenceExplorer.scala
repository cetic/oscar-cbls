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

class InsertedIntSequenceExplorer(
  seq: InsertedIntSequence,
  val position: Int,
  explorerInOriginalSeq: Option[IntSequenceExplorer],
  atInsertedValue: Boolean,
  originalExplorerIsAbove: Boolean
) extends IntSequenceExplorer {
  override val value: Int =
    if (atInsertedValue) seq.insertedValue else explorerInOriginalSeq.head.value

  override def next: Option[IntSequenceExplorer] = {
    if (atInsertedValue) {
      // we are leaving the inserted position
      explorerInOriginalSeq match {
        case None => None
        case Some(p) =>
          if (originalExplorerIsAbove)
            Some(
              new InsertedIntSequenceExplorer(
                seq,
                position + 1,
                explorerInOriginalSeq,
                atInsertedValue = false,
                originalExplorerIsAbove = false
              )
            )
          else {
            p.next match {
              case None => None
              case Some(next1) =>
                Some(
                  new InsertedIntSequenceExplorer(
                    seq,
                    position + 1,
                    Some(next1),
                    atInsertedValue = false,
                    originalExplorerIsAbove = false
                  )
                )
            }
          }
      }
    } else {
      val nextPosition = position + 1
      if (nextPosition == seq.pos) {
        // we are getting into the inserted position
        Some(
          new InsertedIntSequenceExplorer(
            seq,
            position + 1,
            explorerInOriginalSeq,
            atInsertedValue = true,
            originalExplorerIsAbove = false
          )
        )
      } else {
        // nothing special
        explorerInOriginalSeq.head.next match {
          case None => None
          case Some(next) =>
            Some(
              new InsertedIntSequenceExplorer(
                seq,
                position + 1,
                Some(next),
                atInsertedValue = false,
                originalExplorerIsAbove = false
              )
            )
        }
      }
    }
  }

  override def prev: Option[IntSequenceExplorer] = {
    if (atInsertedValue) {
      explorerInOriginalSeq match {
        case None    => None
        case Some(p) =>
          // we are leaving the inserted position
          if (!originalExplorerIsAbove)
            Some(
              new InsertedIntSequenceExplorer(
                seq,
                position - 1,
                explorerInOriginalSeq,
                atInsertedValue = false,
                originalExplorerIsAbove = false
              )
            )
          else {
            p.prev match {
              case None => None
              case Some(prev1) =>
                Some(
                  new InsertedIntSequenceExplorer(
                    seq,
                    position - 1,
                    Some(prev1),
                    atInsertedValue = false,
                    originalExplorerIsAbove = false
                  )
                )
            }
          }
      }
    } else {
      val prevPosition = position - 1
      if (prevPosition == seq.pos) {
        // we are getting into the inserted position
        Some(
          new InsertedIntSequenceExplorer(
            seq,
            position - 1,
            explorerInOriginalSeq,
            atInsertedValue = true,
            originalExplorerIsAbove = true
          )
        )
      } else {
        // nothing special
        explorerInOriginalSeq.head.prev match {
          case None => None
          case Some(prev) =>
            Some(
              new InsertedIntSequenceExplorer(
                seq,
                position - 1,
                Some(prev),
                atInsertedValue = false,
                originalExplorerIsAbove = false
              )
            )
        }
      }
    }
  }
}
