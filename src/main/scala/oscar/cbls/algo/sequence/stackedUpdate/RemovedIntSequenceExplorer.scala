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

class RemovedIntSequenceExplorer(
  seq: RemovedIntSequence,
  val position: Int,
  explorerInOriginalSeq: IntSequenceExplorer
) extends IntSequenceExplorer {
  override val value: Int = explorerInOriginalSeq.value

  override def prev: Option[IntSequenceExplorer] = {
    explorerInOriginalSeq.prev match {
      case None => None
      case Some(tentativePos) =>
        if (tentativePos.position == seq.positionOfDelete)
          tentativePos.prev match {
            case None => None
            case Some(secondTentativePos) =>
              Some(new RemovedIntSequenceExplorer(seq, position - 1, secondTentativePos))
          }
        else Some(new RemovedIntSequenceExplorer(seq, position - 1, tentativePos))
    }
  }

  override def next: Option[IntSequenceExplorer] = {
    explorerInOriginalSeq.next match {
      case None => None
      case Some(tentativePos) =>
        if (tentativePos.position == seq.positionOfDelete)
          tentativePos.next match {
            case None => None
            case Some(secondTentativePos) =>
              Some(new RemovedIntSequenceExplorer(seq, position + 1, secondTentativePos))
          }
        else Some(new RemovedIntSequenceExplorer(seq, position + 1, tentativePos))
    }
  }
}
