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

/** A stacked explorer dedicated for [[MovedIntSequence]].
  *
  * It's used to explore a [[MovedIntSequence]]. It uses the original explorer and generates a new
  * one changing pivot.
  *
  * @param intSequence
  *   The [[InsertedIntSequence]]
  * @param position
  *   The current position in the sequence
  * @param explorerInOriginalSequence
  *   The original explorer, that is with the removed point
  */
class RemovedIntSequenceExplorer(
  val intSequence: RemovedIntSequence,
  val position: Int,
  explorerInOriginalSequence: IntSequenceExplorer
) extends IntSequenceExplorer {
  override val value: Int = explorerInOriginalSequence.value

  override def prev: Option[IntSequenceExplorer] = {
    if(position == 0) return Some(new RootIntSequenceExplorer(intSequence))
    // Just skip the removed point
    explorerInOriginalSequence.prev match {
      case None => None
      case Some(prevPrevExplorerInOriginalSequence) =>
        if (prevPrevExplorerInOriginalSequence.position == intSequence.explorerAtRemovePos.position)
          // At pos of removed point, need to go back once more
          prevPrevExplorerInOriginalSequence.prev match {
            case None => None
            case Some(previous2ExplorerInOriginalSequence) =>
                Some(new RemovedIntSequenceExplorer(intSequence, position - 1, previous2ExplorerInOriginalSequence))
          }
        else {
            Some(new RemovedIntSequenceExplorer(intSequence, position - 1, prevPrevExplorerInOriginalSequence))
        }
    }
  }

  override def next: Option[IntSequenceExplorer] = {
    // Just skip the removed point
    explorerInOriginalSequence.next match {
      case None => None
      case Some(nextExplorerInOriginalSequence) =>
        if (nextExplorerInOriginalSequence.position == intSequence.explorerAtRemovePos.position)
          nextExplorerInOriginalSequence.next match {
            case None => None
            case Some(nextNextExplorerInOriginalSequence) =>
              Some(new RemovedIntSequenceExplorer(intSequence, position + 1, nextNextExplorerInOriginalSequence))
          }
        else Some(new RemovedIntSequenceExplorer(intSequence, position + 1, nextExplorerInOriginalSequence))
    }
  }
}
