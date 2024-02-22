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

package oscar.cbls.algo.sequence

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
  override val intSequence: RemovedIntSequence,
  val position: Int,
  explorerInOriginalSequence: IntSequenceExplorer
) extends IntSequenceExplorer(intSequence) {
  override val value: Int = explorerInOriginalSequence.value

  override def next: IntSequenceExplorer = {
    if (position == intSequence.size - 1) {
      new RootIntSequenceExplorer(intSequence, false)
    } else {
      val nextExplorerInOriginalSequence = explorerInOriginalSequence.next
      // Just skip the removed point
      if (nextExplorerInOriginalSequence.position == intSequence.explorerAtRemovePos.position)
        nextExplorerInOriginalSequence.next match {
          case nextNext: RootIntSequenceExplorer => nextNext
          case nextNext: IntSequenceExplorer =>
            new RemovedIntSequenceExplorer(intSequence, position + 1, nextNext)
        }
      else
        new RemovedIntSequenceExplorer(intSequence, position + 1, nextExplorerInOriginalSequence)
    }
  }

  override def prev: IntSequenceExplorer = {
    if (position == 0) {
      new RootIntSequenceExplorer(intSequence, true)
    } else {
      val prevExplorerInOriginalSequence = explorerInOriginalSequence.prev
      // Just skip the removed point
      if (prevExplorerInOriginalSequence.position == intSequence.explorerAtRemovePos.position)
        // At pos of removed point, need to go back once more
        prevExplorerInOriginalSequence.prev match {
          case _: RootIntSequenceExplorer => new RootIntSequenceExplorer(intSequence, true)
          case prevPrev: IntSequenceExplorer =>
            new RemovedIntSequenceExplorer(intSequence, position - 1, prevPrev)
        }
      else {
        new RemovedIntSequenceExplorer(intSequence, position - 1, prevExplorerInOriginalSequence)
      }
    }
  }
}
