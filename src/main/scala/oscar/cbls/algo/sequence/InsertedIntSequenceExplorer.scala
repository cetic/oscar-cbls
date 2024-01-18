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

/** A stacked explorer dedicated for [[InsertedIntSequence]].
  *
  * It's used to explore an [[InsertedIntSequence]]. It uses the original explorer, except when the
  * explorer is at the position of the inserted point. Reaching this particular point, the original
  * explorer doesn't move so that we can still reach next/prev in the original sequence.
  *
  * @param intSequence
  *   The [[InsertedIntSequence]]
  * @param position
  *   The current position in the sequence
  * @param explorerInOriginalSequence
  *   The original explorer, that is without the inserted point
  */
class InsertedIntSequenceExplorer(
  intSequence: InsertedIntSequence,
  val position: Int,
  val explorerInOriginalSequence: IntSequenceExplorer
) extends IntSequenceExplorer(intSequence) {
  private val atInsertedValue = position == intSequence.insertAfterPos + 1
  override val value: Int =
    if (atInsertedValue) intSequence.insertedValue else explorerInOriginalSequence.value

  override def next: IntSequenceExplorer = {
    val nextPosition = position + 1
    if (nextPosition == intSequence.size) {
      new RootIntSequenceExplorer(intSequence, false)
    } else {
      if (atInsertedValue) {
        val nextExplorerInOriginalSequence = explorerInOriginalSequence.next
        // we are leaving the inserted position
        new InsertedIntSequenceExplorer(intSequence, nextPosition, nextExplorerInOriginalSequence)
      } else {
        if (nextPosition == intSequence.insertAfterPos + 1) {
          // Getting into the inserted position
          // Original explorer doesn't change
          new InsertedIntSequenceExplorer(intSequence, nextPosition, explorerInOriginalSequence)
        } else {
          // nothing special
          new InsertedIntSequenceExplorer(
            intSequence,
            nextPosition,
            explorerInOriginalSequence.next
          )
        }
      }
    }
  }

  override def prev: IntSequenceExplorer = {
    if (position == 0) {
      new RootIntSequenceExplorer(intSequence, true)
    } else {
      val prevPosition = position - 1
      if (atInsertedValue) {
        new InsertedIntSequenceExplorer(intSequence, prevPosition, explorerInOriginalSequence)
      } else {
        new InsertedIntSequenceExplorer(intSequence, prevPosition, explorerInOriginalSequence.prev)
      }
    }
  }
}
