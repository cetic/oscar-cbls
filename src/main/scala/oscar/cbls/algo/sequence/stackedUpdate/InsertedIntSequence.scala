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

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}

/** Quick and stackable update of an [[IntSequence]] applying an insertion move on an existing
  * sequence.
  *
  * @param intSequence
  *   The original sequence as a [[IntSequence]]
  * @param insertedValue
  *   The value to insert as an [[Int]]
  * @param insertAfterPosExpl
  *   The insertion position as an [[Int]]
  * @param depth
  *   The depth of the current update
  */
class InsertedIntSequence(
  intSequence: IntSequence,
  val insertedValue: Int,
  val insertAfterPosExpl: Option[IntSequenceExplorer],
  depth: Int
) extends StackedUpdateIntSequence(depth) {

  private val originalExplorerAtInsertPosition =
    IntSequenceExplorer.getNextExplorerOrElse(insertAfterPosExpl, intSequence.explorerAtPosition(0))
  val insertAfterPos: Int = IntSequenceExplorer.getPosOrElse(insertAfterPosExpl, -1)

  override val size: Int = intSequence.size + 1

  override def nbOccurrence(value: Int): Int =
    if (value == this.insertedValue) intSequence.nbOccurrence(value) + 1
    else intSequence.nbOccurrence(value)

  override def unorderedContentNoDuplicateWithNBOccurrences: List[(Int, Int)] =
    unorderedContentNoDuplicate.map(value =>
      (
        value,
        if (value == insertedValue) intSequence.nbOccurrence(value) + 1
        else intSequence.nbOccurrence(value)
      )
    )

  override def descriptorString: String =
    s"${intSequence.descriptorString}.inserted(val:$insertedValue after pos:$insertAfterPos)"

  override def unorderedContentNoDuplicate: List[Int] =
    if (intSequence.nbOccurrence(insertedValue) == 0)
      insertedValue :: intSequence.unorderedContentNoDuplicate
    else intSequence.unorderedContentNoDuplicate

  override def positionsOfValue(value: Int): List[Int] = {
    val positionsBefore = intSequence.positionsOfValue(value)
    val positionsAfter  = positionsBefore.map(oldPos2NewPos)
    if (value == insertedValue) List(insertAfterPos+1) ::: positionsAfter
    else positionsAfter
  }

  // Private fast method to to map an old position to it's new value
  @inline
  private def oldPos2NewPos(oldPOs: Int): Int = {
    if (oldPOs <= insertAfterPos) oldPOs else oldPOs + 1
  }

  override def originalExplorerAtPosition(position: Int): Option[IntSequenceExplorer] = {
    if (position == insertAfterPos) insertAfterPosExpl
    else if (position == insertAfterPos + 1) originalExplorerAtInsertPosition
    else intSequence.explorerAtPosition(position)
  }

  override def explorerAtPosition(position: Int): Option[IntSequenceExplorer] = {
    if (position == insertAfterPos+1) {
      // Explorer at the inserted point position
      if (position == 0) {
        // Inserted point position is the start of the sequence
        Some(
          new InsertedIntSequenceExplorer(this, position, originalExplorerAtInsertPosition, true, true)
        )
      } else {
        // Inserted point position is later in the sequence
        Some(
          new InsertedIntSequenceExplorer(this, position, insertAfterPosExpl, true, false)
        )
      }
    } else {
      val explorer = {
        // Explorer is empty or position isn't close enough to use next/prev (O(1)) on the known explorer
        if (originalExplorerAtInsertPosition.isEmpty || Math.abs(position - (insertAfterPos+1)) > Math.log(size))
          intSequence.explorerAtPosition(position)
        else
        // Position is close enough to use next/prev (O(1)) on the known explorer
          originalExplorerAtInsertPosition.get.untilPosition({
            if (position < insertAfterPos+1) position
            else position - 1
          })
      }

      explorer match {
        case None    => None
        case Some(p) => Some(new InsertedIntSequenceExplorer(this, position, Some(p), false, false))
      }
    }
  }

  override def contains(value: Int): Boolean =
    value == this.insertedValue || intSequence.contains(value)

  override def commitPendingMoves: IntSequence =
    intSequence.commitPendingMoves.insertAfterPosition(insertedValue, insertAfterPosExpl, fast = false)

  override def isEmpty: Boolean = false

  override def valueAtPosition(position: Int): Option[Int] = {
    if (position == insertAfterPos+1) Some(insertedValue)
    else if (position < insertAfterPos+1) intSequence.valueAtPosition(position)
    else intSequence.valueAtPosition(position - 1)
  }
}
