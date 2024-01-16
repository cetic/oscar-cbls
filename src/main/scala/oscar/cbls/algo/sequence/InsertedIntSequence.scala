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

/** Quick and stackable update of an [[IntSequence]] applying an insertion move on an existing
  * sequence.
  *
  * @param intSequence
  *   The original sequence
  * @param insertedValue
  *   The value to insert
  * @param insertAfterPosExplorer
  *   The explorer at the insertion position
  * @param depth
  *   The depth of the current update
  */
class InsertedIntSequence(
  intSequence: IntSequence,
  val insertedValue: Int,
  val insertAfterPosExplorer: IntSequenceExplorer,
  depth: Int
) extends StackedUpdateIntSequence(depth) {

  private val originalExplorerAtInsertPosition = insertAfterPosExplorer.next
  val insertAfterPos: Int                      = insertAfterPosExplorer.position

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
    if (value == insertedValue) List(insertAfterPos + 1) ::: positionsAfter
    else positionsAfter
  }

  // Private fast method to to map an old position to it's new value
  @inline
  private def oldPos2NewPos(oldPOs: Int): Int = {
    if (oldPOs <= insertAfterPos) oldPOs else oldPOs + 1
  }

  override def explorerAtPosition(position: Int): Option[IntSequenceExplorer] = {
    if (position == -1) Some(new RootIntSequenceExplorer(this, true))
    else if (position == size) Some(new RootIntSequenceExplorer(this, false))
    else if (position == insertAfterPos + 1) {
      Some(new InsertedIntSequenceExplorer(this, position, insertAfterPosExplorer))
    } else {
      val originPos = if (position < insertAfterPos + 1) position else position - 1
      val explorer = {
        // Explorer is empty or position isn't close enough to use next/prev (O(1)) on the known explorer
        if (Math.abs(position - (insertAfterPos + 1)) > Math.log(size))
          intSequence.explorerAtPosition(originPos)
        else
          // Position is close enough to use next/prev (O(1)) on the known explorer
          originalExplorerAtInsertPosition.exploreToPosition(originPos)
      }

      explorer match {
        case None    => None
        case Some(p) => Some(new InsertedIntSequenceExplorer(this, position, p))
      }
    }
  }

  override def contains(value: Int): Boolean =
    value == this.insertedValue || intSequence.contains(value)

  override def commitPendingMoves: IntSequence =
    intSequence.commitPendingMoves.insertAfterPosition(insertedValue, insertAfterPosExplorer)

  override def isEmpty: Boolean = false

  override def valueAtPosition(position: Int): Option[Int] = {
    if (position == insertAfterPos + 1) Some(insertedValue)
    else if (position < insertAfterPos + 1) intSequence.valueAtPosition(position)
    else intSequence.valueAtPosition(position - 1)
  }
}
