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

/** Quick and stackable update of an [[IntSequence]] applying a removal of an existing node.
  *
  * @param originalSequence
  *   The original sequence
  * @param removePosition
  *   The position of the node to remove
  * @param depth
  *   The depth of the current update
  */
class RemovedIntSequence(val originalSequence: IntSequence, val removePosition: Int, depth: Int)
    extends StackedUpdateIntSequence(depth) {

  val removedValue = originalSequence.valueAtPosition(removePosition).get

  override def descriptorString: String =
    originalSequence.descriptorString + ".removed(pos:" + removePosition + " val:" + removedValue + ")"

  override def nbOccurrence(value: Int): Int =
    if (value == this.removedValue) originalSequence.nbOccurrence(value) - 1
    else originalSequence.nbOccurrence(value)

  override def unorderedContentNoDuplicate: List[Int] =
    if (originalSequence.nbOccurrence(removedValue) > 1)
      originalSequence.unorderedContentNoDuplicate
    else originalSequence.unorderedContentNoDuplicate.filter(_ != removedValue)

  override def unorderedContentNoDuplicateWithNBOccurrences: List[(Int, Int)] =
    unorderedContentNoDuplicate.flatMap(value =>
      if (value == removedValue) {
        val occurrencesBefore = originalSequence.nbOccurrence(value)
        if (occurrencesBefore == 1) None
        else Some((value, occurrencesBefore - 1))
      } else Some((value, originalSequence.nbOccurrence(value)))
    )

  override val size: Int = originalSequence.size - 1

  override def explorerAtPosition(position: Int): Option[IntSequenceExplorer] = {
    originalSequence.explorerAtPosition(if (position < this.removePosition) position else position + 1) match {
      case None    => None
      case Some(e) => Some(new RemovedIntSequenceExplorer(this, position, e))
    }
  }

  // TODO use tail
  override def positionsOfValue(value: Int): List[Int] = {
    var positionsBefore     = originalSequence.positionsOfValue(value)
    var toReturn: List[Int] = List.empty
    while (positionsBefore.nonEmpty) {
      val oldPos = positionsBefore.head
      positionsBefore = positionsBefore.tail
      if (oldPos < this.removePosition) {
        toReturn = List(oldPos) ::: toReturn
      } else if (oldPos > removePosition) {
        toReturn = List(oldPos - 1) ::: toReturn
      }
    }
    toReturn
  }

  /** Returns the new position of the specified one considering the removal */
  def oldPos2NewPos(oldPos: Int): Int = {
    if (oldPos < this.removePosition) oldPos else oldPos - 1
  }

  override def contains(value: Int): Boolean = {
    if (value == removedValue) originalSequence.nbOccurrence(value) > 1
    else originalSequence.contains(value)
  }

  override def commitPendingMoves: IntSequence =
    originalSequence.commitPendingMoves.delete(this.removePosition, fast = false)

  override def valueAtPosition(position: Int): Option[Int] = {
    if (position >= this.removePosition) originalSequence.valueAtPosition(position + 1)
    else originalSequence.valueAtPosition(position)
  }
}
