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

class RemovedIntSequence(val seq: IntSequence, val positionOfDelete: Int, depth: Int)
    extends StackedUpdateIntSequence(depth) {

  val removedValue = seq.valueAtPosition(positionOfDelete).head

  override def descriptorString: String =
    seq.descriptorString + ".removed(pos:" + positionOfDelete + " val:" + removedValue + ")"

  override def nbOccurrence(value: Int): Int =
    if (value == this.removedValue) seq.nbOccurrence(value) - 1 else seq.nbOccurrence(value)

  override def unorderedContentNoDuplicate: List[Int] =
    if (seq.nbOccurrence(removedValue) > 1) seq.unorderedContentNoDuplicate
    else seq.unorderedContentNoDuplicate.filter(_ != removedValue)

  override def unorderedContentNoDuplicateWithNBOccurrences: List[(Int, Int)] =
    unorderedContentNoDuplicate.flatMap(value =>
      if (value == removedValue) {
        val occurencesBefore = seq.nbOccurrence(value)
        if (occurencesBefore == 1) None
        else Some((value, occurencesBefore - 1))
      } else Some((value, seq.nbOccurrence(value)))
    )

  override val size: Int = seq.size - 1

  override def explorerAtPosition(position: Int): Option[IntSequenceExplorer] = {
    seq.explorerAtPosition(if (position < this.positionOfDelete) position else position + 1) match {
      case None    => None
      case Some(e) => Some(new RemovedIntSequenceExplorer(this, position, e))
    }
  }

  override def positionsOfValue(value: Int): List[Int] = {
    var positionsBefore     = seq.positionsOfValue(value)
    var toReturn: List[Int] = null
    while (positionsBefore != null) {
      val oldPos = positionsBefore.head
      positionsBefore = positionsBefore.tail
      if (oldPos < this.positionOfDelete) {
        toReturn = List(oldPos) ::: toReturn
      } else if (oldPos > positionOfDelete) {
        toReturn = List(oldPos - 1) ::: toReturn
      }
    }
    toReturn
  }

  def oldPos2NewPos(oldPos: Int) = {
    if (oldPos < this.positionOfDelete) oldPos else oldPos - 1
  }

  override def contains(value: Int): Boolean = {
    if (value == removedValue) seq.nbOccurrence(value) > 1
    else seq.contains(value)
  }

  override def commitPendingMoves: IntSequence =
    seq.commitPendingMoves.delete(this.positionOfDelete, fast = false)

  override def valueAtPosition(position: Int): Option[Int] = {
    if (position >= this.positionOfDelete) seq.valueAtPosition(position + 1)
    else seq.valueAtPosition(position)
  }
}
