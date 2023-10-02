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

class InsertedIntSequence(seq: IntSequence, val insertedValue: Int, val pos: Int, depth: Int)
    extends StackedUpdateIntSequence(depth) {
  override val size: Int = seq.size + 1

  override def nbOccurrence(value: Int): Int =
    if (value == this.insertedValue) seq.nbOccurrence(value) + 1 else seq.nbOccurrence(value)

  override def unorderedContentNoDuplicateWithNBOccurrences: List[(Int, Int)] =
    unorderedContentNoDuplicate.map(value =>
      (value, if (value == insertedValue) seq.nbOccurrence(value) + 1 else seq.nbOccurrence(value))
    )

  override def descriptorString: String =
    s"${seq.descriptorString}.inserted(val:$insertedValue pos:$pos)"

  override def unorderedContentNoDuplicate: List[Int] = if (seq.nbOccurrence(insertedValue) == 0)
    insertedValue :: seq.unorderedContentNoDuplicate
  else seq.unorderedContentNoDuplicate

  override def positionsOfValue(value: Int): List[Int] = {
    var positionsBefore     = seq.positionsOfValue(value)
    var toReturn: List[Int] = null
    while (positionsBefore != null) {
      val oldPos = positionsBefore.head
      positionsBefore = positionsBefore.tail
      val newPos = oldPos2NewPos(oldPos)
      toReturn = List(newPos) ::: toReturn
    }
    if (value == insertedValue) List(pos) ::: toReturn
    else toReturn
  }

  @inline
  private def oldPos2NewPos(oldPOs: Int): Int = {
    if (oldPOs < pos) oldPOs else oldPOs + 1
  }

  override def explorerAtPosition(position: Int): Option[IntSequenceExplorer] = {
    if (position == this.pos) {
      if (position == 0) {
        Some(new InsertedIntSequenceExplorer(this, position, seq.explorerAtPosition(0), true, true))
      } else {
        Some(
          new InsertedIntSequenceExplorer(
            this,
            position,
            seq.explorerAtPosition(position - 1),
            true,
            false
          )
        )
      }
    } else if (position < this.pos) {
      seq.explorerAtPosition(position) match {
        case None    => None
        case Some(p) => Some(new InsertedIntSequenceExplorer(this, position, Some(p), false, false))
      }
    } else {
      seq.explorerAtPosition(position - 1) match {
        case None    => None
        case Some(p) => Some(new InsertedIntSequenceExplorer(this, position, Some(p), false, false))
      }
    }
  }

  override def contains(value: Int): Boolean = value == this.insertedValue || seq.contains(value)

  override def commitPendingMoves: IntSequence =
    seq.commitPendingMoves.insertAtPosition(insertedValue, pos, fast = false)

  override def isEmpty: Boolean = false

  override def valueAtPosition(position: Int): Option[Int] = {
    if (position == pos) Some(insertedValue)
    else if (position < pos) seq.valueAtPosition(position)
    else seq.valueAtPosition(position - 1)
  }
}
