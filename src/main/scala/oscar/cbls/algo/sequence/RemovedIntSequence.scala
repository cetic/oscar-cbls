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

/** Quick and stackable update of an [[IntSequence]] applying a removal of an existing node.
  *
  * @param originalSequence
  *   The original sequence
  * @param explorerAtRemovePos
  *   The position of the node to remove
  * @param depth
  *   The depth of the current update
  */
class RemovedIntSequence(
  val originalSequence: IntSequence,
  val explorerAtRemovePos: IntSequenceExplorer,
  depth: Int
) extends StackedUpdateIntSequence(depth) {

  private val removedValue: Int = explorerAtRemovePos.value

  override def descriptorString: String =
    s"${originalSequence.descriptorString}.removed(pos:${explorerAtRemovePos.position} val:$removedValue)"

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
    if (position == -1) Some(new RootIntSequenceExplorer(this, true))
    else if (position == size) Some(new RootIntSequenceExplorer(this, false))
    else if (position == explorerAtRemovePos.position - 1)
      Some(new RemovedIntSequenceExplorer(this, position, explorerAtRemovePos.prev))
    else {
      originalSequence.explorerAtPosition(
        if (position < this.explorerAtRemovePos.position) position else position + 1
      ) match {
        case None => None
        case Some(e) =>
          e match {
            case r: RootIntSequenceExplorer =>
              Some(new RootIntSequenceExplorer(this, r.beforeStart))
            case _ => Some(new RemovedIntSequenceExplorer(this, position, e))
          }
      }
    }
  }

  override def positionsOfValue(value: Int): List[Int] = {
    val positionsBefore = originalSequence.positionsOfValue(value)
    positionsBefore.collect {
      case oldPos: Int if oldPos != explorerAtRemovePos.position =>
        if (oldPos > this.explorerAtRemovePos.position) oldPos - 1
        else oldPos
    }
  }

  /** Returns the new position of the specified one considering the removal */
  def oldPos2NewPos(oldPos: Int): Int = {
    if (oldPos < this.explorerAtRemovePos.position) oldPos else oldPos - 1
  }

  override def contains(value: Int): Boolean = {
    if (value == removedValue) originalSequence.nbOccurrence(value) > 1
    else originalSequence.contains(value)
  }

  override def commitPendingMoves: IntSequence =
    originalSequence.commitPendingMoves.remove(this.explorerAtRemovePos)

  override def valueAtPosition(position: Int): Option[Int] = {
    if (position >= this.explorerAtRemovePos.position)
      originalSequence.valueAtPosition(position + 1)
    else originalSequence.valueAtPosition(position)
  }
}
