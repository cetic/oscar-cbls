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

package oscar.cbls.core.computation.seq

/** Companion object of SeqUpdateDefineCheckpoint.
  */
object SeqUpdateDefineCheckpoint {

  def apply(
    prev: SeqUpdate,
    maxPivotPerValuePercent: Int,
    level: Int
  ): SeqUpdateDefineCheckpoint = {
    val doRegularize = level == 0
    val newPrev      = if (doRegularize) prev.regularize(maxPivotPerValuePercent) else prev
    new SeqUpdateDefineCheckpoint(newPrev, level)
  }

  def apply(prev: SeqUpdate, level: Int): SeqUpdateDefineCheckpoint = {
    new SeqUpdateDefineCheckpoint(prev, level)
  }

  def unapply(u: SeqUpdateDefineCheckpoint): Option[(SeqUpdate, Int)] = Some(u.prev, u.level)
}

/** A SeqUpdate that defines a new checkpoint for the SeqVariable.
  *
  * @param prev
  *   The previous SeqUpdate of the batch.
  * @param level
  *   The level of this checkpoint, starting at 0.
  */
class SeqUpdateDefineCheckpoint(prev: SeqUpdate, val level: Int)
    extends SeqUpdateWithPrev(prev, prev.newValue) {

  override protected[computation] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = {
    SeqUpdateDefineCheckpoint(prev.appendThisTo(previousUpdates), level)
  }

  protected[computation] def regularize(maxPivot: Int): SeqUpdate = this

  override def oldPosToNewPos(oldPos: Int): Option[Int] = {
    // SeqUpdateDefineCheckpoint does not modify the Sequence
    require(
      requirement = false,
      "SeqUpdateDefineCheckpoint should not be queried for delta on moves"
    )
    None
  }

  override def newPos2OldPos(newPos: Int): Option[Int] = {
    // SeqUpdateDefineCheckpoint does not modify the Sequence
    require(
      requirement = false,
      "SeqUpdateDefineCheckpoint should not be queried for delta on moves"
    )
    None
  }

  override def toString: String = s"SeqUpdateDefineCheckpoint(level:$level prev:$prev)"
}
