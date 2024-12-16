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

  /** Creates a SeqUpdateDefineCheckpoint.
    *
    * Before creating the checkpoint, it checks if the value, an IntSequence, has to be regularized
    * or not.
    *
    * @param prev
    *   The previous update.
    * @param maxPivotPerValuePercent
    *   The maximum number of [[oscar.cbls.algo.sequence.affineFunction.Pivot]] per 100 value in the
    *   IntSequence. When defining a new checkpoint, if this value is exceeded, a regularization is
    *   done.
    * @param level
    *   The level of this checkpoint (starting at 0).
    * @return
    *   A SeqUpdateDefineCheckpoint.
    */
  def apply(
    prev: SeqUpdate,
    maxPivotPerValuePercent: Int,
    level: Int
  ): SeqUpdateDefineCheckpoint = {
    val doRegularize = level == 0
    val newPrev      = if (doRegularize) prev.regularize(maxPivotPerValuePercent) else prev
    new SeqUpdateDefineCheckpoint(newPrev, level)
  }

  /** Creates a SeqUpdateDefineCheckpoint.
    *
    * No regularization is done here. This implementation is supposed to be only used when we append
    * an already existing SeqUpdateDefineCheckpoint after another SeqUpdate.
    *
    * @param prev
    *   The previous update.
    * @param level
    *   The level of this checkpoint (starting at 0).
    * @return
    *   A SeqUpdateDefineCheckpoint.
    */
  def apply(prev: SeqUpdate, level: Int): SeqUpdateDefineCheckpoint = {
    new SeqUpdateDefineCheckpoint(prev, level)
  }

  /** Extracts the parameters of the given SeqUpdateDefineCheckpoint.
    *
    * @param seqUpdateDefineCheckpoint
    *   The SeqUpdateDefineCheckpoint we want to know the parameters of.
    * @return
    *   A tuple containing (The previous update and the level of this SeqUpdateDefineCheckpoint).
    */
  def unapply(seqUpdateDefineCheckpoint: SeqUpdateDefineCheckpoint): Option[(SeqUpdate, Int)] =
    Some(seqUpdateDefineCheckpoint.prev, seqUpdateDefineCheckpoint.level)
}

/** A SeqUpdate that defines a new checkpoint for the SeqVariable.
  *
  * @param prev
  *   The previous SeqUpdate of the batch.
  * @param level
  *   The level of this checkpoint (starting at 0).
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
