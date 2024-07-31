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

import oscar.cbls.algo.sequence.affineFunction.PiecewiseUnitaryAffineFunction
import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer, MovedIntSequence}

object SeqUpdateMove {

  /** Returns the update corresponding to the move of a sub-sequence of nodes after a defined
    * position.
    *
    * The sub-sequence is defined by two IntSequenceExplorer as well as its new position.
    *
    * @param fromIncludedExplorer
    *   The IntSequenceExplorer at the starting position of the sub-sequence
    * @param toIncludedExplorer
    *   The IntSequenceExplorer at the ending position of the sub-sequence
    * @param afterExplorer
    *   The IntSequenceExplorer at the position after which we want to move the sub-sequence
    * @param flip
    *   If the sub-sequence needs to be flipped
    * @param prev
    *   The last update of the IntSequence
    * @return
    *   The update corresponding to the defined move
    */
  def apply(
    fromIncludedExplorer: IntSequenceExplorer,
    toIncludedExplorer: IntSequenceExplorer,
    afterExplorer: IntSequenceExplorer,
    flip: Boolean,
    prev: SeqUpdate
  ): SeqUpdateMove =
    new SeqUpdateMove(
      fromIncludedExplorer,
      toIncludedExplorer,
      afterExplorer,
      flip,
      prev,
      prev.newValue
        .moveAfter(fromIncludedExplorer, toIncludedExplorer, afterExplorer, flip, fast = true)
    )

  /** Returns the update corresponding to the move of a sub-sequence of nodes after a defined
    * position.
    *
    * The sub-sequence is defined by two IntSequenceExplorer as well as its new position.
    *
    * @param fromIncludedExplorer
    *   The IntSequenceExplorer at the starting position of the sub-sequence
    * @param toIncludedExplorer
    *   The IntSequenceExplorer at the ending position of the sub-sequence
    * @param afterExplorer
    *   The IntSequenceExplorer at the position after which we want to move the sub-sequence
    * @param flip
    *   If the sub-sequence needs to be flipped
    * @param prev
    *   The last update of the IntSequence
    * @param seqAfter
    *   The IntSequence value after the move
    * @return
    *   The update corresponding to the defined move
    */
  def apply(
    fromIncludedExplorer: IntSequenceExplorer,
    toIncludedExplorer: IntSequenceExplorer,
    afterExplorer: IntSequenceExplorer,
    flip: Boolean,
    prev: SeqUpdate,
    seqAfter: IntSequence
  ): SeqUpdate = {
    prev match {
      case u: SeqUpdateMove if u.prev.newValue sameIdentity seqAfter => u.prev
      case _ =>
        new SeqUpdateMove(
          fromIncludedExplorer,
          toIncludedExplorer,
          afterExplorer,
          flip,
          prev,
          seqAfter
        )
    }
  }

  /** Extracts the parameters of the SeqUpdateInsert
    *
    * @param seqUpdateMove
    *   The update we want to extracts the parameters from
    * @return
    *   The explorer starting and ending the sub-sequence, the explorer after which the sub-sequence
    *   is moved, if it was flip and the previous update
    */
  def unapply(
    seqUpdateMove: SeqUpdateMove
  ): Option[(IntSequenceExplorer, IntSequenceExplorer, IntSequenceExplorer, Boolean, SeqUpdate)] =
    Some(
      seqUpdateMove.fromIncludedExplorer,
      seqUpdateMove.toIncludedExplorer,
      seqUpdateMove.afterExplorer,
      seqUpdateMove.flip,
      seqUpdateMove.prev
    )
}

/** An IntSequence update, this update consists in moving and optionally flipping a sub-sequence of
  * nodes after a given position.
  *
  * The sub-sequence is defined by two included IntSequenceExplorer as well as its new position, to
  * ease the update of the potential Invariant depending on this IntSequence.
  *
  * @param fromIncludedExplorer
  *   The IntSequenceExplorer at the starting position of the sub-sequence
  * @param toIncludedExplorer
  *   The IntSequenceExplorer at the ending position of the sub-sequence
  * @param afterExplorer
  *   The IntSequenceExplorer at the position after which we want to move the sub-sequence
  * @param flip
  *   If the sub-sequence needs to be flipped
  * @param prev
  *   The last update of the IntSequence
  * @param seqAfter
  *   The IntSequence value after the move
  */
class SeqUpdateMove(
  val fromIncludedExplorer: IntSequenceExplorer,
  val toIncludedExplorer: IntSequenceExplorer,
  val afterExplorer: IntSequenceExplorer,
  val flip: Boolean,
  prev: SeqUpdate,
  seqAfter: IntSequence
) extends SeqUpdateWithPrev(prev, seqAfter) {

  assert(
    seqAfter equals prev.newValue
      .moveAfter(fromIncludedExplorer, toIncludedExplorer, afterExplorer, flip, fast = true),
    s"given seq=$seqAfter should be ${prev.newValue
        .moveAfter(fromIncludedExplorer, toIncludedExplorer, afterExplorer, flip, fast = true)}"
  )

  def movedValues: List[Int] =
    fromIncludedExplorer.forward.untilValue(toIncludedExplorer.value).map(_.value).toList

  override protected[computation] def reverseThis(
    newValueForThisAfterFullReverse: IntSequence,
    nextOp: SeqUpdate
  ): SeqUpdate = {
    val (intFromIncluded, intToIncluded) =
      if (flip) (toIncludedExplorer.position, fromIncludedExplorer.position)
      else (fromIncludedExplorer.position, toIncludedExplorer.position)

    prev.reverseThis(
      newValueForThisAfterFullReverse,
      SeqUpdateMove(
        seqAfter.explorerAtPosition(oldPosToNewPos(intFromIncluded).get).get,
        seqAfter.explorerAtPosition(oldPosToNewPos(intToIncluded).get).get,
        seqAfter.explorerAtPosition(oldPosToNewPos(fromIncludedExplorer.position - 1).get).get,
        flip,
        nextOp,
        prev.newValue
      )
    )
  }

  override protected[computation] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = {
    SeqUpdateMove(
      fromIncludedExplorer,
      toIncludedExplorer,
      afterExplorer,
      flip,
      prev.appendThisTo(previousUpdates),
      seqAfter: IntSequence
    )
  }

  private val localBijection: PiecewiseUnitaryAffineFunction = seqAfter match {
    case m: MovedIntSequence =>
      m.localBijection
    case _ =>
      MovedIntSequence.bijectionForMove(
        fromIncludedExplorer.position,
        toIncludedExplorer.position,
        afterExplorer.position,
        flip
      )
  }

  override def oldPosToNewPos(oldPos: Int): Option[Int] = {
    Some(
      MovedIntSequence.oldPosToNewPos(
        oldPos,
        fromIncludedExplorer.position,
        toIncludedExplorer.position,
        afterExplorer.position,
        flip: Boolean
      )
    )
  }

  override def newPos2OldPos(newPos: Int): Option[Int] =
    Some(localBijection(newPos))

  override protected[computation] def regularize(maxPivot: Int): SeqUpdate =
    SeqUpdateMove(
      fromIncludedExplorer,
      toIncludedExplorer,
      afterExplorer,
      flip,
      prev,
      seqAfter.regularizeToMaxPivot(maxPivot)
    )

  override def toString: String =
    s"SeqUpdateMove(fromIncluded:${fromIncludedExplorer.position} toIncluded:${toIncludedExplorer.position} after:${afterExplorer.position} flip:$flip prev:$prev)"
}
