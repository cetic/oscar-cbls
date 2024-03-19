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
    * @param seq
    *   The IntSequence value after the move (if known)
    * @return
    *   The update corresponding to the defined move
    */
  def apply(
    fromIncludedExplorer: IntSequenceExplorer,
    toIncludedExplorer: IntSequenceExplorer,
    afterExplorer: IntSequenceExplorer,
    flip: Boolean,
    prev: SeqUpdate,
    seq: IntSequence
  ): SeqUpdate = {
    prev match {
      case u: SeqUpdateMove if u.prev.newValue quickEquals seq => u.prev
      case _ =>
        new SeqUpdateMove(fromIncludedExplorer, toIncludedExplorer, afterExplorer, flip, prev, seq)
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
  * @param seq
  *   The IntSequence value after the move (if known)
  */
class SeqUpdateMove(
  val fromIncludedExplorer: IntSequenceExplorer,
  val toIncludedExplorer: IntSequenceExplorer,
  val afterExplorer: IntSequenceExplorer,
  val flip: Boolean,
  prev: SeqUpdate,
  seq: IntSequence
) extends SeqUpdateWithPrev(prev, seq) {

  assert(
    seq equals prev.newValue
      .moveAfter(fromIncludedExplorer, toIncludedExplorer, afterExplorer, flip, fast = true),
    s"given seq=$seq should be ${prev.newValue
        .moveAfter(fromIncludedExplorer, toIncludedExplorer, afterExplorer, flip, fast = true)}"
  )

  def isSimpleFlip: Boolean       = afterExplorer.next == fromIncludedExplorer && flip
  def isNop: Boolean              = afterExplorer.next == fromIncludedExplorer && !flip
  def fromValue: Int              = fromIncludedExplorer.value
  def toValue: Int                = toIncludedExplorer.value
  def afterValue: Int             = afterExplorer.value
  def moveDownwards: Boolean      = fromIncludedExplorer.position > afterExplorer.position
  def moveUpwards: Boolean        = fromIncludedExplorer.position < afterExplorer.position
  def nbPointsInMovedSegment: Int = toIncludedExplorer.position - fromIncludedExplorer.position + 1

  def movedValuesQList: List[Int] =
    fromIncludedExplorer.forward.untilValue(toValue).map(_.value).toList

  override protected[computation] def reverseThis(
    newValueForThisAfterFullReverse: IntSequence,
    nextOp: SeqUpdate
  ): SeqUpdate = {
    val (intFromIncluded, intToIncluded) =
      if (flip) (toIncludedExplorer.position, fromIncludedExplorer.position)
      else (fromIncludedExplorer.position, toIncludedExplorer.position)

    // TODO : find a better way to do it,
    prev.reverseThis(
      newValueForThisAfterFullReverse,
      SeqUpdateMove(
        seq.explorerAtPosition(oldPosToNewPos(intFromIncluded).get).get,
        seq.explorerAtPosition(oldPosToNewPos(intToIncluded).get).get,
        seq.explorerAtPosition(oldPosToNewPos(fromIncludedExplorer.position - 1).get).get,
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
      seq: IntSequence
    )
  }

  override protected[computation] def explicitHowToRollBack(): SeqUpdate = {
    SeqUpdateMove(
      fromIncludedExplorer,
      toIncludedExplorer,
      afterExplorer,
      flip,
      prev.explicitHowToRollBack(),
      seq: IntSequence
    )
  }

  private val localBijection: PiecewiseUnitaryAffineFunction = seq match {
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
      seq.regularizeToMaxPivot(maxPivot)
    )

  override def toString: String =
    s"SeqUpdateMove(fromIncluded:$fromIncludedExplorer toIncluded:$toIncludedExplorer after:$afterExplorer flip:$flip prev:$prev)"
}
