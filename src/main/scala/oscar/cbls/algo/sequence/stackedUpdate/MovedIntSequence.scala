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

import oscar.cbls.algo.sequence._
import oscar.cbls.algo.sequence.affineFunction._
import oscar.cbls.algo.sequence.concrete.RootIntSequenceExplorer
import oscar.cbls.algo.sequence.stackedUpdate.MoveType._
import oscar.cbls.algo.sequence.stackedUpdate.PositionInMovement._

/** Companion object of [[MovedIntSequence]] * */
object MovedIntSequence {

  /** Creates the bijections corresponding to the movement passed as parameters.
    *   - forward : The node that is at x was before at y
    *   - backward (oldPosToNewPos) : the node that was before at y is at x
    *
    * @param fromIncluded
    *   The first (included) position of the moved segment
    * @param toIncluded
    *   The last (included) position of the moved segment
    * @param moveAfter
    *   The position after which the segment is moved
    * @param flip
    *   Whether or not the segment is flipped
    * @return
    *   The bijections as a
    *   [[oscar.cbls.algo.sequence.affineFunction.PiecewiseUnitaryAffineFunction]]
    */
  @inline
  def bijectionForMove(
    fromIncluded: Int,
    toIncluded: Int,
    moveAfter: Int,
    flip: Boolean
  ): PiecewiseUnitaryAffineFunction = {
    val moveType: MoveType = {
      if (moveAfter + 1 == fromIncluded) NotMoving
      else if (fromIncluded < moveAfter) Forward
      else Backward
    }
    (moveType, flip) match {
      case (NotMoving, false) => PiecewiseUnitaryAffineFunction.identity
      case (NotMoving, true) =>
        PiecewiseUnitaryAffineFunction.createFromPivots(
          List(
            // If fromIncluded == 0 this identity function will be overriden.
            new Pivot(0, UnitaryAffineFunction.identity),
            new Pivot(fromIncluded, UnitaryAffineFunction(toIncluded + fromIncluded, flip = true)),
            new Pivot(toIncluded + 1, UnitaryAffineFunction.identity)
          )
        )
      case (Forward, _) =>
        PiecewiseUnitaryAffineFunction.createFromPivots(
          List(
            new Pivot(
              fromIncluded,
              UnitaryAffineFunction(toIncluded + 1 - fromIncluded, flip = false)
            ),
            new Pivot(
              moveAfter + fromIncluded - toIncluded,
              UnitaryAffineFunction(
                if (flip) fromIncluded + moveAfter
                else toIncluded - moveAfter,
                flip
              )
            ),
            new Pivot(moveAfter + 1, UnitaryAffineFunction.identity)
          )
        )
      case (Backward, _) =>
        PiecewiseUnitaryAffineFunction.createFromPivots(
          List(
            new Pivot(
              moveAfter + 1,
              UnitaryAffineFunction(
                if (flip) toIncluded + moveAfter + 1
                else fromIncluded - moveAfter - 1,
                flip
              )
            ),
            new Pivot(
              moveAfter + toIncluded - fromIncluded + 2,
              UnitaryAffineFunction(fromIncluded - toIncluded - 1, flip = false)
            ),
            new Pivot(toIncluded + 1, UnitaryAffineFunction.identity)
          )
        )
    }
  }

  /** Returns the new position of a specified position considering the movement applied to the
    * sequence.
    *
    * @param oldPos
    *   The old position for which we want to know the new one
    * @param fromIncluded
    *   The first (included) position of the moved segment
    * @param toIncluded
    *   The last (included) position of the moved segment
    * @param after
    *   The position after which the segment was moved
    * @param flip
    *   Whether or not the segment was flipped
    * @return
    *   The new position of oldPos
    */
  @inline
  def oldPosToNewPos(
    oldPos: Int,
    fromIncluded: Int,
    toIncluded: Int,
    after: Int,
    flip: Boolean
  ): Int = {
    val moveType: MoveType = {
      if (after + 1 == fromIncluded) NotMoving
      else if (fromIncluded < after) Forward
      else Backward
    }

    // Inside == within the moved segment
    // between == Between after and moved segment
    // Outside == outside the moved segment and not between after and moved segment
    val oldPosRelativePosition: PositionInMovement = {
      if (oldPos >= fromIncluded && oldPos <= toIncluded) Inside
      else if (after < fromIncluded) {
        if (oldPos <= after || oldPos > toIncluded) Outside
        else Between
      } else {
        if (oldPos > after || oldPos < fromIncluded) Outside
        else Between
      }
    }

    (moveType, oldPosRelativePosition, flip) match {
      case (NotMoving, _, false)      => oldPos
      case (NotMoving, Between, true) => oldPos // shouldn't happen
      case (_, Outside, _)            => oldPos
      case (NotMoving, Inside, true)  => -oldPos + fromIncluded + toIncluded
      case (Forward, Inside, true)    => -oldPos + fromIncluded + after
      case (Forward, Inside, false)   => oldPos - toIncluded + after
      case (Forward, Between, _)      => oldPos + fromIncluded - toIncluded - 1
      case (Backward, Inside, true)   => -oldPos + toIncluded + after + 1
      case (Backward, Inside, false)  => oldPos - fromIncluded + after + 1
      case (Backward, Between, _)     => oldPos - fromIncluded + toIncluded + 1
    }
  }
}

/** Quick and stackable update of an [[IntSequence]] applying a movement on the existing node in the
  * sequence.
  *
  * Basically it's the move of a defined segment after another position. The segment can be flipped.
  *
  * @param seq
  *   The original sequence
  * @param fromIncludedExplorer
  *   The first (included) position of the moved segment
  * @param toIncludedExplorer
  *   The last (included) position of the moved segment
  * @param moveAfterExplorer
  *   The position after which the segment is moved
  * @param flip
  *   Whether or not the segment is flipped
  * @param depth
  *   The depth of the current update
  */
class MovedIntSequence(
  val seq: IntSequence,
  val fromIncludedExplorer: IntSequenceExplorer,
  val toIncludedExplorer: IntSequenceExplorer,
  val moveAfterExplorer: IntSequenceExplorer,
  val flip: Boolean,
  depth: Int
) extends StackedUpdateIntSequence(depth) {

  // The PiecewiseUnitaryAffineFunction corresponding to this move
  private val localBijection = MovedIntSequence.bijectionForMove(
    fromIncludedExplorer.position,
    toIncludedExplorer.position,
    moveAfterExplorer.position,
    flip
  )

  private val (
    originalExplorerBeforeFromPositionIncluded,
    originalExplorerAfterToPositionIncluded,
    originalExplorerAfterMoveAfterPosition
  ): (Option[IntSequenceExplorer], Option[IntSequenceExplorer], Option[IntSequenceExplorer]) =
    (fromIncludedExplorer.prev, toIncludedExplorer.next, moveAfterExplorer.next)

  override val size: Int = seq.size

  override def nbOccurrence(value: Int): Int = seq.nbOccurrence(value)

  override def unorderedContentNoDuplicate: List[Int] = seq.unorderedContentNoDuplicate

  override def unorderedContentNoDuplicateWithNBOccurrences: List[(Int, Int)] =
    seq.unorderedContentNoDuplicateWithNBOccurrences

  override def descriptorString: String =
    s"${seq.descriptorString}.moved(startPos:${fromIncludedExplorer.position} endPos:${toIncludedExplorer.position} targetPos:${moveAfterExplorer.position} flip:$flip)"

  override def commitPendingMoves: IntSequence = seq.commitPendingMoves.moveAfter(
    fromIncludedExplorer,
    toIncludedExplorer,
    moveAfterExplorer,
    flip
  )

  override def originalExplorerAtPosition(position: Int): Option[IntSequenceExplorer] = {
    if (position < 0) None
    else if (position == fromIncludedExplorer.position - 1)
      originalExplorerBeforeFromPositionIncluded
    else if (position == fromIncludedExplorer.position) Some(fromIncludedExplorer)
    else if (position == toIncludedExplorer.position) Some(toIncludedExplorer)
    else if (position == toIncludedExplorer.position + 1) originalExplorerAfterToPositionIncluded
    else if (moveAfterExplorer.position == position) Some(moveAfterExplorer)
    else if (moveAfterExplorer.position + 1 == position) originalExplorerAfterMoveAfterPosition
    else seq.explorerAtPosition(position)
  }

  override def explorerAtPosition(position: Int): Option[IntSequenceExplorer] = {
    if (position == -1) Some(new RootIntSequenceExplorer(this))
    else {
      val positionOfCurrentPivot = localBijection.pivotWithPositionApplyingTo(position)
      seq.explorerAtPosition(localBijection(position)) match {
        case None => None
        case Some(explorerInBasicSequence) =>
          Some(
            new MovedIntSequenceExplorer(
              this,
              position,
              explorerInBasicSequence,
              positionOfCurrentPivot,
              positionOfCurrentPivot match {
                case None    => localBijection.firstPivotAndPosition
                case Some(x) => x.next
              }
            )()
          )
      }
    }
  }

  // Returns the new position of the specified old position
  private def oldPosToNewPos(oldPos: Int): Int = {
    val tmp = MovedIntSequence.oldPosToNewPos(
      oldPos,
      fromIncludedExplorer.position,
      toIncludedExplorer.position,
      moveAfterExplorer.position,
      flip
    )
    assert(
      tmp == localBijection.backward(oldPos),
      s"oldPosToNewPos method got $tmp. Should be : ${localBijection.backward(oldPos)}"
    )
    tmp
  }

  override def positionsOfValue(value: Int): List[Int] = {
    val positionsBefore = seq.positionsOfValue(value)
    positionsBefore.map(oldPos => oldPosToNewPos(oldPos))
  }

  override def contains(value: Int): Boolean = seq.contains(value)

  override def isEmpty: Boolean = seq.isEmpty

  override def valueAtPosition(position: Int): Option[Int] = {
    seq.valueAtPosition(localBijection(position))
  }
}

/* Describes the movement */
sealed abstract class MoveType
object MoveType {
  case object NotMoving extends MoveType
  case object Forward   extends MoveType
  case object Backward  extends MoveType
}

/* Describes the position of a node relatively to the movement */
sealed abstract class PositionInMovement
object PositionInMovement {
  /* Inside the moved segment */
  case object Inside extends PositionInMovement
  /* Between the moved segment and the moveAfter position */
  case object Between extends PositionInMovement
  /* Before or after the moved segment and the moveAfter position */
  case object Outside extends PositionInMovement
}
