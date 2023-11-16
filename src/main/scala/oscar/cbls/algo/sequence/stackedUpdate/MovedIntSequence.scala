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
import oscar.cbls.algo.sequence.stackedUpdate.MoveType._
import oscar.cbls.algo.sequence.stackedUpdate.PositionInMovement._

/** Companion object of [[MovedIntSequence]] * */
object MovedIntSequence {

  /** Creates the bijections corresponding to the movement passed as parameters.
    *   - forward : The node that is at x was before at y
    *   - backward (oldPosToNewPos) : the node that was before at y is at x
    *
    * @param fromIncluded
    *   The first (included) position of the moved segment as an [[Int]]
    * @param toIncluded
    *   The last (included) position of the moved segment as an [[Int]]
    * @param moveAfter
    *   The position after which the segment is moved as an [[Int]]
    * @param flip
    *   Whether or not the segment is flipped as a [[Boolean]]
    * @return
    *   The bijections as a [[PiecewiseUnitaryAffineFunction]]
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
            // If fromIncluded == 0 this identity function will be override.
            new Pivot(0, UnitaryAffineFunction.identity),
            new Pivot(fromIncluded, UnitaryAffineFunction(toIncluded + fromIncluded, true)),
            new Pivot(toIncluded + 1, UnitaryAffineFunction.identity)
          )
        )
      case (Forward, _) =>
        PiecewiseUnitaryAffineFunction.createFromPivots(
          List(
            new Pivot(fromIncluded, UnitaryAffineFunction(toIncluded + 1 - fromIncluded, false)),
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
              UnitaryAffineFunction(fromIncluded - toIncluded - 1, false)
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
    *   The old position for which we want to know the new one as an [[Int]]
    * @param fromIncluded
    *   The first (included) position of the moved segment as an [[Int]]
    * @param toIncluded
    *   The last (included) position of the moved segment as an [[Int]]
    * @param after
    *   The position after which the segment was moved as an [[Int]]
    * @param flip
    *   Whether or not the segment was flipped as a [[Boolean]]
    * @return
    *   The new position of oldPos as an [[Int]]
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
      case (NotMoving, _, false)       => oldPos
      case (_, Outside, _)             => oldPos
      case (NotMoving, Inside, true) => -oldPos + fromIncluded + toIncluded
      case (Forward, Inside, true)   => -oldPos + fromIncluded + after
      case (Forward, Inside, false)  => oldPos - toIncluded + after
      case (Forward, Between, _)     => oldPos + fromIncluded - toIncluded - 1
      case (Backward, Inside, true)  => -oldPos + toIncluded + after + 1
      case (Backward, Inside, false) => oldPos - fromIncluded + after + 1
      case (Backward, Between, _)    => oldPos - fromIncluded + toIncluded + 1
    }
  }
}

/** Quick and stackable update of an [[IntSequence]] applying a movement on the existing node in the
  * sequence.
  *
  * Basically it's the move of a defined segment after another position. The segment can be flipped.
  *
  * @param seq
  *   The original sequence as a [[IntSequence]]
  * @param fromPositionIncluded
  *   The first (included) position of the moved segment as an [[Int]]
  * @param toPositionIncluded
  *   The last (included) position of the moved segment as an [[Int]]
  * @param moveAfterPosition
  *   The position after which the segment is moved as an [[Int]]
  * @param flip
  *   Whether or not the segment is flipped as an [[Int]]
  * @param depth
  *   The depth of the current update
  */
class MovedIntSequence(
  val seq: IntSequence,
  val fromPositionIncluded: Int,
  val toPositionIncluded: Int,
  val moveAfterPosition: Int,
  val flip: Boolean,
  depth: Int
) extends StackedUpdateIntSequence(depth) {

  // TODO: provide a cache on the values at the boundary of the move

  // The PiecewiseUnitaryAffineFunction corresponding to this move
  private val localBijection = MovedIntSequence.bijectionForMove(
    fromPositionIncluded,
    toPositionIncluded,
    moveAfterPosition,
    flip
  )

  private val (
    originalExplorerBeforeFromPositionIncluded,
    originalExplorerAtFromPositionIncluded,
    originalExplorerAtToPositionIncluded,
    originalExplorerAfterToPositionIncluded,
    originalExplorerAtMoveAfterPosition,
    originalExplorerAfterMoverAfterPosition
  ): (
    Option[IntSequenceExplorer],
    Option[IntSequenceExplorer],
    Option[IntSequenceExplorer],
    Option[IntSequenceExplorer],
    Option[IntSequenceExplorer],
    Option[IntSequenceExplorer]
  ) = {
    // Can't be None
    val originExplorerAtFromIncluded = seq.explorerAtPosition(fromPositionIncluded)
    // Could be None
    val originExplorerBeforeFromIncluded = originExplorerAtFromIncluded.get.prev
    // Can't be None
    val originExplorerAtToIncluded = {
      if (toPositionIncluded - fromPositionIncluded < Math.log(seq.size)) {
        originExplorerAtFromIncluded.get.untilPosition(toPositionIncluded)
      } else {
        seq.explorerAtPosition(toPositionIncluded)
      }
    }
    // Could be None
    val originExplorerAfterToIncluded = originExplorerAtToIncluded.get.next
    // Could be None if moving at start (moveAfterPosition == -1)
    val originExplorerAtMoveAfter = {
      if (moveAfterPosition == -1) None
      else if (
        moveAfterPosition < fromPositionIncluded && fromPositionIncluded - moveAfterPosition < Math
          .log(seq.size)
      ) originExplorerAtFromIncluded.get.untilPosition(moveAfterPosition)
      else if (
        moveAfterPosition > toPositionIncluded && moveAfterPosition - toPositionIncluded < Math.log(
          seq.size
        )
      ) originExplorerAfterToIncluded.get.untilPosition(moveAfterPosition)
      else seq.explorerAtPosition(moveAfterPosition)
    }
    // Could be None
    val originExplorerAtAfterMoveAfter =
      if (originExplorerAtMoveAfter.nonEmpty) originExplorerAtMoveAfter.get.next
      else if (moveAfterPosition == -1) seq.explorerAtPosition(0)
      else None
    (
      originExplorerBeforeFromIncluded,
      originExplorerAtFromIncluded,
      originExplorerAtToIncluded,
      originExplorerAfterToIncluded,
      originExplorerAtMoveAfter,
      originExplorerAtAfterMoveAfter
    )
  }

  override val size: Int = seq.size

  override def nbOccurrence(value: Int): Int = seq.nbOccurrence(value)

  override def unorderedContentNoDuplicate: List[Int] = seq.unorderedContentNoDuplicate

  override def unorderedContentNoDuplicateWithNBOccurrences: List[(Int, Int)] =
    seq.unorderedContentNoDuplicateWithNBOccurrences

  override def descriptorString: String =
    s"${seq.descriptorString}.moved(startPos:$fromPositionIncluded endPos:$toPositionIncluded targetPos:$moveAfterPosition flip:$flip)"

  override def commitPendingMoves: IntSequence = seq.commitPendingMoves.moveAfter(
    fromPositionIncluded,
    toPositionIncluded,
    moveAfterPosition,
    flip,
    fast = false
  )

  override def originalExplorerAtPosition(position: Int): Option[IntSequenceExplorer] = {
    if (position == fromPositionIncluded - 1) originalExplorerBeforeFromPositionIncluded
    else if (position == fromPositionIncluded) originalExplorerAtFromPositionIncluded
    else if (position == toPositionIncluded) originalExplorerAtToPositionIncluded
    else if (position == toPositionIncluded + 1) originalExplorerAfterToPositionIncluded
    else if (position == moveAfterPosition) originalExplorerAtMoveAfterPosition
    else if (position == moveAfterPosition + 1) originalExplorerAfterMoverAfterPosition
    else seq.explorerAtPosition(position)
  }

  override def explorerAtPosition(position: Int): Option[IntSequenceExplorer] = {
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

  // Returns the new position of the specified old position
  private def oldPosToNewPos(oldPos: Int): Int = {
    val tmp = MovedIntSequence.oldPosToNewPos(
      oldPos,
      fromPositionIncluded,
      toPositionIncluded,
      moveAfterPosition,
      flip
    )
    assert(
      tmp == localBijection.backward(oldPos),
      "oldPosToNewPos method got " + tmp + ". Should be : " + localBijection.backward(oldPos)
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
  case object Forward extends MoveType
  case object Backward extends MoveType
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