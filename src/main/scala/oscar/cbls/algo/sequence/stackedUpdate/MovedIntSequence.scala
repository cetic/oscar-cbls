package oscar.cbls.algo.sequence.stackedUpdate

import oscar.cbls.algo.rb.RedBlackTreeMapExplorer
import oscar.cbls.algo.sequence._
import oscar.cbls.algo.sequence.affineFunction.{
  PiecewiseUnitaryAffineFunction,
  Pivot,
  UnitaryAffineFunction
}

object MovedIntSequence {

  @inline
  def bijectionForMove(
    startPositionIncluded: Int,
    endPositionIncluded: Int,
    moveAfterPosition: Int,
    flip: Boolean
  ): PiecewiseUnitaryAffineFunction = {
    if (moveAfterPosition + 1 == startPositionIncluded) {
      // not moving
      if (flip) {
        // just flipping
        if (startPositionIncluded == 0) {
          PiecewiseUnitaryAffineFunction.createFromPivots(
            List(
              new Pivot(0, new UnitaryAffineFunction(endPositionIncluded, true)),
              new Pivot(endPositionIncluded + 1, UnitaryAffineFunction.identity)
            )
          )
        } else {
          PiecewiseUnitaryAffineFunction.createFromPivots(
            List(
              new Pivot(0, UnitaryAffineFunction.identity),
              new Pivot(
                startPositionIncluded,
                new UnitaryAffineFunction(endPositionIncluded + startPositionIncluded, true)
              ),
              new Pivot(endPositionIncluded + 1, UnitaryAffineFunction.identity)
            )
          )
        }

      } else {
        // nop
        PiecewiseUnitaryAffineFunction.identity
      }
    } else {
      if (moveAfterPosition > startPositionIncluded) {
        // move upwards
        PiecewiseUnitaryAffineFunction.createFromPivots(
          List(
            new Pivot(
              startPositionIncluded,
              UnitaryAffineFunction(endPositionIncluded + 1 - startPositionIncluded, false)
            ),
            new Pivot(
              moveAfterPosition + startPositionIncluded - endPositionIncluded,
              UnitaryAffineFunction(
                if (flip) startPositionIncluded + moveAfterPosition
                else endPositionIncluded - moveAfterPosition,
                flip
              )
            ),
            new Pivot(moveAfterPosition + 1, UnitaryAffineFunction.identity)
          )
        )
      } else {
        // move downwards
        PiecewiseUnitaryAffineFunction.createFromPivots(
          List(
            new Pivot(
              moveAfterPosition + 1,
              UnitaryAffineFunction(
                if (flip) endPositionIncluded + moveAfterPosition + 1
                else startPositionIncluded - moveAfterPosition - 1,
                flip
              )
            ),
            new Pivot(
              moveAfterPosition + endPositionIncluded - startPositionIncluded + 2,
              UnitaryAffineFunction(startPositionIncluded - endPositionIncluded - 1, false)
            ),
            new Pivot(endPositionIncluded + 1, UnitaryAffineFunction.identity)
          )
        )
      }
    }
  }

  @inline
  def oldPosToNewPos(
    oldPos: Int,
    fromIncluded: Int,
    toIncluded: Int,
    after: Int,
    flip: Boolean
  ): Int = {
    // println("oldPosToNewPos(oldPos:"  + oldPos + " fromIncluded:" + fromIncluded + " toIncluded:" + toIncluded + " after:" + after +" flip:" + flip + ")")

    if (after + 1 == fromIncluded && !flip) oldPos
    else if (after + 1 == fromIncluded && flip) {
      if (oldPos < fromIncluded || oldPos > toIncluded) oldPos
      else fromIncluded + toIncluded - oldPos
    } else if (fromIncluded < after) {
      // println("move upwards")
      if (oldPos < fromIncluded || oldPos > after) oldPos
      else if (oldPos <= toIncluded) {
        // println("in the moved segment")
        if (flip) {
          fromIncluded - oldPos + after
        } else {
          oldPos + after - toIncluded
        }
      } else {
        // println("not in the moved segment")
        oldPos - toIncluded + fromIncluded - 1
      }
    } else {
      // println("move downwards")
      if (oldPos <= after || oldPos > toIncluded) oldPos
      else if (oldPos < fromIncluded) {
        // println("not in the moved segment")
        oldPos + toIncluded - fromIncluded + 1
      } else {
        // println("in the moved segment")
        if (flip) {
          after + 1 + toIncluded - oldPos
        } else {
          oldPos + after - fromIncluded + 1
        }
      }
    }
  }
}

class MovedIntSequence(
  val seq: IntSequence,
  val startPositionIncluded: Int,
  val endPositionIncluded: Int,
  val moveAfterPosition: Int,
  val flip: Boolean,
  depth: Int
) extends StackedUpdateIntSequence(depth) {

  // TODO: provide a cache on the values at the boundary of the move

  override def unorderedContentNoDuplicate: List[Int] = seq.unorderedContentNoDuplicate

  override def unorderedContentNoDuplicateWithNBOccurrences: List[(Int, Int)] =
    seq.unorderedContentNoDuplicateWithNBOccurrences

  override def descriptorString: String =
    s"${seq.descriptorString}.moved(startPos:$startPositionIncluded endPos:$endPositionIncluded targetPos:$moveAfterPosition flip:$flip)"

  val localBijection = MovedIntSequence.bijectionForMove(
    startPositionIncluded,
    endPositionIncluded,
    moveAfterPosition,
    flip
  )

  override val size: Int = seq.size

  override def nbOccurrence(value: Int): Int = seq.nbOccurrence(value)

  override def commitPendingMoves: IntSequence = seq.commitPendingMoves.moveAfter(
    startPositionIncluded,
    endPositionIncluded,
    moveAfterPosition,
    flip,
    fast = false,
    autoRework = false
  )

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

  def oldPosToNewPos(oldPos: Int): Int = {
    val tmp = MovedIntSequence.oldPosToNewPos(
      oldPos,
      startPositionIncluded,
      endPositionIncluded,
      moveAfterPosition,
      flip
    )
    assert(
      tmp == localBijection.backward(oldPos),
      "oldPosToNewPos got" + tmp + " expected " + localBijection.backward(oldPos)
    )
    tmp
  }

  override def positionsOfValue(value: Int): List[Int] = {
    var positionsBefore     = seq.positionsOfValue(value)
    var toReturn: List[Int] = null
    while (positionsBefore != null) {
      val oldPos = positionsBefore.head
      positionsBefore = positionsBefore.tail
      val newPos = oldPosToNewPos(oldPos)
      toReturn = List(newPos) ::: toReturn
    }
    toReturn
  }

  override def contains(value: Int): Boolean = seq.contains(value)

  override def isEmpty: Boolean = seq.isEmpty

  override def valueAtPosition(position: Int): Option[Int] = {
    seq.valueAtPosition(localBijection(position))
  }
}
