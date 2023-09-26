package oscar.cbls.algo.sequence.stackedUpdate

import oscar.cbls.algo.sequence._

abstract class StackedUpdateIntSequence(depth: Int) extends IntSequence(depth = depth) {
  override def delete(pos: Int, fast: Boolean, autoRework: Boolean): IntSequence = {
    require(pos >= 0, "pos=" + pos + " for delete on UniqueIntSequence should be >= 0")
    require(pos < size, "cannot delete past end of sequence in UniqueIntSequence")
    if (depth >= 20) {
      new RemovedIntSequence(this, pos, depth + 1).commitPendingMoves
    } else {
      new RemovedIntSequence(this, pos, depth + 1)
    }
  }

  override def moveAfter(
                          startPositionIncluded: Int,
                          endPositionIncluded: Int,
                          moveAfterPosition: Int,
                          flip: Boolean,
                          fast: Boolean,
                          autoRework: Boolean
                        ): IntSequence = {
    require(
      startPositionIncluded >= 0 && startPositionIncluded < size,
      "startPositionIncluded=" + startPositionIncluded + " should be in [0,size" + size + "[ in UniqueIntSequence.moveAfter"
    )
    require(
      endPositionIncluded >= 0 && endPositionIncluded < size,
      "endPositionIncluded=" + endPositionIncluded + " should be in [0,size" + size + "[ in UniqueIntSequence.moveAfter"
    )
    require(
      moveAfterPosition >= -1 && moveAfterPosition < size,
      "moveAfterPosition=" + moveAfterPosition + " should be in [-1,size=" + size + "[ in UniqueIntSequence.moveAfter"
    )
    if (depth >= 20) {
      new MovedIntSequence(
        this,
        startPositionIncluded,
        endPositionIncluded,
        moveAfterPosition,
        flip,
        depth + 1
      ).commitPendingMoves
    } else {
      new MovedIntSequence(
        this,
        startPositionIncluded,
        endPositionIncluded,
        moveAfterPosition,
        flip,
        depth + 1
      )
    }
  }

  override def insertAtPosition(
                                 value: Int,
                                 pos: Int,
                                 fast: Boolean,
                                 autoRework: Boolean
                               ): IntSequence = {
    require(
      pos >= 0 && pos <= size,
      "pos=" + pos + " should be in [0,size=" + size + "] in IntSequence.insertAt"
    )
    if (depth >= 20) {
      new InsertedIntSequence(this, value: Int, pos: Int, depth + 1).commitPendingMoves
    } else {
      new InsertedIntSequence(this, value: Int, pos: Int, depth + 1)
    }
  }

  override def regularizeToMaxPivot(
                                     maxPivotPerValuePercent: Int,
                                     targetToken: Token = this.token
                                   ): ConcreteIntSequence =
    commitPendingMoves.regularizeToMaxPivot(maxPivotPerValuePercent, targetToken)

  override def regularize(targetToken: Token = this.token): ConcreteIntSequence =
    commitPendingMoves.regularize(targetToken)
}