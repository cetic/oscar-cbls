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
import oscar.cbls.algo.sequence.concrete.ConcreteIntSequence

/** Quick and stackable update of an [[IntSequence]]
  *
  * For performance purpose, it's easier to create several stackable updates and commit them times
  * to times. That's the purpose of the [[StackedUpdateIntSequence]]. That way you can easi
  *
  * @param depth
  *   The depth of the current update
  * @param maxDepth
  *   The maximum depth allowed before committing all updates and generate a new
  *   [[ConcreteIntSequence]]
  */
abstract class StackedUpdateIntSequence(depth: Int, maxDepth: Int = 20)
    extends IntSequence(depth = depth) {
  override def delete(pos: Int, fast: Boolean): IntSequence = {
    require(pos >= 0 && pos < size, s"Remove position must be in [0, size=$size [. Got $pos")
    if (depth >= maxDepth) {
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
    fast: Boolean
  ): IntSequence = {
    require(
      startPositionIncluded >= 0 && startPositionIncluded < size,
      s"StartPositionIncluded should be in [0,sizeOfSequence=$size[. Got $startPositionIncluded"
    )
    require(
      endPositionIncluded >= 0 && endPositionIncluded < size,
      s"EndPositionIncluded should be in [0,sizeOfSequence=$size[. Got $endPositionIncluded"
    )
    require(
      moveAfterPosition >= -1 && moveAfterPosition < size,
      s"MoveAfterPosition should be in [-1,sizeOfSequence=$size[. Got $moveAfterPosition"
    )

    require(
      moveAfterPosition < startPositionIncluded || moveAfterPosition > endPositionIncluded,
      s"MoveAfterPosition cannot be between startPositionIncluded and endPositionIncluded. " +
        s"Got $moveAfterPosition (move), $startPositionIncluded (start) $endPositionIncluded (end)"
    )
    require(
      startPositionIncluded <= endPositionIncluded,
      s"StartPositionIncluded must be <= endPositionIncluded. Got $startPositionIncluded <= $endPositionIncluded"
    )
    if (depth >= maxDepth) {
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

  override def insertAtPosition(value: Int, pos: Int, fast: Boolean): IntSequence = {
    require(
      pos >= 0 && pos <= size,
      s"Insertion position must be in [0,sizeOfSequence=$size]. Got $pos"
    )
    if (depth >= maxDepth) {
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
