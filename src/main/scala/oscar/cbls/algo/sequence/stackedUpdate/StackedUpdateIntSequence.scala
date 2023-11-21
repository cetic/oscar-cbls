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
  override def delete(removePosAsExplorer: IntSequenceExplorer, fast: Boolean): IntSequence = {
    val pos = removePosAsExplorer.position
    require(pos >= 0 && pos < size, s"Remove position must be in [0, size=$size [. Got $pos")
    if (depth >= maxDepth) {
      new RemovedIntSequence(this, removePosAsExplorer, depth + 1).commitPendingMoves
    } else {
      new RemovedIntSequence(this, removePosAsExplorer, depth + 1)
    }
  }

  override def moveAfter(
                          fromIncludedExpl: IntSequenceExplorer,
                          toIncludedExpl: IntSequenceExplorer,
                          moveAfterExpl: Option[IntSequenceExplorer],
                          flip: Boolean,
                          fast: Boolean
  ): IntSequence = {
    val fromIncludedPos = fromIncludedExpl.position
    val toIncludedPos = toIncludedExpl.position
    val moveAfterPos = if(moveAfterExpl.nonEmpty)moveAfterExpl.get.position else -1
    require(
      fromIncludedPos >= 0 && fromIncludedPos < size,
      s"StartPositionIncluded should be in [0,sizeOfSequence=$size[. Got $fromIncludedPos"
    )
    require(
      toIncludedPos >= 0 && toIncludedPos < size,
      s"EndPositionIncluded should be in [0,sizeOfSequence=$size[. Got $toIncludedPos"
    )
    require(
      moveAfterPos >= -1 && moveAfterPos < size,
      s"MoveAfterPosition should be in [-1,sizeOfSequence=$size[. Got $moveAfterPos"
    )

    require(
      moveAfterPos < fromIncludedPos || moveAfterPos > toIncludedPos,
      s"MoveAfterPosition cannot be between startPositionIncluded and endPositionIncluded. " +
        s"Got $moveAfterPos (move), $fromIncludedPos (start) $toIncludedPos (end)"
    )
    require(
      fromIncludedPos <= toIncludedPos,
      s"StartPositionIncluded must be <= endPositionIncluded. Got $fromIncludedPos <= $toIncludedPos"
    )
    if (depth >= maxDepth) {
      new MovedIntSequence(
        this,
        fromIncludedExpl,
        toIncludedExpl,
        moveAfterExpl,
        flip,
        depth + 1
      ).commitPendingMoves
    } else {
      new MovedIntSequence(
        this,
        fromIncludedExpl,
        toIncludedExpl,
        moveAfterExpl,
        flip,
        depth + 1
      )
    }
  }

  override def insertAfterPosition(value: Int, insertAfterPositionExpl: Option[IntSequenceExplorer], fast: Boolean): IntSequence = {
    val insertAfterPos = IntSequenceExplorer.getPosOrElse(insertAfterPositionExpl,-1)
    require(
      insertAfterPos >= -1 && insertAfterPos <= size-1,
      s"Insert after position must be in [-1,sizeOfSequence minus 1=${size-1}]. Got ${insertAfterPos}"
    )
    if (depth >= maxDepth) {
      new InsertedIntSequence(this, value: Int, insertAfterPositionExpl, depth + 1).commitPendingMoves
    } else {
      new InsertedIntSequence(this, value: Int, insertAfterPositionExpl, depth + 1)
    }
  }

  override def regularizeToMaxPivot(
    maxPivotPerValuePercent: Int,
    targetToken: Token = this.token
  ): IntSequence =
    commitPendingMoves.regularizeToMaxPivot(maxPivotPerValuePercent, targetToken)

  override def regularize(targetToken: Token = this.token): IntSequence =
    commitPendingMoves.regularize(targetToken)

  def originalExplorerAtPosition(position: Int): Option[IntSequenceExplorer]
}
