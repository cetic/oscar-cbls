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

package oscar.cbls.algo.sequence

import oscar.cbls.algo.rb.RedBlackTreeMapExplorer
import oscar.cbls.algo.sequence.affineFunction.{Pivot, UnitaryAffineFunction}

/** A stacked explorer dedicated for [[MovedIntSequence]].
  *
  * It's used to explore a [[MovedIntSequence]]. It uses the original explorer and generates a new
  * one changing pivot.
  *
  * @param intSequence
  *   The [[InsertedIntSequence]]
  * @param position
  *   The current position in the sequence
  * @param explorerInOriginalSequence
  *   The original explorer, that is with the moved segment
  * @param currentPivotPosition
  *   The current [[oscar.cbls.algo.sequence.affineFunction.Pivot]] as an optional
  *   [[oscar.cbls.algo.rb.RedBlackTreeMapExplorer]]
  */
class MovedIntSequenceExplorer(
  intSequence: MovedIntSequence,
  override val position: Int,
  explorerInOriginalSequence: IntSequenceExplorer,
  currentPivotPosition: Option[RedBlackTreeMapExplorer[Pivot]]
) extends IntSequenceExplorer(intSequence) {

  private val pivotAbovePosition: Option[RedBlackTreeMapExplorer[Pivot]] =
    currentPivotPosition match {
      case None              => intSequence.localBijection.firstPivotAndPosition
      case Some(rbtExplorer) => rbtExplorer.next
    }

  private val currentPivot: Pivot = currentPivotPosition match {
    case None              => new Pivot(0, UnitaryAffineFunction.identity)
    case Some(rbtExplorer) => rbtExplorer.value
  }

  val limitAboveForCurrentPivot: Int = pivotAbovePosition match {
    case None                  => Int.MaxValue
    case Some(nextRbtExplorer) => nextRbtExplorer.value.fromValue - 1
  }
  val limitBelowForCurrentPivot: Int = currentPivot.fromValue
  val slopeIsPositive: Boolean       = !currentPivot.f.flip

  override val value: Int = explorerInOriginalSequence.value

  override def next: IntSequenceExplorer = {
    if (position == intSequence.size - 1) {
      new RootIntSequenceExplorer(intSequence, false)
    } else {
      if (position == limitAboveForCurrentPivot) {
        // Moving to next Pivot
        val newPosition = position + 1
        // We don't know the old position of the new starting pivot
        // We know that there is a next pivot, otherwise limitAboveForCurrentPivot would be
        // Int.MaxValue ==> end of the sequence ==> RootIntSequenceExplorer
        val newPositionInOriginalSequence =
          intSequence.originalExplorerAtPosition(pivotAbovePosition.get.value.f(newPosition)).get
        new MovedIntSequenceExplorer(
          intSequence,
          newPosition,
          newPositionInOriginalSequence,
          pivotAbovePosition
        )
      } else {
        // Staying on same pivot, using its slope to determine the next position in original sequence
        val newExplorerInOriginalSequence =
          if (slopeIsPositive) explorerInOriginalSequence.next
          else explorerInOriginalSequence.prev
        new MovedIntSequenceExplorer(
          intSequence,
          position + 1,
          newExplorerInOriginalSequence,
          currentPivotPosition
        )
      }
    }
  }

  override def prev: IntSequenceExplorer = {
    if (position == 0) {
      new RootIntSequenceExplorer(intSequence, true)
    } else if (position == limitBelowForCurrentPivot) {
      // Moving to previous Pivot
      val newPosition             = position - 1
      val newCurrentPivotPosition = currentPivotPosition.get.prev
      // Getting the original position corresponding to this position.
      val newInternalPosition = newCurrentPivotPosition match {
        case None                  => newPosition // If no pivot before ==> identity
        case Some(prevRbtExplorer) => prevRbtExplorer.value.f(newPosition)
      }

      new MovedIntSequenceExplorer(
        intSequence,
        newPosition,
        intSequence.originalExplorerAtPosition(newInternalPosition).get,
        newCurrentPivotPosition
      )
    } else {
      // Staying on same pivot, using its slope to determine the next position in original sequence
      new MovedIntSequenceExplorer(
        intSequence,
        position - 1,
        if (slopeIsPositive) explorerInOriginalSequence.prev
        else explorerInOriginalSequence.next,
        currentPivotPosition
      )
    }
  }
}
