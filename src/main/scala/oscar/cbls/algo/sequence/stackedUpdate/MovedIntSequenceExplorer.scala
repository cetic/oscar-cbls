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

import oscar.cbls.algo.rb.RedBlackTreeMapExplorer
import oscar.cbls.algo.sequence.IntSequenceExplorer
import oscar.cbls.algo.sequence.affineFunction.Pivot

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
  *   The current [[Pivot]] as an optional [[RedBlackTreeMapExplorer]]
  * @param pivotAbovePosition
  *   The next [[Pivot]] as an optional [[RedBlackTreeMapExplorer]]
  */
class MovedIntSequenceExplorer(
  intSequence: MovedIntSequence,
  override val position: Int,
  explorerInOriginalSequence: IntSequenceExplorer,
  currentPivotPosition: Option[RedBlackTreeMapExplorer[Pivot]],
  pivotAbovePosition: Option[RedBlackTreeMapExplorer[Pivot]]
)(
  limitAboveForCurrentPivot: Int = pivotAbovePosition match {
    case None    => Int.MaxValue
    case Some(p) => p.value.fromValue - 1
  },
  limitBelowForCurrentPivot: Int = currentPivotPosition match {
    case None    => Int.MinValue
    case Some(p) => p.value.fromValue
  },
  slopeIsPositive: Boolean = currentPivotPosition match {
    case None    => true
    case Some(p) => !p.value.f.flip
  }
) extends IntSequenceExplorer {

  override val value: Int = explorerInOriginalSequence.value

  override def next: Option[IntSequenceExplorer] = {
    if (position == intSequence.size - 1) return None
    if (position == limitAboveForCurrentPivot) {
      // Moving to next Pivot
      val newPivotAbovePosition = pivotAbovePosition.get.next
      val newPosition           = position + 1
      // We don't know the old position of the new starting pivot
      val newPositionInBasicSequence =
        intSequence.seq.explorerAtPosition(pivotAbovePosition.get.value.f(newPosition))
      newPositionInBasicSequence match {
        case None => None
        case Some(newPositionInOS) =>
          Some(
            new MovedIntSequenceExplorer(
              intSequence,
              newPosition,
              newPositionInOS,
              pivotAbovePosition,
              newPivotAbovePosition
            )(limitBelowForCurrentPivot = newPosition)
          )
      }
    } else {
      // Staying on same pivot, using its slope to determine the next position in original sequence
      (if (slopeIsPositive) explorerInOriginalSequence.next
       else explorerInOriginalSequence.prev) match {
        case None => None
        case Some(newPositionInOS) =>
          Some(
            new MovedIntSequenceExplorer(
              intSequence,
              position + 1,
              newPositionInOS,
              currentPivotPosition,
              pivotAbovePosition
            )(limitAboveForCurrentPivot, limitBelowForCurrentPivot, slopeIsPositive)
          )
      }
    }
  }

  override def prev: Option[IntSequenceExplorer] = {
    if (position == 0) None
    else if (position == limitBelowForCurrentPivot) {
      // Moving to previous Pivot
      val newPosition             = position - 1
      val newCurrentPivotPosition = currentPivotPosition.get.prev
      // Getting the original position corresponding to this position.
      val newInternalPosition = newCurrentPivotPosition match {
        case None                        => newPosition
        case Some(previousPivotExplorer) => previousPivotExplorer.value.f(newPosition)
      }

      Some(
        new MovedIntSequenceExplorer(
          intSequence,
          newPosition,
          intSequence.seq.explorerAtPosition(newInternalPosition).get,
          newCurrentPivotPosition,
          currentPivotPosition
        )(limitAboveForCurrentPivot = limitBelowForCurrentPivot - 1)
      )
    } else {
      // Staying on same pivot, using its slope to determine the next position in original sequence
      Some(
        new MovedIntSequenceExplorer(
          intSequence,
          position - 1,
          if (slopeIsPositive) explorerInOriginalSequence.prev.get
          else explorerInOriginalSequence.next.get,
          currentPivotPosition,
          pivotAbovePosition
        )(limitAboveForCurrentPivot, limitBelowForCurrentPivot, slopeIsPositive)
      )
    }
  }
}
