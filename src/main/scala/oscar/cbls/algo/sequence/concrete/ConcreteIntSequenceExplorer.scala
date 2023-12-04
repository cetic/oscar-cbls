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

package oscar.cbls.algo.sequence.concrete

import oscar.cbls.algo.rb.RedBlackTreeMapExplorer
import oscar.cbls.algo.sequence.IntSequenceExplorer
import oscar.cbls.algo.sequence.affineFunction.Pivot

/** A [[ConcreteIntSequence]] explorer
  *
  * The position is the current external position. So to get it's value, we need to know the
  * internal position (using Pivot). To do that we keep track of the next and previous Pivot within
  * the sequence.
  *
  * Reminder : A pivot starts at a particular external position and ends when another Pivot starts
  *
  * @param sequence
  *   The [[ConcreteIntSequence]]
  * @param position
  *   The current EXTERNAL position
  * @param positionInRB
  *   The current internal position
  * @param currentPivotPosition
  *   The [[oscar.cbls.algo.sequence.affineFunction.Pivot]] impacting the current external position.
  *   [[scala.None]] or a [[oscar.cbls.algo.rb.RedBlackTreeMapExplorer]] of Pivot.
  * @param pivotAbovePosition
  *   The [[oscar.cbls.algo.sequence.affineFunction.Pivot]] after the current one. [[scala.None]] or
  *   a [[oscar.cbls.algo.rb.RedBlackTreeMapExplorer]] of Pivot.
  * @param limitAboveForCurrentPivot
  *   The maximal external position before switching to the next
  *   [[oscar.cbls.algo.sequence.affineFunction.Pivot]]
  * @param limitBelowForCurrentPivot
  *   The starting position of the current [[oscar.cbls.algo.sequence.affineFunction.Pivot]]
  * @param slopeIsPositive
  *   Whether or not the current [[oscar.cbls.algo.sequence.affineFunction.Pivot]] has a positive
  *   slope.
  */
class ConcreteIntSequenceExplorer(
  sequence: ConcreteIntSequence,
  override val position: Int,
  positionInRB: RedBlackTreeMapExplorer[Int],
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

  override def toString: String =
    s"ConcreteIntSequenceExplorer(position:$position value:$value currentPivotPosition:$currentPivotPosition pivotAbovePosition:$pivotAbovePosition positionInRB:$positionInRB)"

  override val value: Int = positionInRB.value

  private[sequence] def internalPos = positionInRB.key

  override def next: Option[IntSequenceExplorer] = {
    if (position == sequence.size - 1) return None
    // At end of current pivot &rarr; moving to next one
    if (position == limitAboveForCurrentPivot) {
      // Always a pivot, at least identity
      val newPivotAbovePosition = pivotAbovePosition.get.next
      val newPosition           = position + 1
      val newPositionInRBOpt =
        sequence.internalPositionToValue.positionOf(pivotAbovePosition.get.value.f(newPosition))
      newPositionInRBOpt match {
        case None => None
        case Some(newPositionInRB) =>
          Some(
            new ConcreteIntSequenceExplorer(
              sequence,
              newPosition,
              newPositionInRB,
              pivotAbovePosition,
              newPivotAbovePosition
            )(limitBelowForCurrentPivot = newPosition)
          )
      }
    } else {
      Some(
        new ConcreteIntSequenceExplorer(
          sequence,
          position + 1,
          if (slopeIsPositive) positionInRB.next.get else positionInRB.prev.get,
          currentPivotPosition,
          pivotAbovePosition
        )(limitAboveForCurrentPivot, limitBelowForCurrentPivot, slopeIsPositive)
      )
    }
  }

  override def prev: Option[IntSequenceExplorer] = {
    // Already at start of sequence &rarr; None
    if (position == 0) Some(new RootIntSequenceExplorer(sequence))
    // At start of current Pivot &rarr; moving to the previous one
    else if (position == limitBelowForCurrentPivot) {
      val newPosition             = position - 1
      val newCurrentPivotPosition = currentPivotPosition.get.prev
      val newInternalPosition = newCurrentPivotPosition match {
        case None            => newPosition
        case Some(position2) => position2.value.f(newPosition)
      }
      val newCurrentPositionInRB =
        sequence.internalPositionToValue.positionOf(newInternalPosition).get
      Some(
        new ConcreteIntSequenceExplorer(
          sequence,
          newPosition,
          newCurrentPositionInRB,
          newCurrentPivotPosition,
          currentPivotPosition
        )(limitAboveForCurrentPivot = limitBelowForCurrentPivot - 1)
      )
    } else {
      Some(
        new ConcreteIntSequenceExplorer(
          sequence,
          position - 1,
          if (slopeIsPositive) positionInRB.prev.get else positionInRB.next.get,
          currentPivotPosition,
          pivotAbovePosition
        )(limitAboveForCurrentPivot, limitBelowForCurrentPivot, slopeIsPositive)
      )
    }
  }
}
