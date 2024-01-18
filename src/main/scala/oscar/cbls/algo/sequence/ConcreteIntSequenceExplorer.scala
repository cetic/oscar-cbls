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
import affineFunction.{Pivot, UnitaryAffineFunction}

/** A [[ConcreteIntSequence]] explorer
  *
  * The position is the current external position. So to get it's value, we need to know the
  * internal position (using Pivot). To do that we keep track of the next and previous Pivot within
  * the sequence.
  *
  * Reminder : A pivot starts at a particular external position and ends when another Pivot starts
  *
  * @param intSequence
  *   The [[ConcreteIntSequence]]
  * @param position
  *   The current EXTERNAL position
  * @param positionInRB
  *   The current internal position
  * @param currentPivotPosition
  *   The [[affineFunction.Pivot]] impacting the current external position. [[scala.None]] or a
  *   [[oscar.cbls.algo.rb.RedBlackTreeMapExplorer]] of Pivot.
  */
class ConcreteIntSequenceExplorer(
  intSequence: ConcreteIntSequence,
  override val position: Int,
  positionInRB: RedBlackTreeMapExplorer[Int],
  currentPivotPosition: Option[RedBlackTreeMapExplorer[Pivot]]
) extends IntSequenceExplorer(intSequence) {

  override val value: Int = positionInRB.value

  private[sequence] def internalPos = positionInRB.key

  private val pivotAbovePosition: Option[RedBlackTreeMapExplorer[Pivot]] =
    currentPivotPosition match {
      case None              => intSequence.externalToInternalPosition.firstPivotAndPosition
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

  override def next: IntSequenceExplorer = {
    if (position == intSequence.size - 1) {
      new RootIntSequenceExplorer(intSequence, false)
    } else if (position == limitAboveForCurrentPivot) {
      // At end of current pivot &rarr; moving to next one
      // Always a pivot, at least identity
      val newPosition = position + 1
      val newPositionInRB =
        intSequence.internalPositionToValue
          .positionOf(pivotAbovePosition.get.value.f(newPosition))
          .get
      new ConcreteIntSequenceExplorer(intSequence, newPosition, newPositionInRB, pivotAbovePosition)
    } else {
      new ConcreteIntSequenceExplorer(
        intSequence,
        position + 1,
        if (slopeIsPositive) positionInRB.next.get else positionInRB.prev.get,
        currentPivotPosition
      )
    }
  }

  override def prev: IntSequenceExplorer = {
    // Already at start of sequence &rarr; None
    if (position == 0) {
      new RootIntSequenceExplorer(intSequence, true)
    }
    // At start of current Pivot &rarr; moving to the previous one
    else if (position == limitBelowForCurrentPivot) {
      val newPosition             = position - 1
      val newCurrentPivotPosition = currentPivotPosition.get.prev
      val newInternalPosition = newCurrentPivotPosition match {
        case None            => newPosition
        case Some(position2) => position2.value.f(newPosition)
      }
      val newCurrentPositionInRB =
        intSequence.internalPositionToValue.positionOf(newInternalPosition).get
      new ConcreteIntSequenceExplorer(
        intSequence,
        newPosition,
        newCurrentPositionInRB,
        newCurrentPivotPosition
      )
    } else {
      new ConcreteIntSequenceExplorer(
        intSequence,
        position - 1,
        if (slopeIsPositive) positionInRB.prev.get else positionInRB.next.get,
        currentPivotPosition
      )
    }
  }

  override def toString: String =
    s"ConcreteIntSequenceExplorer(position:$position value:$value currentPivotPosition:$currentPivotPosition pivotAbovePosition:$pivotAbovePosition positionInRB:$positionInRB)"
}
