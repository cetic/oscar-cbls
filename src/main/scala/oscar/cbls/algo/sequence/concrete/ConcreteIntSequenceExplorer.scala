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
    if (position == limitAboveForCurrentPivot) {
      // change pivot, we are also sure that there is a next, so use .head
      val newPivotAbovePosition = pivotAbovePosition.head.next
      val newPosition           = position + 1
      val newPositionInRBOpt =
        sequence.internalPositionToValue.positionOf(pivotAbovePosition.head.value.f(newPosition))
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
      // do not change pivot

      (if (slopeIsPositive) positionInRB.next else positionInRB.prev) match {
        case None => None
        case Some(newPositionInRB) =>
          Some(
            new ConcreteIntSequenceExplorer(
              sequence,
              position + 1,
              newPositionInRB,
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
      // change pivot

      val newPosition             = position - 1
      val newCurrentPivotPosition = currentPivotPosition.head.prev
      val newInternalPosition = newCurrentPivotPosition match {
        case None            => newPosition
        case Some(position2) => position2.value.f(newPosition)
      }
      val newCurrentPositionInRB =
        sequence.internalPositionToValue.positionOf(newInternalPosition).head
      // println("change pivot newPosition:" + newPosition + " newCurrentPivotPosition:" + newCurrentPivotPosition + " oldPosition:" + currentPivotPosition)
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
      // do not change pivot
      // println("not change pivot")
      Some(
        new ConcreteIntSequenceExplorer(
          sequence,
          position - 1,
          if (slopeIsPositive) positionInRB.prev.head else positionInRB.next.head,
          currentPivotPosition,
          pivotAbovePosition
        )(limitAboveForCurrentPivot, limitBelowForCurrentPivot, slopeIsPositive)
      )
    }
  }
}
