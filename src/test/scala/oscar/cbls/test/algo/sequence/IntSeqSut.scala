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

package oscar.cbls.test.algo.sequence

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}

/** Mutable IntSequence used by the test bench as a SUT. */
private[sequence] class IntSeqSut(values: Iterable[Int]) {

  private var _intSeq: IntSequence = IntSequence(values)

  def intSeq: IntSequence = _intSeq

  def insertAfterPosition(
    value: Int,
    insertAfterPositionExplorer: IntSequenceExplorer,
    fast: Boolean
  ): Unit = {
    _intSeq = _intSeq.insertAfterPosition(value, insertAfterPositionExplorer, fast)
  }

  def remove(removePosAsExplorer: IntSequenceExplorer, fast: Boolean): Unit = {
    _intSeq = _intSeq.remove(removePosAsExplorer, fast: Boolean)
  }

  def flip(fast: Boolean): Unit = {
    _intSeq = _intSeq.flip(fast)
  }

  def moveAfter(
    fromIncludedExplorer: IntSequenceExplorer,
    toIncludedExplorer: IntSequenceExplorer,
    moveAfterExplorer: IntSequenceExplorer,
    flip: Boolean,
    fast: Boolean
  ): Unit = {
    _intSeq =
      _intSeq.moveAfter(fromIncludedExplorer, toIncludedExplorer, moveAfterExplorer, flip, fast)
  }

  def regularize(): Unit = {
    _intSeq = _intSeq.regularizeToMaxPivot(5)
  }

  def commitPendingMoves(): Unit = {
    _intSeq = _intSeq.commitPendingMoves
  }

  override def toString: String = _intSeq.toString
}
