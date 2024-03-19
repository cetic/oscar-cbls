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

package oscar.cbls.core.computation.seq

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.Invariant

object IdentitySeqInvariant {
  def apply(fromValue: SeqVariable, toValue: SeqVariable): IdentitySeqInvariant = {
    new IdentitySeqInvariant(fromValue, toValue)
  }
}

class IdentitySeqInvariant(fromValue: SeqVariable, toValue: SeqVariable)
    extends Invariant(fromValue.model)
    with SeqNotificationTarget {

  registerStaticallyListenedElement(fromValue)
  toValue.setDefiningInvariant(this)

  toValue := fromValue.value

  override def notifySeqChanges(v: SeqVariable, d: Int, changes: SeqUpdate): Unit = {
    assert(v == fromValue)
    digestChanges(changes)
  }

  private var checkPointStackNotTop: List[IntSequence] = List.empty

  private var topCheckpoint: IntSequence = null
  private var levelTopCheckpoint: Int    = -1

  private def popTopCheckpoint(): Unit = {
    checkPointStackNotTop match {
      case cp :: tail =>
        topCheckpoint = cp
        checkPointStackNotTop = tail
        assert(levelTopCheckpoint + 1 == checkPointStackNotTop.size)
        levelTopCheckpoint -= 1
      case _ =>
        topCheckpoint = null
        levelTopCheckpoint = -1
    }
  }

  private def pushTopCheckpoint(newCheckpoint: IntSequence): Unit = {
    if (topCheckpoint != null) {
      checkPointStackNotTop = topCheckpoint :: checkPointStackNotTop
    }
    topCheckpoint = newCheckpoint
    levelTopCheckpoint += 1
  }

  def digestChanges(changes: SeqUpdate): Unit = {
    changes match {
      case SeqUpdateInsert(
            value: Int,
            insertAfterPositionExplorer: IntSequenceExplorer,
            prev: SeqUpdate
          ) =>
        digestChanges(prev)
        toValue.insertAfterPosition(value, insertAfterPositionExplorer, Some(changes.newValue))
      case SeqUpdateMove(
            fromIncludedExplorer: IntSequenceExplorer,
            toIncludedExplorer: IntSequenceExplorer,
            afterExplorer: IntSequenceExplorer,
            flip: Boolean,
            prev: SeqUpdate
          ) =>
        digestChanges(prev)
        toValue.move(
          fromIncludedExplorer,
          toIncludedExplorer,
          afterExplorer,
          flip,
          Some(changes.newValue)
        )
      case SeqUpdateRemove(removePositionExplorer: IntSequenceExplorer, prev: SeqUpdate) =>
        digestChanges(prev)
        toValue.remove(removePositionExplorer, Some(changes.newValue))
      case SeqUpdateAssign(s) =>
        while (levelTopCheckpoint >= 0) {
          toValue.releaseTopCheckpoint()
          popTopCheckpoint()
        }
        toValue := s
      case SeqUpdateLastNotified(value: IntSequence) =>
        assert(value equals toValue.newValue)
      case SeqUpdateRollBackToCheckpoint(value: IntSequence, level: Int) =>
        // roll back might free some checkpoints implicitly
        while (level < levelTopCheckpoint) {
          toValue.releaseTopCheckpoint()
          popTopCheckpoint()
        }
        require(level == levelTopCheckpoint)
        require(
          value quickEquals topCheckpoint,
          s"fail on quick equals equals=${value.toList equals topCheckpoint.toList} value:$value topCheckpoint:$topCheckpoint"
        )
        toValue.rollbackToTopCheckpoint(value)
      case SeqUpdateDefineCheckpoint(prev: SeqUpdate, level: Int) =>
        digestChanges(prev)
        while (level <= levelTopCheckpoint) {
          toValue.releaseTopCheckpoint()
          popTopCheckpoint()
        }
        require(changes.newValue quickEquals prev.newValue)
        pushTopCheckpoint(changes.newValue)
        toValue.defineCurrentValueAsCheckpoint()

      case _ =>
      // Default case, do nothing
    }
  }

  override def checkInternals(): Unit = {
    require(
      toValue.newValue.toList equals fromValue.newValue.toList,
      Some(
        s"IdentitySeq: toValue.value=${toValue.value} should equals fromValue.value=${fromValue.value}"
      )
    )
  }
}
