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
import oscar.cbls.core.computation.{Invariant, Store}
import oscar.cbls.core.propagation.PropagationStructure

object SeqIdentityInvariant {
  def apply(store: Store, fromValue: SeqVariable, toValue: SeqVariable): SeqIdentityInvariant = {
    new SeqIdentityInvariant(store, fromValue, toValue)
  }
}

class SeqIdentityInvariant(store: Store, fromValue: SeqVariable, toValue: SeqVariable)
    extends Invariant(store)
    with SeqNotificationTarget {

  registerStaticallyListenedElement(fromValue)
  fromValue.registerDynamicallyListeningElement(this)
  toValue.setDefiningInvariant(this)

  toValue := fromValue.value

  override def notifySeqChanges(v: SeqVariable, d: Int, changes: SeqUpdate): Unit = {
    assert(v == fromValue)
    digestChanges(changes)
  }

  private var checkPointStack: List[IntSequence] = List.empty

  private var levelTopCheckpoint: Int = -1

  private def popTopCheckpoint(): Unit = {
    checkPointStack match {
      case _ :: tail =>
        assert(
          levelTopCheckpoint + 1 == checkPointStack.size,
          s"'${levelTopCheckpoint + 1} vs ${checkPointStack.size}"
        )
        checkPointStack = tail
        levelTopCheckpoint -= 1
      case Nil =>
        require(false, "Should happen : pop on an empty stack")
    }
  }

  private def pushTopCheckpoint(newCheckpoint: IntSequence): Unit = {
    checkPointStack = newCheckpoint :: checkPointStack
    levelTopCheckpoint += 1
    require(
      levelTopCheckpoint + 1 == checkPointStack.size,
      s"${levelTopCheckpoint + 1} == ${checkPointStack.size}"
    )
  }

  private def digestChanges(changes: SeqUpdate): Unit = {
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
        toValue := s
      case SeqUpdateLastNotified(value: IntSequence) =>
        assert(value equals toValue.newValue)
      case SeqUpdateRollBackToTopCheckpoint(
            value: IntSequence,
            howToRollBack: SeqUpdate,
            level: Int,
            prev: SeqUpdate
          ) =>
        digestChanges(prev)
        require(
          level == levelTopCheckpoint,
          s"Top checkpoint of original sequence is not the same as the copy one: Should be $level got $levelTopCheckpoint"
        )
        require(
          value sameIdentity checkPointStack.head,
          s"fail on quick equals equals=${value.toList equals checkPointStack.head.toList} value:$value topCheckpoint:${checkPointStack.head}"
        )
        toValue.rollbackToTopCheckpoint()
      case SeqUpdateReleaseTopCheckPoint(prev: SeqUpdate, _: IntSequence) =>
        digestChanges(prev)
        popTopCheckpoint()
        toValue.releaseTopCheckpoint()
      case SeqUpdateDefineCheckpoint(prev: SeqUpdate, level: Int) =>
        digestChanges(prev)
        require(changes.newValue sameIdentity prev.newValue)
        pushTopCheckpoint(changes.newValue)
        assert(
          levelTopCheckpoint == level,
          s"Top checkpoint of original sequence is not the same as the copy one: Should be $level got $levelTopCheckpoint"
        )
        toValue.defineCurrentValueAsCheckpoint(Some(changes.newValue))

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
