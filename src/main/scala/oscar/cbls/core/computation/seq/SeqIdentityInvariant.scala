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

object SeqIdentityInvariant {

  /** Creates a SeqIdentityInvariant.
    *
    * @param store
    *   The model to which input and output are registered.
    * @param input
    *   The SeqVariable we are copying.
    * @param output
    *   The copy of the input.
    * @return
    *   A SeqIdentityInvariant
    */
  def apply(store: Store, input: SeqVariable, output: SeqVariable): SeqIdentityInvariant = {
    new SeqIdentityInvariant(store, input, output)
  }
}

/** An Invariant whose job is to ensure that output is strictly identical to input.
  *
  * After each propagation of input updates, output must be equal to input. Meaning, all stacked
  * updates must be the same and the associated [[oscar.cbls.algo.sequence.IntSequence]] must have
  * the same token (identity).
  *
  * This invariant is used for instance when you have several heavy constraints depending on one
  * SeqVariable. Instead of updating all the constraints at once, using several copy of the
  * SeqVariable and linking each constraint to a different copy will save you time. For instance:
  *   - A CVRPTW. The capacity constraint could be slow to compute, as for the time window
  *     constraint. Let's say it takes 5 ms for the capacity and 10 ms for the time window.
  *   - Without the use of copy, each time you explore a movement, you'll spend 15ms to update both
  *     of them.
  *   - With the copies like this:
  *     - main <- copy1 <- copy2
  *     - RouteLength listening to copy2.
  *     - TW listening to copy 1.
  *     - Capacity listening to main.
  *   - Checking if the capacity constraint is not violated won't trigger the update of TW or
  *     RouteLength. Saving you time.
  *
  * @param store
  *   The model to which input and output are registered.
  * @param input
  *   The SeqVariable we are copying.
  * @param output
  *   The copy of the input.
  */
class SeqIdentityInvariant(store: Store, input: SeqVariable, output: SeqVariable)
    extends Invariant(store)
    with SeqNotificationTarget {

  input.registerStaticallyAndDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)

  output := input.value()

  override def notifySeqChanges(
    v: SeqVariable,
    contextualVarIndex: Int,
    changes: SeqUpdate
  ): Unit = {
    assert(v == input)
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
        require(requirement = false, "Shouldn't happen: pop on an empty stack")
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
        output.insertAfterPosition(value, insertAfterPositionExplorer, Some(changes.newValue))

      case SeqUpdateMove(
            fromIncludedExplorer: IntSequenceExplorer,
            toIncludedExplorer: IntSequenceExplorer,
            afterExplorer: IntSequenceExplorer,
            flip: Boolean,
            prev: SeqUpdate
          ) =>
        digestChanges(prev)
        output.move(
          fromIncludedExplorer,
          toIncludedExplorer,
          afterExplorer,
          flip,
          Some(changes.newValue)
        )

      case SeqUpdateRemove(removePositionExplorer: IntSequenceExplorer, prev: SeqUpdate) =>
        digestChanges(prev)
        output.remove(removePositionExplorer, Some(changes.newValue))

      case SeqUpdateAssign(s) =>
        output := s

      case SeqUpdateLastNotified(value: IntSequence) =>
        assert(value equals output.pendingValue)

      case SeqUpdateRollBackToTopCheckpoint(
            value: IntSequence,
            _: SeqUpdate,
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
        output.rollbackToTopCheckpoint()

      case SeqUpdateReleaseTopCheckpoint(prev: SeqUpdate, _: IntSequence) =>
        digestChanges(prev)
        popTopCheckpoint()
        output.releaseTopCheckpoint()

      case SeqUpdateDefineCheckpoint(prev: SeqUpdate, level: Int) =>
        digestChanges(prev)
        require(changes.newValue sameIdentity prev.newValue)
        pushTopCheckpoint(changes.newValue)
        assert(
          levelTopCheckpoint == level,
          s"Top checkpoint of original sequence is not the same as the copy one: Should be $level got $levelTopCheckpoint"
        )
        output.defineCurrentValueAsCheckpoint(Some(changes.newValue))

      case _ =>
      // Default case, do nothing
    }
  }

  override def checkInternals(): Unit = {
    require(
      output.pendingValue.toList equals input.pendingValue.toList,
      Some(
        s"IdentitySeq: toValue.value=${output.value()} should equals fromValue.value=${input.value()}"
      )
    )
  }
}
