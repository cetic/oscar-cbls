/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
package oscar.cbls.lib.invariant.seq

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.core.computation.{ChangingSeqValue, SeqCheckpointedValueStack, SeqInvariant, SeqNotificationTarget, SeqUpdate, SeqUpdateAssign, SeqUpdateDefineCheckpoint, SeqUpdateInsert, SeqUpdateLastNotified, SeqUpdateMove, SeqUpdateRemove, SeqUpdateRollBackToCheckpoint, SeqValue}
import oscar.cbls.core.propagation.Checker

/**
 * maintains this as the flipped value of v
 * @param v
 * @param maxPivotPerValuePercent
 * @param maxHistorySize
 */
case class Flip(v: SeqValue,override val maxPivotPerValuePercent:Int = 10, override val maxHistorySize:Int = 10)
  extends SeqInvariant(v.value.flip(true,true), v.max, maxPivotPerValuePercent, maxHistorySize)
  with SeqNotificationTarget{

  setName(s"Flip(${v.name})")

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    if (!digestChanges(changes)) {
      this := changes.newValue.flip(true,true)
    }
  }

  val checkpointStack = new SeqCheckpointedValueStack[IntSequence]()

  def digestChanges(changes : SeqUpdate) : Boolean = {
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false

        //build on the original value instead of maintaining two data structs? , changes.newValue.flip(fast=true)
        this.insertAtPosition(value, prev.newValue.size - pos)

        true

      case m@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        if(m.isNop) {
          ;
        }else if(m.isSimpleFlip){
          this.flip(0,prev.newValue.size -1)
        }else {
          //Complete move with a flip

          //there is a special case if the after is -1
          if (after == -1){
            //the segment to move starts at zero, and ends later
            val numberOfMovesPointsMinusOne = toIncluded - fromIncluded
            val prevSize = prev.newValue.size
            val flippedFromIncluded = prevSize - toIncluded - 1
            val flippedToIncluded = prevSize - fromIncluded - 1
            this.move(flippedFromIncluded, flippedToIncluded, prev.newValue.size-1, flip)

          }else {
            val prevSize = prev.newValue.size
            val tentativeFlippedAfter = prevSize - after - 2
            val flippedFromIncluded = prevSize - toIncluded - 1
            val flippedToIncluded = prevSize - fromIncluded - 1
            val flippedAfter = if (tentativeFlippedAfter == flippedToIncluded) flippedFromIncluded - 1 else tentativeFlippedAfter
            this.move (flippedFromIncluded, flippedToIncluded, flippedAfter, flip)
          }
        }
        true

      case r@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        this.remove(prev.newValue.size - position - 1,changes.newValue.flip(fast=true))
        true

      case u@SeqUpdateRollBackToCheckpoint(checkPoint,level) =>
        releaseTopCheckpointsToLevel(level,false)
        this.rollbackToTopCheckpoint(checkpointStack.rollBackAndOutputValue(checkPoint,level))
        require(checkPoint quickEquals checkpointStack.topCheckpoint)
        assert(this.newValue quickEquals checkpointStack.outputAtTopCheckpoint(checkPoint))
        true

      case SeqUpdateDefineCheckpoint(prev : SeqUpdate, isStarMode, level) =>
        if(!digestChanges(prev)){
          this := changes.newValue.flip(false,true)
        }

        releaseTopCheckpointsToLevel(level,true)
        this.defineCurrentValueAsCheckpoint(isStarMode)
        checkpointStack.defineCheckpoint(prev.newValue,level,this.newValue)
        true

      case SeqUpdateLastNotified(value) =>
        true

      case SeqUpdateAssign(value : IntSequence) =>
        releaseTopCheckpointsToLevel(0,true)
        false
    }
  }

  override def checkInternals(c: Checker): Unit = {
    c.check(this.newValue.toList equals v.value.toList.reverse, Some(s"this.newValue(=${this.newValue.toList}) == v.value.flip(=${v.value.toList.reverse})"))
    c.check(this.newValue.toList.reverse equals v.value.toList, Some(s"this.newValue.flip(=${this.newValue.toList.reverse}) == v.value(=${v.value.toList})"))
  }
}
