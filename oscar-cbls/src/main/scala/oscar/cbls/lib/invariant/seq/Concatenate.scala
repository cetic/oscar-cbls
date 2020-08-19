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
import oscar.cbls.core.computation.{CBLSSeqConst, ChangingSeqValue, SeqCheckpointedValueStack, SeqInvariant, SeqNotificationTarget, SeqUpdate, SeqUpdateAssign, SeqUpdateDefineCheckpoint, SeqUpdateInsert, SeqUpdateLastNotified, SeqUpdateMove, SeqUpdateRemove, SeqUpdateRollBackToCheckpoint, SeqValue}
import oscar.cbls.core.propagation.Checker

/**
 * helper object for concatenation invariant between sequences of integers
 */
object Concatenate {

  /**
   * builds a changing seq value that is maintained as the concatenation of a and b.
   * if either of them is a constant, the instantiated class is adapted
   *
   * @param a
   * @param b
   * @param maxPivotPerValuePercent
   * @param maxHistorySize
   * @return
   */
  def apply(a : SeqValue, b : SeqValue, maxPivotPerValuePercent : Int = 4, maxHistorySize : Int = 20) : SeqValue = {
    (a,b) match {
      case (ac : CBLSSeqConst,bc : CBLSSeqConst) =>
        CBLSSeqConst(IntSequence(ac.value ++ bc.value))
      case (ac : CBLSSeqConst,bv : ChangingSeqValue) =>
        new ConcatenateFirstConstant(ac.value.toList, bv, maxPivotPerValuePercent, maxHistorySize)
      case (av : ChangingSeqValue, bc : CBLSSeqConst) =>
        new ConcatenateSecondConstant(av, bc.value.toList, maxPivotPerValuePercent, maxHistorySize)
      case (av : ChangingSeqValue, bv : ChangingSeqValue) =>
        new Concatenate(av, bv, maxPivotPerValuePercent, maxHistorySize)
    }
  }
}

class Concatenate(a:ChangingSeqValue,b:ChangingSeqValue,maxPivotPerValuePercent: Int, maxHistorySize:Int)
  extends SeqInvariant(IntSequence(a.value ++ b.value), math.max(a.max,b.max), maxPivotPerValuePercent, maxHistorySize)
  with SeqNotificationTarget {

  require(a != b)

  registerStaticAndDynamicDependency(a, 0)
  registerStaticAndDynamicDependency(b, 1)
  finishInitialization()

  var outputToRecompute:Boolean = false

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    val isFirst = (d == 0)
    if(!outputToRecompute && !digestChanges(isFirst, changes)) {
      outputToRecompute = true
      scheduleForPropagation()
    }
  }

  override def performInvariantPropagation() : Unit = {
    if(this.outputToRecompute){
      outputToRecompute = false
      this := IntSequence(a.value ++ b.value)
    }
  }

  def digestChanges(isFirst : Boolean, changes : SeqUpdate) : Boolean = {
    changes match {
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if (!digestChanges(isFirst, prev)) return false
        this.insertAtPosition(value, if (isFirst) pos else pos + a.value.size)
        true

      case SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        if (!digestChanges(isFirst, prev)) return false
        if (isFirst) {
          this.move(fromIncluded, toIncluded, after, flip)
        } else {
          val offset = a.value.size
          this.move(fromIncluded + offset, toIncluded + offset, after + offset, flip)
        }
        true

      case SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        if (!digestChanges(isFirst, prev)) return false
        this.remove(if (isFirst) position else position + a.value.size)
        true

      case u@SeqUpdateRollBackToCheckpoint(checkpoint,checkpointLevel) =>
        digestChanges(isFirst, u.howToRollBack)

      case SeqUpdateDefineCheckpoint(prev : SeqUpdate, isStarMode, checkpointLevel) =>
        digestChanges(isFirst, prev)

      case SeqUpdateLastNotified(value) =>
        true

      case SeqUpdateAssign(value : IntSequence) =>
        false
    }
  }

  override def checkInternals(c : Checker) : Unit = {
    c.check((a.value.toList ++ b.value.toList) equals this.value.toList,
      Some(s"a.value.toList:${a.value.toList} b.value.toList:${b.value.toList} should== this.value.toList${this.value.toList}"))
  }
}

class ConcatenateFirstConstant(a:List[Int],b:ChangingSeqValue,maxPivotPerValuePercent: Int, maxHistorySize:Int)
  extends SeqInvariant(IntSequence(a ++ b.value), math.max(a.max,b.max), maxPivotPerValuePercent, maxHistorySize)
  with SeqNotificationTarget {

  registerStaticAndDynamicDependency(b)

  finishInitialization()

  var checkpointStack = new SeqCheckpointedValueStack[IntSequence]()

  var offsetForSecond = a.size

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    if (!digestChanges(changes)) {
      this := IntSequence(a ++ changes.newValue)
    }
  }

  def digestChanges(changes : SeqUpdate) : Boolean = {
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        this.insertAtPosition(value, pos + offsetForSecond)
        true

      case SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        this.move(fromIncluded + offsetForSecond, toIncluded + offsetForSecond, after + offsetForSecond, flip)
        true

      case r@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        this.remove(position + offsetForSecond)
        true

      case u@SeqUpdateRollBackToCheckpoint(checkpoint,checkpointLevel) =>
        require(checkpoint quickEquals checkpointStack.topCheckpoint)
        this.releaseTopCheckpointsToLevel(checkpointLevel,false)
        this.rollbackToTopCheckpoint(checkpointStack.rollBackAndOutputValue(checkpoint,checkpointLevel))
        true

      case SeqUpdateDefineCheckpoint(prev : SeqUpdate, isActive, checkpointLevel) =>
        if(!digestChanges(prev)) {
          this := IntSequence(a ++ prev.newValue)
        }
        this.releaseTopCheckpointsToLevel(checkpointLevel,true)
        this.defineCurrentValueAsCheckpoint(isActive)
        checkpointStack.defineCheckpoint(prev.newValue,checkpointLevel,this.newValue)
        true

      case SeqUpdateLastNotified(value) =>
        true

      case SeqUpdateAssign(value : IntSequence) =>
        false
    }
  }

  override def checkInternals(c : Checker) : Unit = {
    c.check((a ++ b.value.toList) equals this.value.toList,
      Some(s"a.value.toList:$a b.value.toList:${b.value.toList} should== this.value.toList${this.value.toList}"))
  }
}

class ConcatenateSecondConstant(a:ChangingSeqValue,b:List[Int],maxPivotPerValuePercent: Int, maxHistorySize:Int)
  extends SeqInvariant(IntSequence(a.value ++ b), math.max(a.max,b.max), maxPivotPerValuePercent, maxHistorySize)
  with SeqNotificationTarget {

  registerStaticAndDynamicDependency(a)
  finishInitialization()

  var checkpointStack = new SeqCheckpointedValueStack[IntSequence]()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    if (!digestChanges(changes)) {
      this := IntSequence(changes.newValue ++ b)
    }
  }

  def digestChanges(changes : SeqUpdate) : Boolean = {
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        this.insertAtPosition(value, pos)
        true

      case SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        this.move(fromIncluded, toIncluded, after, flip)
        true

      case r@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        this.remove(position)
        true

      case u@SeqUpdateRollBackToCheckpoint(checkpoint,checkpointLevel) =>
        require(checkpoint quickEquals checkpointStack.topCheckpoint)
        this.releaseTopCheckpointsToLevel(checkpointLevel,false)
        this.rollbackToTopCheckpoint(checkpointStack.rollBackAndOutputValue(checkpoint,checkpointLevel))
        true

      case SeqUpdateDefineCheckpoint(prev : SeqUpdate, isActive, checkpointLevel) =>
        if(!digestChanges(prev)) {
          this := IntSequence(prev.newValue ++ b)
        }
        this.releaseTopCheckpointsToLevel(checkpointLevel,true)
        this.defineCurrentValueAsCheckpoint(isActive)
        checkpointStack.defineCheckpoint(prev.newValue,checkpointLevel,this.newValue)
        true

      case SeqUpdateLastNotified(value) =>
        true

      case SeqUpdateAssign(value : IntSequence) =>
        false
    }
  }

  override def checkInternals(c : Checker) : Unit = {
    c.check((a.value.toList ++ b) equals this.value.toList,
      Some(s"a.value.toList:${a.value.toList} b.value.toList:$b should== this.value.toList${this.value.toList}"))
  }
}
