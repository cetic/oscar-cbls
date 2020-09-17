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
import oscar.cbls.core.computation.{ChangingIntValue, ChangingSeqValue, SeqCheckpointedValueStack, SeqInvariant, SeqNotificationTarget, SeqUpdate, SeqUpdateAssign, SeqUpdateDefineCheckpoint, SeqUpdateInsert, SeqUpdateLastNotified, SeqUpdateMove, SeqUpdateRemove, SeqUpdateRollBackToCheckpoint, SeqValue, ShortIntNotificationTarget}
import oscar.cbls.core.propagation.Checker

//TODO: document, test and put into modeling API
case class SubSequence(v: SeqValue,index:Int, length: Int,
                       override val maxPivotPerValuePercent:Int = 10,
                       override val maxHistorySize:Int = 10)
  extends SeqInvariant(IntSequence.empty(), v.max, maxPivotPerValuePercent, maxHistorySize)
    with SeqNotificationTarget{

  setName(s"SubSequence(${v.name})")

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  def internalPrint(): Unit ={
    val seq = v.value.toList
    for(i <- seq.indices){
      if(i == index){
        print("[")
      }
      print(seq(i))
      if(i == index+length-1){
        print("]")
      }
      if(i != seq.length-1){
        print(",")
      }
    }
    print(" -> \n")
    println("["+this.value.toList.mkString(",")+"]")
  }

  def computeFromScratch(s:IntSequence): IntSequence = {
    var explorer = s.explorerAtPosition(index)
    var subSeq = IntSequence.empty()

    for(i <- 0 until length){
      explorer match {
        case None => return subSeq.regularize()
        case Some(e) =>
          subSeq = subSeq.insertAtPosition(e.value,i,true)
          explorer = e.next
      }
    }
    subSeq.regularize()
  }

  this := computeFromScratch(v.value)

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    if (!digestChanges(changes)) {
      this := computeFromScratch(changes.newValue)
    }
  }

  val checkpointStack = new SeqCheckpointedValueStack[IntSequence]()

  def digestChanges(changes : SeqUpdate) : Boolean = {
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        if( pos >= index+length) return true
        if( this.newValue.size == length){
          this.remove(length-1)
        }
        if(pos >= index){
          this.insertAtPosition(value, pos-index)
        }else{
          if(changes.newValue.size > index){
            this.insertAtPosition(changes.newValue.valueAtPosition(index).head,0)
          }
        }
        true

      case SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        if((toIncluded < index && after < index) ||
           (fromIncluded >= index+length && after >= index+length-1)) return true

        if(fromIncluded >= index && toIncluded <= index+length-1 && after > index-1 && after <= index+length - 1) {
          this.move(fromIncluded - index, toIncluded - index, after - index, flip)
          return true
        }
        false

      case r@SeqUpdateRemove(pos : Int, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        if( pos >= index+length) return true

        changes.newValue.valueAtPosition(index+length-1) match{
          case None => ()
          case Some(v) => this.insertAtPosition(v,length)
        }

        if(pos >= index){
          this.remove(pos-index)
        }else{
          this.remove(0)
        }
        true

      case u@SeqUpdateRollBackToCheckpoint(checkpoint,checkPointLevel) =>

        this.releaseTopCheckpointsToLevel(checkPointLevel,false)
        rollbackToTopCheckpoint(checkpointStack.rollBackAndOutputValue(checkpoint,checkPointLevel))
        true

      case SeqUpdateDefineCheckpoint(prev : SeqUpdate, isActive, checkpointLevel) =>
        if(!digestChanges(prev)){
          this := computeFromScratch(prev.newValue)
        }

        releaseTopCheckpointsToLevel(checkpointLevel,true)
        this.defineCurrentValueAsCheckpoint(isActive)
        //we perform this after the define checkpoint above, so that hte saved value is the regularized one (I do not know, but his might be a good idea)
        checkpointStack.defineCheckpoint(prev.newValue,checkpointLevel,this.newValue)
        true

      case SeqUpdateLastNotified(value) =>
        true

      case SeqUpdateAssign(value : IntSequence) =>
        false
    }
  }

  override def checkInternals(c: Checker): Unit = {
    require(this.newValue.toList equals computeFromScratch(v.value).toList,
      Some(s"this.newValue(=${this.newValue.toList}) == v.value.subSequence(=${v.value.toList.reverse})"))
   }
}

//TODO: document, test and put into modeling API
case class SubSequenceVar(originalSeq: SeqValue, index:ChangingIntValue, length: Int, override val maxPivotPerValuePercent:Int = 10,
                          override val maxHistorySize:Int = 10)(shiftLimitBeforeRecompute:Int = length/2)
  extends SeqInvariant(IntSequence.empty(), originalSeq.max, maxPivotPerValuePercent, maxHistorySize)
    with SeqNotificationTarget with ShortIntNotificationTarget{

  //setName("Flip(" + v.name + ")")
  registerStaticAndDynamicDependency(index)
  registerStaticAndDynamicDependency(originalSeq)
  finishInitialization()

  def printAll(): Unit ={
    val seq = originalSeq.value.toList
    for(i <- seq.indices){
      if(i == index.value){
        print("[")
      }
      print(seq(i))
      if(i == index.value+length-1){
        print("]")
      }
      if(i != seq.length-1){
        print(",")
      }
    }
    print(" -> \n")
    println("["+this.value.toList.mkString(",")+"]")
  }

  def computeFromScratch(s:IntSequence, idx:Int): IntSequence = {
    var explorer = s.explorerAtPosition(idx)
    var subSeq = IntSequence.empty()

    for(i <- 0 until length){
      explorer match {
        case None => return subSeq.regularize()
        case Some(e) =>
          subSeq = subSeq.insertAtPosition(e.value,i,true)
          explorer = e.next
      }
    }
    subSeq.regularize()
  }

  this := computeFromScratch(originalSeq.value, index.valueInt)

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Int, NewVal: Int): Unit = {
    require(v == index)
    if(NewVal >= originalSeq.value.size  && OldVal >= originalSeq.value.size) return
    if(Math.abs(OldVal-NewVal) > shiftLimitBeforeRecompute){
      this := computeFromScratch(originalSeq.value,NewVal)
      return
    }
    if(NewVal > OldVal){
      for( i <- 0 until NewVal-Math.max(0,OldVal)){
        this.remove(0)
      }
      var originalExplorer = originalSeq.value.explorerAtPosition(OldVal+length)
      for( i <- 0 until NewVal-OldVal){
        originalExplorer match {
          case None => return
          case Some(e) => this.insertAtPosition(e.value, this.newValue.size)
            originalExplorer = e.next
        }
      }
    }else if(OldVal > NewVal){
      for( i <- 0 until OldVal - NewVal - Math.max(0,(OldVal+length-originalSeq.value.size))){
        this.remove(this.newValue.size-1)
      }
      var originalExplorer = originalSeq.value.explorerAtPosition(OldVal-1)
      for( i <- 0 until OldVal-NewVal){
        originalExplorer match {
          case None => return
          case Some(e) => this.insertAtPosition(e.value, 0)
            originalExplorer = e.prev
        }
      }
    }
  }

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    if (!digestChanges(changes)) {
      this := computeFromScratch(v.value,index.valueInt)
    }
  }

  def digestChanges(changes : SeqUpdate) : Boolean = {
    val currentIndex = index.valueInt
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        if( pos >= currentIndex+length) return true
        if( this.newValue.size == length){
          this.remove(length-1)
        }
        if(pos >= currentIndex){
          this.insertAtPosition(value, pos-currentIndex)
        }else{
          if(changes.newValue.size > currentIndex){
            this.insertAtPosition(changes.newValue.valueAtPosition(currentIndex).head,0)
          }
        }
        true

      case SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        if((toIncluded < currentIndex && after < currentIndex) ||
          (fromIncluded >= currentIndex+length && after >= currentIndex+length-1)) return true

        if(fromIncluded >= currentIndex && toIncluded <= currentIndex+length-1 && after > currentIndex-1 && after <= currentIndex+length - 1) {
          this.move(fromIncluded - currentIndex, toIncluded - currentIndex, after - currentIndex, flip)
          return true
        }
        false

      case r@SeqUpdateRemove(pos : Int, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        if( pos >= currentIndex+length) return true

        changes.newValue.valueAtPosition(currentIndex+length-1) match{
          case None => ()
          case Some(v) => this.insertAtPosition(v,length)
        }

        if(pos >= currentIndex){
          this.remove(pos-currentIndex)
        }else{
          this.remove(0)
        }

        true

      case u@SeqUpdateRollBackToCheckpoint(checkpoint,checkpointLevel) =>
        digestChanges(u.howToRollBack)

      case SeqUpdateDefineCheckpoint(prev, isActive, checkpointLevel) =>
        digestChanges(prev)

      case SeqUpdateLastNotified(value) =>
        true

      case SeqUpdateAssign(value : IntSequence) =>
        false
    }
  }

  override def checkInternals(c: Checker): Unit = {
    require(this.newValue.toList equals computeFromScratch(originalSeq.value,index.valueInt).toList,
      Some(s"this.newValue(=${this.newValue.toList}) == v.value.subSequence(=${originalSeq.value.toList.reverse})"))
  }
}
