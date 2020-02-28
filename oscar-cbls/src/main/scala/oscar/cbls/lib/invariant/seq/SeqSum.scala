package oscar.cbls.lib.invariant.seq

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

import oscar.cbls._
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.core._


/**
 * sum(f(v))
 * the sum of all element in v after passing through f;
 * if a value has multiple occurrences, their f-transformed occurrences are summed
 * @param v is a SeqValue, containing a number of values, to sum
 * @param f is a function that is applied to every value in f prior to the sum
 * @author renaud.delandtsheer@cetic.be
 */
case class SeqSum(v: SeqValue, f:(Long => Long) = (a:Long) => a)
  extends IntInvariant()
  with SeqNotificationTarget{

  setName("SeqSum(" + v.name + ")")

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  this := computeSumFromScratch(v.value)

  val checkpointStack = new SeqCheckpointedValueStack[Long]()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    if (!digestChanges(changes)) {
      this := computeSumFromScratch(changes.newValue)
    }
  }

  private def computeSumFromScratch(v:IntSequence):Long = {
    var contentWithOccurences = v.unorderedContentNoDuplicateWithNBOccurences
    var sum = 0L
    while(contentWithOccurences match{
      case Nil => return sum
      case (value,occ) :: tail =>
        sum += (f(value)*occ)
        contentWithOccurences = tail
        true
    }){}
    0L
  }

  def digestChanges(changes : SeqUpdate) : Boolean = {
    changes match {
      case s@SeqUpdateInsert(value : Long, pos : Int, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        this :+= f(value)
        true

      case SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
         digestChanges(prev) //nothing to do :-)

      case r@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        this :-= f(r.removedValue)
        true

      case u@SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence,checkpointLevel:Int) =>
        this := checkpointStack.rollBackAndOutputValue(checkpoint,checkpointLevel)
        true

      case SeqUpdateDefineCheckpoint(prev : SeqUpdate, isActive,checkpointLevel) =>
        if(!digestChanges(prev)){
          val myOutput = computeSumFromScratch(changes.newValue)
          checkpointStack.defineCheckpoint(prev.newValue,checkpointLevel,myOutput)
          this := myOutput
          true
        }else{
          checkpointStack.defineCheckpoint(prev.newValue,checkpointLevel,this.newValue)
          true
        }

      case SeqUpdateLastNotified(value) =>
        true

      case SeqUpdateAssign(value : IntSequence) =>
        false
    }
  }

  override def checkInternals(c: Checker) {
//    c.check(this.newValue == v.value.toList.map(f).sum)
    c.check(this.newValue == computeSumFromScratch(v.value),Some("this.newValue(="+ this.newValue+") == Sum(v.value(="+ computeSumFromScratch(v.value)+ ")"))
  }
}

