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
import oscar.cbls.core.computation.{ChangingSeqValue, SeqNotificationTarget, SeqUpdate, SeqUpdateAssign, SeqUpdateDefineCheckpoint, SeqUpdateInsert, SeqUpdateLastNotified, SeqUpdateMove, SeqUpdateRemove, SeqUpdateRollBackToCheckpoint, SeqValue, SetInvariant}
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.SortedSet

/**
 * content of v
 * @param v is a SeqValue, the values appearing in the sequence
 * @author renaud.delandtsheer@cetic.be
 */
case class Content(v:SeqValue)
  extends SetInvariant(SortedSet.empty[Int] ++ v.value.unorderedContentNoDuplicate,v.domain)
  with SeqNotificationTarget{

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    if(!digestUpdates(changes)) {
      updateFromScratch(changes.newValue)
    }
  }

  private def updateFromScratch(u:IntSequence): Unit ={
    this := (SortedSet.empty[Int] ++ u.unorderedContentNoDuplicate)
  }

  //true if could be incremental, false otherwise
  def digestUpdates(changes : SeqUpdate):Boolean = {
    changes match {
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if (!digestUpdates(prev)) return false
        this :+= value
        true
      case SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        digestUpdates(prev)
      case r@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        if (!digestUpdates(prev)) return false
        val value = r.removedValue
        if (changes.newValue.nbOccurrence(value) == 0){
          this :-= value
        }
        true
      case r@SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence,checkpointLevel:Int) =>
        digestUpdates(r.howToRollBack)

      case SeqUpdateLastNotified(value) =>
        require(value quickEquals v.value)
        //start at the previous value; easy game.
        true
      case SeqUpdateAssign(value:IntSequence) =>
        //raw assign, no incremental possible
        false
      case SeqUpdateDefineCheckpoint(prev:SeqUpdate,isStarMode:Boolean,checkpointLevel) =>
        digestUpdates(prev)
    }
  }

  override def checkInternals(c: Checker): Unit = {
    c.check(this.value.toList.sorted equals v.value.unorderedContentNoDuplicate.sorted,
      Some(s"this.value.toList:${this.value.toList} == v.value.toList:${v.value.unorderedContentNoDuplicate.sorted} v.value.unorderedContentNoDuplicate:$v"))
  }
}
