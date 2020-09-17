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
import oscar.cbls.core.computation.{CBLSSetVar, ChangingSeqValue, Invariant, SeqNotificationTarget, SeqUpdate, SeqUpdateAssign, SeqUpdateDefineCheckpoint, SeqUpdateInsert, SeqUpdateLastNotified, SeqUpdateMove, SeqUpdateRemove, SeqUpdateRollBackToCheckpoint}
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.SortedSet

object Successors {
  /**
   * Maintains and array telling, for each value (indice of the array) the set of value that can succeed it in the sequence.
   * There are multiple successors although we only consider the next value
   * because a value can appear several time in the sequence, it can therefore have several successors.
    *
    * @param seq
   * @return the array of SetVar that mention the set of successor for each possible value.
   * @author renaud.delandtsheer@cetic.be
   */
  def apply(seq : ChangingSeqValue) : Array[CBLSSetVar] = {

    val succ: Array[CBLSSetVar] =
      Array.tabulate(seq.max+1)(v => CBLSSetVar(seq.model, name = "next Value of" + v))

    new Successors(seq, succ)

    succ
  }
}

/**
 * Maintains and array telling, for each value (indice of the array) the set of value that can succeed it in the sequence.
 * There are multiple successors although we only consider the next value
 * because a value can appear several time in the sequence, it can therefore have several successors.
  *
  * @param sequence
 * @param successorValues
 * @author renaud.delandtsheer@cetic.be
 */
class Successors(sequence:ChangingSeqValue, successorValues:Array[CBLSSetVar])
  extends Invariant() with SeqNotificationTarget {

  //this one does not use checkpoint at all, so just skipping them.

  registerStaticAndDynamicDependency(sequence)
  finishInitialization()
  for(i <- successorValues) i.setDefiningInvariant(this)

  computeAllFromScratch(sequence.value)

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    performUpdate(changes:SeqUpdate)
  }

  private def performUpdate(changes:SeqUpdate): Unit ={
    computeImpactedValues(changes) match{
      case None =>
      computeAllFromScratch(changes.newValue)
      case Some(values) =>
        computeForValues(changes.newValue,values)
    }
  }

  def computeImpactedValues(changes:SeqUpdate):Option[SortedSet[Int]] = {
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        //removing next val from pred

        computeImpactedValues(prev) match{
          case None => None
          case Some(impactedValue) =>
            val oldSeq = prev.newValue
            Some(impactedValue ++ oldSeq.predecessorPos2Val(pos) + value)
          }

      case SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        computeImpactedValues(prev) match{
          case None => None
          case Some(impactedValues) =>
            val oldSeq = prev.newValue
            if(flip){
              //O(n) stuff
              Some(impactedValues
                ++ changes.newValue.predecessorPos2Val(fromIncluded)
                ++ changes.newValue.valueAtPosition(after)
                ++ oldSeq.valuesBetweenPositionsQList(fromIncluded, toIncluded))
            }else{
              Some(impactedValues ++
                changes.newValue.predecessorPos2Val(fromIncluded) +
                toIncluded ++
                changes.newValue.valueAtPosition(after))
            }
        }

      case r@SeqUpdateRemove(position: Int, prev : SeqUpdate) =>
        computeImpactedValues(prev) match {
          case None => None
          case Some(impactedValues) =>
            val oldSeq = prev.newValue
            Some(impactedValues + r.removedValue ++ oldSeq.predecessorPos2Val(position))
        }

      case u@SeqUpdateRollBackToCheckpoint(checkpoint,checkpointLevel) =>
        computeImpactedValues(u.howToRollBack)

      case SeqUpdateDefineCheckpoint(prev:SeqUpdate,isActive,checkpointLevel) =>
        computeImpactedValues(prev)

      case SeqUpdateLastNotified(value) =>
        require(value quickEquals sequence.value)
        Some(SortedSet.empty[Int]) //we are starting from the previous value

      case SeqUpdateAssign(value : IntSequence) =>
        None //impossible to go incremental
    }
  }

  def computeForValues(seq:IntSequence, values:SortedSet[Int]): Unit ={
    for(value <- values){
      successorValues(value) := SortedSet.empty[Int] ++ seq.positionsOfValue(value).flatMap(position => seq.successorPos2Val(position))
    }
  }

  def computeAllFromScratch(seq:IntSequence): Unit ={
    val emptySet = SortedSet.empty[Int]
    successorValues.foreach(node => node := emptySet)

    var explorer = seq.explorerAtPosition(0).head
    while(explorer.next match{
      case None =>
        false
      case Some(next) =>
        successorValues(explorer.value) :+= next.value
        explorer = next
        true
    }){}
  }

  def computeAllFromScratchNoAffect(seq:IntSequence):Array[SortedSet[Int]] = {
    val emptySet = SortedSet.empty[Int]
    val successorValues = Array.tabulate(sequence.max+1)(v => emptySet)

    try{
    var explorer = seq.explorerAtPosition(0).head
    while(explorer.next match{
      case None =>
        false
      case Some(next) =>
        successorValues(explorer.value) += next.value
        explorer = next
        true
    }){}
    }catch{
      case e: Exception =>
    }

    successorValues
  }

  override def checkInternals(c : Checker): Unit ={
    val fromScratch = computeAllFromScratchNoAffect(sequence.value)
    for(node <- 0 to sequence.maxValue){  //TODO: sequence .value.size, and we can have redundant values in the equence!!!
      c.check(
        successorValues(node).value
          ==
        fromScratch(node),
        Some(s"error on next for node $node: ${successorValues(node).value} should== ${fromScratch(node)}"))
    }
  }
}
