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

package oscar.cbls.lib.invariant.set

import oscar.cbls.algo.quick.QList
import oscar.cbls.core.computation.{ChangingSetValue, Domain, IntInvariant, SetInvariant, SetNotificationTarget, SetValue}
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.SortedSet

/**
 * sum(on s:subsetToMonitorAndMaxValues if #(s._1 inter s) > s._2 of s._3 )
 * @param s a setValue
 * @param subsetToMonitorAndMaxValues iterable of (subset, max occurrence in the subset, weight in case of violation)
 */
case class IncludedSubsets(s: SetValue, subsetToMonitorAndMaxValues:Iterable[(Iterable[Int],Int,Int)])
  extends IntInvariant(0,Domain(0 , subsetToMonitorAndMaxValues.map(_._3).sum))
  with SetNotificationTarget{

  registerStaticAndDynamicDependenciesNoID(s)
  finishInitialization()

  require(s.min == 0, s"SetValue min domain should be equal to 0 instead of ${s.min}")

  val subsetAndMaxAndWeightArray = subsetToMonitorAndMaxValues.toArray
  val n = subsetAndMaxAndWeightArray.length

  val valueToSubsetID:Array[QList[Int]] = Array.fill(s.max+1)(null)

  for (forbiddenID <- subsetAndMaxAndWeightArray.indices) {
    val (values,maxNumber,weight) = subsetAndMaxAndWeightArray(forbiddenID)
    for(value <- values){
      valueToSubsetID(value) = QList(forbiddenID,valueToSubsetID(value))
    }
  }

  this := 0
  val subsetToNbPresent = Array.fill[Int](n)(0)
  for(value <- s.value){
    notifyInsert(value)
  }

  override def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]): Unit = {
    for (added <- addedValues) notifyInsert(added)
    for (deleted <- removedValues) notifyDelete(deleted)
  }

  @inline
  private def notifyInsert(value: Int): Unit = {
    for(subset <- QList.toIterable(valueToSubsetID(value))){
      subsetToNbPresent(subset) = subsetToNbPresent(subset) + 1
      if(subsetToNbPresent(subset) == subsetAndMaxAndWeightArray(subset)._2 + 1) {
        this :+= subsetAndMaxAndWeightArray(subset)._3
      }
    }
  }

  @inline
  private def notifyDelete(value: Int): Unit = {
    for(subset <- QList.toIterable(valueToSubsetID(value))){
      subsetToNbPresent(subset) = subsetToNbPresent(subset) - 1
      if(subsetToNbPresent(subset) == subsetAndMaxAndWeightArray(subset)._2){
        this :-= subsetAndMaxAndWeightArray(subset)._3
      }
    }
  }

  override def checkInternals(c: Checker): Unit = {
    val violation = subsetToMonitorAndMaxValues.map({case (values,maxValue,weight) => if(values.count(v => s.value.contains(v)) > maxValue) weight else 0}).sum
    c.check(this.value == violation,Some(s"included subset Error value=${this.value} should be:$violation"))
  }
}

/**
 * {i in such that there is a clause {subset,maxOcc} in clauseAndMaxOccList such that #(subset \inter s) > maxOcc)
 * @param s a setValue
 * @param clauseAndMaxOccList iterable of (subset, max occurrence in the subset)
 */
case class ValuesInViolatedClauses(s: SetValue, clauseAndMaxOccList:Iterable[(Iterable[Int],Int)])
  extends SetInvariant(initialDomain = s.domain)
  with SetNotificationTarget{

  registerStaticAndDynamicDependenciesNoID(s)
  finishInitialization()

  require(s.min == 0)

  val clauseAndMaxOccArray = clauseAndMaxOccList.toArray
  val numberOfClauses = clauseAndMaxOccArray.length

  //building valueToClauseIDs
  val valueToClauseIDs:Array[QList[Int]] = Array.fill(s.max+1)(null)
  for (clauseID <- clauseAndMaxOccArray.indices) {
    val (values,maxNumber) = clauseAndMaxOccArray(clauseID)
    for(value <- values){
      valueToClauseIDs(value) = QList(clauseID,valueToClauseIDs(value))
    }
  }

  //value to number of violated clauses this is in
  val valToNumberOfViolatedClauses:Array[Int] = Array.fill(s.max + 1)(0)

  //clause to number of values in s in it
  val clauseToNbPresent:Array[Int] = Array.fill(numberOfClauses)(0)

  //init
  this := SortedSet.empty
  for(value <- s.value){
    notifyInsert(value)
  }

  override def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]): Unit = {
    for (added <- addedValues) notifyInsert(added)
    for (deleted <- removedValues) notifyDelete(deleted)
  }

  @inline
  private def setClauseViolated(clauseNumber:Int): Unit ={
    for(number <- clauseAndMaxOccArray(clauseNumber)._1){
      val oldCount = valToNumberOfViolatedClauses(number)
      valToNumberOfViolatedClauses(number) = oldCount + 1
      if(oldCount == 0){
        this.insertValue(number)
      }
    }
  }

  @inline
  private def setClauseNonViolated(clauseNumber:Int): Unit ={
    for(number <- clauseAndMaxOccArray(clauseNumber)._1){
      val oldCount = valToNumberOfViolatedClauses(number)
      valToNumberOfViolatedClauses(number) = oldCount - 1
      if(oldCount == 1){
        this.deleteValue(number)
      }
    }
  }

  @inline
  private def notifyInsert(value: Int): Unit ={
    for(clauseNumber <- QList.toIterable(valueToClauseIDs(value))){
      val oldNbPresent = clauseToNbPresent(clauseNumber)
      clauseToNbPresent(clauseNumber) = oldNbPresent + 1
      if(oldNbPresent == clauseAndMaxOccArray(clauseNumber)._2) {
        setClauseViolated(clauseNumber)
      }
    }
  }

  @inline
  private def notifyDelete(value: Int): Unit ={
    for(clauseNumber <- QList.toIterable(valueToClauseIDs(value))){
      val oldNbPresent = clauseToNbPresent(clauseNumber)
      clauseToNbPresent(clauseNumber) = oldNbPresent - 1
      if(oldNbPresent == clauseAndMaxOccArray(clauseNumber)._2 + 1){
        setClauseNonViolated(clauseNumber)
      }
    }
  }

  private def computeFromScratch(values:SortedSet[Int]):SortedSet[Int] = {
    val violatedClauses = clauseAndMaxOccList.filter({case (clause,maxOcc) => clause.count(v => values.contains(v)) > maxOcc})
    SortedSet.empty[Int] ++ violatedClauses.flatMap({case (clause,maxOcc) => clause})
  }

  override def checkInternals(c: Checker): Unit = {
    val str = s"included subset Error value=${this.value} should be:${computeFromScratch(s.value)}"
    c.check(this.value equals computeFromScratch(s.value), Some(str))
  }
}
