package oscar.cbls.lib.invariant.minmax

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

import oscar.cbls.algo.heap.BinomialHeapWithMoveInt
import oscar.cbls.algo.quick.QList
import oscar.cbls._
import oscar.cbls.core._

import scala.collection.immutable.SortedSet

/**
 * Maintains Min(Var(i) | i in cond)
 * @param varss is an array of Long
 * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
case class MinConstArray(varss: Array[Long], ccond: SetValue, default: Long = Long.MaxValue)
  extends MiaxConstArray(varss, ccond, default) {

  override def Ord(v: Long): Long = v

  override def checkInternals(c: Checker): Unit = {
    if(ccond.value.isEmpty) c.check(value == default)
    else c.check(value == varss(ccond.value.minBy(varss(_))))
  }
}


/**
 * Maintains Max(Var(i) | i in cond)
 * @param varss is an array of IntVar, which can be bulked
 * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
case class MaxConstArray(varss: Array[Long], ccond: SetValue, default: Long = Long.MinValue)
  extends MiaxConstArray(varss, ccond, default) {

  override def Ord(v: Long): Long = -v

  override def checkInternals(c: Checker): Unit = {
    if(ccond.value.isEmpty) c.check(value == default)
    else c.check(value == varss(ccond.value.maxBy(varss(_))))
  }
}


/**
 * Maintains Min(Var(i) | i in cond)
 * this is a variant that is lazy, and maintains a TODO-list of postponed updates.
 * postponed updates are ones that do not impact on the outout of the invariant.
 * when there is an update, it is first checked against the TODO-list, for cancellation.
 * if the update does not impact the output, it is postponed
 * if it affects the output, it is performed
 * @param varss is an array of Long
 * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * @param default the value if ccond is empty
 * @param maxBackLogSize is the maximal number of postponed updates (TODOlist is handled as a FIFO)
 * update is O(log(n)), faster (O(1L) if you do updates and backtracks
 * @author renaud.delandtsheer@cetic.be
 * */
case class MinConstArrayLazy(varss: Array[Long], ccond: SetValue, default: Long = Long.MaxValue, maxBackLogSize:Long = Long.MaxValue)
  extends MiaxConstArrayLazy(varss, ccond, default, maxBackLogSize) {

  override def Ord(v: Long): Long = v

  override def checkInternals(c: Checker): Unit = {
    if(ccond.value.isEmpty) c.check(value == default,Some("default"))
    else  c.check(value == varss(ccond.value.minBy(varss(_))),Some("expected " + varss(ccond.value.minBy(varss(_))) + " got " + value))
  }

  @inline
  override def equalOrNotImpactingMiax(potentialMiax: Long): Boolean = this.newValue <= potentialMiax
}


/**
 * Maintains Max(Var(i) | i in cond)
 * this is a variant that is lazy, and maintains a TODO-list of postponed updates.
 * postponed updates are ones that do not impact on the outout of the invariant.
 * when there is an update, it is first checked against the TODO-list, for cancellation.
 * if the update does not impact the output, it is postponed
 * if it affects the output, it is performed
 * @param varss is an array of IntVar, which can be bulked
 * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * @param default the value if ccond is empty
 * @param maxBackLogSize is the maximal number of postponed updates (TODOlist is handled as a FIFO)
 * update is O(log(n)), faster (O(1L) if you do updates and backtracks
 * @author renaud.delandtsheer@cetic.be
 * */
case class MaxConstArrayLazy(varss: Array[Long], ccond: SetValue, default: Long = Long.MinValue, maxBackLogSize:Long = 10L)
  extends MiaxConstArrayLazy(varss, ccond, default, maxBackLogSize) {

  @inline
  override def Ord(v: Long): Long = -v

  override def checkInternals(c: Checker): Unit = {
    if(ccond.value.isEmpty) c.check(value == default,Some("default"))
    else  c.check(value == varss(ccond.value.maxBy(varss(_))),Some("expected " + varss(ccond.value.maxBy(varss(_))) + " got " + value))
  }

  @inline
  override def equalOrNotImpactingMiax(potentialMiax: Long): Boolean = this.newValue >= potentialMiax
}

/**
 * Maintains Miax(Var(i) | i in cond)
 * Exact ordering is specified by implementing abstract methods of the class.
 * @param vars is an array of IntVar, which can be bulked
 * @param cond is the condition, cannot be null
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
abstract class MiaxConstArray(vars: Array[Long], cond: SetValue, default: Long)
  extends IntInvariant
  with SetNotificationTarget{

  var h: BinomialHeapWithMoveInt = new BinomialHeapWithMoveInt(i => Ord(vars(i)), vars.length, vars.length)

  registerStaticAndDynamicDependency(cond)
  finishInitialization()

  //TODO: restrict domain

  for (i <- cond.value) {
    h.insert(i)
  }

  def Ord(v: Long): Long

  if (h.isEmpty) {
    this := default
  } else {
    this := vars(h.getFirst)
  }

  override def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Long], removedValues: Iterable[Long], oldValue: SortedSet[Long], newValue: SortedSet[Long]): Unit = {
    for (added <- addedValues) notifyInsertOn(v: ChangingSetValue, added)
    for(deleted <- removedValues) notifyDeleteOn(v: ChangingSetValue, deleted)
    if (h.isEmpty) {
      this := default
    } else {
      this := vars(h.getFirst)
    }
  }

  @inline
  def notifyInsertOn(v: ChangingSetValue, value: Long) {
    assert(v == cond)

    //mettre a jour le heap
    h.insert(value)
  }

  @inline
  def notifyDeleteOn(v: ChangingSetValue, value: Long) {
    assert(v == cond)

    //mettre a jour le heap
    h.delete(value)
  }

  override def checkInternals(c: Checker): Unit = {
    if(cond.value.isEmpty) c.check(value == default)
    else c.check(value == cond.value.maxBy(vars(_)))
  }
}

/**
 * Maintains Miax(Var(i) | i in cond)
 * Exact ordering is specified by implementing abstract methods of the class.
 * @param vars is an array of IntVar, which can be bulked
 * @param cond is the condition, cannot be null
 * update is O(log(n)), but probably faster if you do neighborhood exploration with moves and backtracks
 * @author renaud.delandtsheer@cetic.be
 * */
abstract class MiaxConstArrayLazy(vars: Array[Long], cond: SetValue, default: Long, maxBacklog:Long = Long.MaxValue)
  extends IntInvariant
  with SetNotificationTarget{

  //  var nbAnihilation = 0L
  //  var nbDoIt = 0L

  val n = vars.length
  var h: BinomialHeapWithMoveInt = new BinomialHeapWithMoveInt(i => Ord(vars(i)), vars.length, vars.length)

  var backLog:QList[Long] = null
  var backlogSize:Long = 0L
  val isBacklogged:Array[Boolean] = Array.fill(vars.size)(false)
  val consideredValue:Array[Boolean] = Array.fill(vars.size)(false)

  registerStaticAndDynamicDependency(cond)
  finishInitialization()

  def computeMinMax():(Long,Long) = {
    var myMin = default
    var myMax = default
    for (i <- vars) {
      if(i > myMax) myMax = i
      if(i < myMin) myMin = i
    }
    (myMin,myMax)
  }


  for (i <- cond.value) {
    h.insert(i)
    consideredValue(i) = true
  }

  def Ord(v: Long): Long

  @inline
  private[this] def updateFromHeap() {
    if (h.isEmpty) {
      this := default
    } else {
      updateFromNonEmptyHeap()
    }
  }

  @inline
  private[this] def updateFromNonEmptyHeap() {
    this := vars(h.getFirst)
  }

  updateFromHeap()
  restrictDomain(computeMinMax())

  @inline
  def equalOrNotImpactingMiax(potentialMiax:Long):Boolean

  @inline
  private[this] def putIntoBackLog(cond:Long): Unit ={
    if(!isBacklogged(cond)){
      backLog = QList(cond,backLog)
      isBacklogged(cond) = true
      backlogSize += 1L
    }
  }

  /**
   * does not perform final update because was not supposed to be impacted
   */
  @inline
  private[this] def trimBackLog(){
    while(true){
      if(backLog == null) return
      if(!isBacklogged(backLog.head)){
        backlogSize -=1L
        backLog = backLog.tail
      } else if(backlogSize > maxBacklog){
        val condValue = backLog.head
        backlogSize -=1L
        backLog = backLog.tail
        processThisRealBackLog(condValue)
      }else{
        return
      }
    }
  }

  @inline
  private[this] def processThisRealBackLog(condValue:Long): Unit ={
    if(consideredValue(condValue)){ //should be removed
      assert(cond.value.contains(condValue))
      h.delete(condValue)
      consideredValue(condValue) = false
    }else{ //should be added
      assert(!cond.value.contains(condValue))
      h.insert(condValue)
      consideredValue(condValue) = true
    }
    isBacklogged(condValue) = false
  }

  @inline
  private[this] def processBackLog(): Unit ={
    while(backLog != null){
      val condValue = backLog.head
      backLog = backLog.tail
      if(isBacklogged(condValue)) {
        processThisRealBackLog(condValue)
      }
    }
    backlogSize = 0L
  }

  override def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Long], removedValues: Iterable[Long], oldValue: SortedSet[Long], newValue: SortedSet[Long]): Unit = {
    //insert first because reduces chances of flush
    val itAdded = addedValues.iterator
    while(itAdded.hasNext){
      notifyInsertOn(v: ChangingSetValue, itAdded.next())
    }

    val itDeleted = removedValues.iterator
    while(itDeleted.hasNext){
      notifyDeleteOn(v: ChangingSetValue, itDeleted.next())
    }
  }

  @inline
  def notifyInsertOn(v: ChangingSetValue, value: Long) {
    assert(v == cond)
    if(consideredValue(value)){ //anihilation
      assert(isBacklogged(value))
      isBacklogged(value) = false
      return
    }
    if(equalOrNotImpactingMiax(vars(value))){//backlog
      trimBackLog()
      putIntoBackLog(value)
    }else{//impacted
      this := vars(value)
      h.insert(value)
      consideredValue(value) = true
    }
  }

  @inline
  def notifyDeleteOn(v: ChangingSetValue, value: Long) {
    assert(v == cond)
    if(!consideredValue(value)){ //anihilation
      assert(isBacklogged(value))
      isBacklogged(value) = false
      return
    }
    if(this.newValue == vars(value)){//impacted, flush backLog
      processBackLog()
      h.delete(value)
      consideredValue(value) = false
      updateFromHeap()
    }else{//not impacted, backlog
      trimBackLog()
      putIntoBackLog(value)
    }
  }
}
