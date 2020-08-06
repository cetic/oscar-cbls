/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cbls.lib.invariant.logic

import oscar.cbls.core.computation.{CBLSIntVar, ChangingIntValue, Domain, DomainRange, IntInvariant, IntNotificationTarget, IntValue, Invariant, InvariantHelper, ShortIntNotificationTarget, Store}
import oscar.cbls.core.propagation.Checker

/**
 * Author: Jean-Noël Monette
 */
case class SparseCount(values: Array[IntValue], counts: Map[Long,CBLSIntVar])
  extends Invariant
  with IntNotificationTarget{

  for (v <- values.indices) registerStaticAndDynamicDependency(values(v), v)
  
  finishInitialization()
  for (count <- counts.values) { count := 0L }
  for (v <- values.indices){
    counts.get(values(v).value).foreach(c => c :+= 1L)
  }
  for(c <- counts.values) c.setDefiningInvariant(this)
    
  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Long, NewVal: Long): Unit = {
    assert(values(index) == v)
    counts.get(OldVal).foreach(c => c :-= 1L)
    counts.get(NewVal).foreach(c => c :+= 1L)
  }
}

/**
 * Maintains a count of the indexes of array: count(j) = #{i in index of values | values[i]+offset == j}
 * This is considered as a dense count because counts is an array and must cover all the possibles values of the values in the array ''values''
 *
 * it is expected that the values are always >= 0L
 * @author renaud.delandtsheer@cetic.be
 * */
case class DenseCount(values: Array[IntValue], counts: Array[CBLSIntVar], offset:Int = 0)
  extends Invariant
  with ShortIntNotificationTarget{

  for (v <- values.indices) registerStaticAndDynamicDependency(values(v), v)

  for (count <- counts) { count := 0L }

  for (v <- values.indices) {
    counts(values(v).valueInt+offset) :+= 1L
  }

  finishInitialization()

  for (c <- counts) { c.setDefiningInvariant(this) }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Int, NewVal: Int): Unit = {
    assert(values(index) == v)
    counts(OldVal + offset) :-= 1L
    counts(NewVal + offset) :+= 1L
  }

  override def checkInternals(c: Checker): Unit = {
    /**
     * Maintains a count of the indexes of array:
     * count(j) = #{i in index of values | values[i] == j}
     * This is considered as a dense count because counts is
     * an array and must cover all the possibles values of the values
     * in the array ''values''
     */
    val myCounts = Array.fill[Long](counts.length)(0L)
    for (i <- values.indices) {
      val v = values(i).valueInt
      myCounts(v+offset) = myCounts(v+offset) + 1L
    }

    for (j <- counts.indices) {
      c.check(counts(j+offset).value == myCounts(j+offset),
        Some(s"counts($j+offset).getValue(false) (${counts(j+offset).value}) == myCounts($j+offset) (${myCounts(j+offset)})"))
    }
  }
}

/**
  * Maintains the number of occurances of c \in values
  *
  * @author gustav.bjordal@it.uu.se
  * */
case class ConstCount(values: Array[IntValue], c: Long)
  extends IntInvariant(values.count(v => v.value == c), DomainRange(0L,values.length))
    with IntNotificationTarget{

  registerStaticAndDynamicDependencyArrayIndex(values)

  finishInitialization()

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Long, NewVal: Long): Unit = {
    if(NewVal == c){
      this :+= 1L
    }else if(OldVal == c){
      this :-= 1L
    }
  }

  override def checkInternals(c: Checker): Unit = {
  }
}

/**
 * Maintains the number of values in the array that are acceptad by the condition
 *
 * @author renaud.delandtsheeer@cetic.be
 * */
case class Count(values: Array[IntValue], condition:Long=>Boolean = _!=0)
  extends IntInvariant(values.count(v => condition(v.value)), DomainRange(0L,values.length))
    with IntNotificationTarget{

  registerStaticAndDynamicDependencyArrayIndex(values)

  finishInitialization()

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Long, NewVal: Long): Unit = {
    if(condition(NewVal) && !condition(OldVal)) {
      this :+= 1L
    }else if(!condition(NewVal) && condition(OldVal)){
      this :-= 1L
    }
  }

  override def checkInternals(c: Checker): Unit = {
    require(values.count(v => condition(v.value)) == this.value)
  }
}

object DenseCount{
  def makeDenseCount(vars: Array[IntValue]):DenseCount = {
    val (minMin,maxMax) = InvariantHelper.getMinMaxBoundsShort(vars)
    val mbValues = maxMax - minMin + 1
    val m:Store = InvariantHelper.findModel(vars)
    val nbVars = vars.length
    val counts = Array.tabulate(mbValues)(i => CBLSIntVar(m,0L, Domain(0L , nbVars), "count_" + (i-minMin)))
    DenseCount(vars,counts,-minMin)
  }
}
