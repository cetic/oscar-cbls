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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  *            Yoann Guyot
  ******************************************************************************/
package oscar.cbls.lib.invariant.logic

import oscar.cbls.algo.heap.{BinomialHeap, BinomialHeapWithMove}
import oscar.cbls.core.computation.{ChangingIntValue, IntNotificationTarget, IntValue, SetInvariant}
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.SortedSet
import scala.collection.mutable

/**
 * {i in index of values | values[i] <= boundary}
 * It is based on two heap data structure, hence updates are log(n) and all updates are allowed
 * @param values an array of [[oscar.cbls.core.computation.CBLSIntVar]]
 * @param boundary the boundary for comparison
 * @author renaud.delandtsheer@cetic.be
 * */
case class SelectLEHeapHeap(values: Array[IntValue], boundary: IntValue)
  extends SetInvariant(SortedSet.empty[Int], values.indices.start to values.indices.end)
  with IntNotificationTarget{

  for (v <- values.indices) registerStaticAndDynamicDependency(values(v), v)
  registerStaticAndDynamicDependency(boundary)
  finishInitialization()

  val HeapAbove: BinomialHeapWithMove[Int] = new BinomialHeapWithMove((i: Int) => values(i).value, values.size)
  val HeapBelowOrEqual: BinomialHeapWithMove[Int] = new BinomialHeapWithMove((i: Int) => -values(i).value, values.size)

  for(v <- values.indices){
    if(values(v).value <= boundary.value){
      HeapBelowOrEqual.insert(v)
      this.insertValue(v)
    } else {
      HeapAbove.insert(v)
    }
  }

  //pomper des elements de Above et les mettre dans Below et dans output
  @inline
  def TransferToBelow(): Unit = {
    while (!HeapAbove.isEmpty && values(HeapAbove.getFirst).value <= boundary.value) {
      val v = HeapAbove.removeFirst()
      HeapBelowOrEqual.insert(v)
      this.insertValue(v)
    }
  }

  //pomper des elements de Above et les mettre dans Below et dans output
  @inline
  def TransferToAbove(): Unit = {
    //pomper des elements de beloworequal et les mettre dans above
    while (!HeapBelowOrEqual.isEmpty && values(HeapBelowOrEqual.getFirst).value > boundary.value) {
      val v = HeapBelowOrEqual.removeFirst()
      HeapAbove.insert(v)
      this.deleteValue(v)
    }
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, i: Int, OldVal: Long, NewVal: Long): Unit = {
    if (v == boundary) {
      //c'est le boundary
      if (NewVal > OldVal) {
        TransferToBelow()
      } else { TransferToAbove() }
    } else {
      if (OldVal <= boundary.value) {
        //il est dans BelowOrEqual
        HeapBelowOrEqual.notifyChange(i)
        TransferToAbove()
      } else {
        HeapAbove.notifyChange(i)
        TransferToBelow()
      }
    }
  }

  override def checkInternals(c: Checker): Unit = {
    for (v <- this.value) {
      c.check(values(v).value <= boundary.value,
        Some(s"values($v).value (${values(v).value}) <= boundary.value (${boundary.value})"))
    }
    var count: Long = 0L
    for (v <- values) {
      if (v.value <= boundary.value)
        count += 1L
    }
    c.check(count == this.value.size,
      Some(s"count ($count) == output.value.size (${this.value.size})"))
    c.check(HeapAbove.size + HeapBelowOrEqual.size == values.length,
      Some(s"HeapAbove.size + HeapBelowOrEqual.size (${HeapAbove.size}+${HeapBelowOrEqual.size}) == values.size (${values.length})"))
  }
}

/**
 * {i \in index of values | values[i] <= boundary}
 * It is based on a queue for the values above the boundary, hence all updates must be accepted by this scheme:
 * - SelectLESetQueue does not allow boundary to decrease
 * - SelectLESetQueue does not allow elements above boundary to change
 * - SelectLESetQueue requires latest variables passing above boundary to be the biggest one
 * @param values: an array of intvar
 * @param boundary: the boundary for comparison
 * @author renaud.delandtsheer@cetic.be
 * */
case class SelectLESetQueue[X<:IntValue](values: Array[X], boundary: IntValue)
  extends SetInvariant(initialDomain = values.indices.start to values.indices.end)
  with IntNotificationTarget{

  for (v <- values.indices) registerStaticAndDynamicDependency(values(v), v)
  registerStaticAndDynamicDependency(boundary)
  finishInitialization()

  val QueueAbove: mutable.Queue[Int] = new mutable.Queue[Int]

  this := SortedSet.empty[Int]
  val HeapAbove: BinomialHeap[Int] = new BinomialHeap((i: Int) => values(i).value, values.size)
  for (v <- values.indices) {
    if (values(v).value <= boundary.value) {
      this.insertValue(v)
    } else {
      HeapAbove.insert(v)
    }
  }
  while (!HeapAbove.isEmpty) {
    QueueAbove.enqueue(HeapAbove.popFirst())
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Long, NewVal: Long): Unit = {
    if (v == boundary) {
      //c'est le boundary
      assert(NewVal > OldVal, "SelectLESetQueue does not allow boundary to decrease")
      while (QueueAbove.nonEmpty && values(QueueAbove.head).value <= boundary.value) {
        val v = QueueAbove.dequeue()
        this.insertValue(v)
      }
    } else { //il est dans BelowOrEqual
      //     println("SelectLEnotify " + v + " index: " + index +  " OldVal: " + OldVal + " NewVal: " + NewVal + " boundary: " + boundary + " this " + this)
      assert(OldVal <= boundary.value,
        s"SelectLESetQueue does not allow elements above boundary to change: $v(new: $NewVal, old: $OldVal) pivot: $boundary")
      assert(QueueAbove.isEmpty || values(QueueAbove.last).value <= NewVal,
        s"SelectLESetQueue requires latest variables passing above boundary to be the biggest one: $v")
      QueueAbove.enqueue(index)
      this.deleteValue(index)
    }
  }

  override def checkInternals(c: Checker): Unit = {
    var count: Long = 0L
    for (i <- values.indices) {
      if (values(i).value <= boundary.value) {
        c.check(this.value.contains(i), Some(s"this.value.contains($i)"))
        count += 1L
      }
    }
    c.check(this.value.size == count,
      Some(s"this.value.size (${this.value.size}) == count ($count)"))
  }
}
