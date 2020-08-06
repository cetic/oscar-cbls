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
package oscar.cbls.algo.search

import scala.collection.Iterator
import scala.collection.immutable.{NumericRange, SortedSet}

/**
 * this proposes a set of methods to enable hot restart on iteration over an iterable.
 * it takes an Iterable[Long] and some pivot, and ensures that the iteration will explore
 * the values above the pivot first, in increasing order,
 * and the values below the pivot later, in increasing order as well.
*/
object HotRestart {

  /** this will return a shiftedIterable
    * the most efficient method will be automatically selected for Range and sorted sets
    * @param it
    * @param pivot
    * @return
    */
  def apply(it:Iterable[Int], pivot:Int):Iterable[Int] = {
    it match{
      case r:NumericRange[Int] => if (r contains pivot) new InstrumentedRange(r) startBy pivot else r
      case s:SortedSet[Int] => new ShiftedSet(s,pivot)
      case _ => new ShiftedIterable(it, pivot)
    }
  }

  def hotRestartPreserveSequence(it:Iterable[Int],pivot:Int) = new ShiftedIterable(it,pivot,true)

  def apply(r:NumericRange[Int], pivot:Int):Iterable[Int] =  if (r contains pivot) new InstrumentedRange(r) startBy pivot else r

  def apply(s:SortedSet[Int], pivot:Int):Iterable[Int] =  new ShiftedSet(s,pivot)
}

class ShiftedIterable(it:Iterable[Int], pivot:Int, sequence:Boolean = false) extends Iterable[Int] {
  override def iterator: Iterator[Int] = {
    if(sequence) {
      //splitting into two parts:
      val aboveIterator = it.iterator
      def fetchHead: List[Int] = {
        if (aboveIterator.hasNext) {
          val nextValue = aboveIterator.next()
          if (nextValue == pivot) List(nextValue)
           else nextValue :: fetchHead
        }else Nil
      }
      val below = fetchHead
      new ShiftedIterator(aboveIterator.toList, below)
    }else {
      val (above, below) = it.partition(i => i > pivot)
      new ShiftedIterator(above, below)
    }
  }

  class ShiftedIterator[@specialized(Int,Long) T](first:Iterable[T], var second:Iterable[T]) extends Iterator[T]{
    //TODO: this is awful: maybe the stuff is already sorted
    //TODO: we should perform a lazy sort since all the first might not be covered anyway
    var it:Iterator[T] = first.toList.iterator
    override def hasNext: Boolean = {
      if(it.hasNext) true
      else if (second == null) false
      else{
        it = second.toList.iterator
        second = null
        it.hasNext
      }
    }

    override def next(): T = it.next()
  }
}

class InstrumentedRange(r:NumericRange[Int]){
  def startBy (pivot:Int) = if (r contains pivot) new ShiftedRange(r.head, r.last,pivot, r.step) else r
}

/**
 * this is an inclusive range.
 * @param start
 * @param end
 * @param startBy
 * @param step
 */
class ShiftedRange(val start:Int, val end:Int, val startBy:Int, val step:Int = 1) extends Iterable[Int]{
  if((start > startBy) || (startBy > end)) throw new Exception("ShiftedRange must contain startBy value ")
  if(step != 1) throw new Exception("only step of 1L is supported in ShiftedRange")

  def getNextValue(a:Int): Int = {
    if(a == end) start
    else a+1
  }

  override def iterator: Iterator[Int] = new ShiftedRangeIterator(this)

  override def toArray[B >: Int](implicit evidence$1L: scala.reflect.ClassTag[B]): Array[B] = toList.toArray

  override def toString(): String = "ShiftedRange(" + toList + ")"


  class ShiftedRangeIterator(val s:ShiftedRange) extends Iterator[Int]{
    var currentValue: Int = s.startBy
    var hasNext = true

    def next(): Int = {
      val tmp = currentValue
      currentValue = s.getNextValue(currentValue)
      if(currentValue == s.startBy) hasNext = false
      tmp
    }
  }
}

class ShiftedSet(s:SortedSet[Int], pivot:Int) extends Iterable[Int] {
  override def iterator: Iterator[Int] = {
    new ShiftedIterator(s, pivot)
  }

  class ShiftedIterator(s:SortedSet[Int], pivot:Int) extends Iterator[Int]{
    var it:Iterator[Int] = s.iteratorFrom(pivot)
    var first=true
    var currentValue:Int = 0
    var currentValueReady = false

    /** returns true if a next value is available
      *
      * @return
      */
    def internalMoveToNext():Boolean = {
      if(currentValueReady) return true
      if(first){
        if(it.hasNext) {
          currentValue = it.next()
          currentValueReady = true
          return true
        }else{
          //start the second iterator
          it = s.iterator
          first = false
          //and continue the execution flow
        }
      }
      //second iterator
      if(!it.hasNext) return false
      currentValue = it.next()
      if(currentValue >= pivot) return false
      currentValueReady = true
      true
    }

    override def hasNext: Boolean = internalMoveToNext()

    override def next(): Int = {
      if(!internalMoveToNext()) throw new Error("no more elements to iterate")
      currentValueReady = false
      currentValue
    }
  }
}
