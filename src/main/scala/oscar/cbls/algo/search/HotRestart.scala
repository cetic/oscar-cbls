// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.algo.search

import scala.collection.immutable.{NumericRange, SortedSet}

/** this proposes a set of methods to enable hot restart on iteration over an iterable. it takes an
  * Iterable[Long] and some pivot, and ensures that the iteration will explore the values above the
  * pivot first, in increasing order, and the values below the pivot later, in increasing order as
  * well.
  */
object HotRestart {

  /** this will return a shiftedIterable the most efficient method will be automatically selected
    * for Range and sorted sets
    * @param it
    * @param pivot
    * @return
    */
  def apply(it: Iterable[Int], pivot: Int): Iterable[Int] = {
    it match {
      case r: Range =>
        require(r.step == 1, "Only a range step of 1 is currently supported")
        if (r contains pivot)
          new ShiftedRange(r.head, r.last, pivot, r.step)
        else r
      case s: SortedSet[Int] => new ShiftedSet(s, pivot)
      case _                 => new ShiftedIterable(it, pivot)
    }
  }

  def preserveSequence(it: Iterable[Int], pivot: Int) =
    new ShiftedIterable(it, pivot, true)
}

/** this is an inclusive range.
  * @param start
  * @param end
  * @param startBy
  * @param step
  */
class ShiftedRange(val start: Int, val end: Int, val startBy: Int, val step: Int = 1)
    extends Iterable[Int] {
  assert(start <= startBy && startBy <= end, "ShiftedRange must contain startBy value")
  assert(step == 1, "only step of 1L is currently supported in ShiftedRange")

  override def iterator: Iterator[Int] = new AbstractIterator[Int] {
    private var currentValue: Int = startBy
    private var stop              = false

    private def getNextValue(a: Int): Int = {
      if (a == end) start
      else a + 1
    }

    def hasNext: Boolean = !stop

    def next(): Int = {
      val tmp = currentValue
      currentValue = getNextValue(currentValue)
      if (currentValue == startBy) stop = true
      tmp
    }
  }

  override def toArray[B >: Int](implicit evidence$1L: scala.reflect.ClassTag[B]): Array[B] =
    toList.toArray

  override def toString(): String = "ShiftedRange(" + toList + ")"
}

class ShiftedSet(s: SortedSet[Int], pivot: Int) extends Iterable[Int] {

  override def iterator: Iterator[Int] = new AbstractIterator[Int] {
    private var it: Iterator[Int] = s.iteratorFrom(pivot)
    private var onFirstIterator   = true
    private var currentValue: Int = 0
    private var currentValueReady = false

    // maintains which iterator is in use, the next value in the iterator if available,
    // and returns true iff there is a next value
    private def internalMoveToNext(): Boolean = {
      if (currentValueReady) return true
      if (onFirstIterator) {
        if (it.hasNext) {
          currentValue = it.next()
          currentValueReady = true
          return true
        } else {
          // start the second iterator
          it = s.iterator
          onFirstIterator = false
          // and continue the execution flow
        }
      }
      // second iterator
      if (!it.hasNext) return false
      currentValue = it.next()
      if (currentValue >= pivot) return false
      currentValueReady = true
      true
    }

    override def hasNext: Boolean = internalMoveToNext()

    override def next(): Int = {
      if (!internalMoveToNext()) Iterator.empty.next()
      currentValueReady = false
      currentValue
    }
  }
}

class ShiftedIterable(it: Iterable[Int], pivot: Int, sequence: Boolean = false)
    extends Iterable[Int] {
  override def iterator: Iterator[Int] = {
    if (sequence) {
      val l = it.iterator.toList
      (l.dropWhile(_ != pivot) ::: l.takeWhile(_ != pivot)).iterator
    } else {
      val (pqAbove, pqBelow) = (PQ[Int]()(Ordering.by(-_)), PQ[Int]()(Ordering.by(-_)))
      it.foreach(i => if (i >= pivot) pqAbove.enqueue(i) else pqBelow.enqueue(i))
      Iterator.tabulate(it.size)(_ =>
        if (pqAbove.nonEmpty) pqAbove.dequeue() else pqBelow.dequeue()
      )
    }
  }
}
