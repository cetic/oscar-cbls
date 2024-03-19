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

import scala.collection.AbstractIterator
import scala.collection.immutable.SortedSet
import scala.collection.mutable.{PriorityQueue => PQ}

/** This object encapsulates a set of methods used to enable hot restart while iterating over an
  * iterable collection of integers. Given such an iterable and an integer pivot, hot restart
  * ensures that iteration first occurs on values greater than or equal to the pivot, in increasing
  * order, followed by the values smaller than the pivot. If the collection does not contain the
  * pivot, the elements will simply be returned in ascending order.
  *
  * If the `preserveSequence` method is used, the elements will instead be returned in the order of
  * the original collection, starting from the first occurrence of the pivot element, if present,
  * and from the first element returned by its original iterator otherwise.
  */
object HotRestart {

  /** Default way to use HotRestart, returning elements in increasing order.
    * @param it
    *   the collection of elements
    * @param pivot
    *   the pivot element
    * @return
    *   an iterable over the elements of the original collection with the hot restart property
    */
  def apply(it: Iterable[Int], pivot: Int): Iterable[Int] = {
    if (it.isEmpty) it
    else
      it match {
        case r: Range =>
          require(r.step == 1, "Only a range step of 1 is currently supported")
          if (r contains pivot)
            new ShiftedRange(r.start, r.last, pivot, r.step)
          else r
        case s: SortedSet[Int] => new ShiftedSet(s, pivot)
        case _                 => new ShiftedIterable(it, pivot)
      }
  }

  /** This method returns elements in the given collection starting from the first occurrence of the
    * pivot, while respecting the ordering of its original iterator.
    * @param it
    *   the collection of elements
    * @param pivot
    *   the pivot element
    * @return
    *   an iterable over the collection from the pivot with original ordering
    */
  def preserveSequence(it: Iterable[Int], pivot: Int) =
    new ShiftedIterable(it, pivot, true)
}

/** Class handling hot restart over a numeric range. Only 1-step ranges are supported.
  */
protected[search] class ShiftedRange(
  val start: Int,
  val end: Int,
  val startBy: Int,
  val step: Int = 1
) extends Iterable[Int] {
  assert(start <= startBy && startBy <= end, "ShiftedRange must contain startBy value")
  assert(step == 1, "only step of 1 is currently supported in ShiftedRange")

  override def iterator: Iterator[Int] = new AbstractIterator[Int] {
    private var currentValue: Int = startBy
    private var stop              = false

    private def getNextValue(a: Int): Int = {
      if (a == end) start
      else a + 1
    }

    def hasNext: Boolean = !stop

    def next(): Int = {
      if (stop) Iterator.empty.next()
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

/** Class handling hot restart over a sorted set. */
protected[search] class ShiftedSet(s: SortedSet[Int], pivot: Int) extends Iterable[Int] {

  override def iterator: Iterator[Int] = new AbstractIterator[Int] {
    private var it: Iterator[Int] = s.iteratorFrom(pivot)
    private var onFirstIterator   = true
    private var currentValue: Int = 0
    private var currentValueReady = false

    // Maintains which iterator is in use, the next value in the iterator if available,
    // and returns true iff there is a next value
    override def hasNext: Boolean = {
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

    override def next(): Int = {
      if (!hasNext) Iterator.empty.next()
      currentValueReady = false
      currentValue
    }
  }
}

/** Class handling hot restart over generic collections. Handles the case when `preserveSequence` is
  * invoked.
  */
protected[search] class ShiftedIterable(it: Iterable[Int], pivot: Int, sequence: Boolean = false)
    extends Iterable[Int] {

  override def iterator: Iterator[Int] = {
    if (sequence) {
      // if order matters, just convert the iterator output to a list,
      // then split at pivot and concatenate
      val l = it.iterator.toList
      (l.dropWhile(_ != pivot) ::: l.takeWhile(_ != pivot)).iterator
    } else {
      // otherwise put elements in two min-priority queues
      // and have the iterator dequeue them in order
      // using Long conversion to handle appearance of Int.MinValue
      val (pqAbove, pqBelow) = (PQ[Long]()(Ordering.by(-_)), PQ[Long]()(Ordering.by(-_)))
      it.foreach(i => if (i >= pivot) pqAbove.enqueue(i) else pqBelow.enqueue(i))
      Iterator.tabulate(it.size)(_ =>
        (if (pqAbove.nonEmpty) pqAbove.dequeue() else pqBelow.dequeue()).toInt
      )
    }
  }
}
