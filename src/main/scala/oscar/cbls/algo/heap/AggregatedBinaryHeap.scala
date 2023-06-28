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

package oscar.cbls.algo.heap

import scala.language.implicitConversions

object AggregatedBinaryHeap {
  def apply[T](priorityFunction: T => Int, maxPriority: Int): AggregatedBinaryHeap[T] = {
    new AggregatedBinaryHeap[T](priorityFunction, maxPriority)
  }
}

/** This binary heap implementation is dedicated to problem where it's highly likely to have
  * multiple elements of the same priority.
  *
  * The heap is actually made of an array, storing lists containing items with same priority. A
  * binary heap is maintained to record the lowest priority in the heap. This is more efficient if
  * it often occurs that elements have the same priority. Priority is assumed to start at zero.
  *
  * @param priorityFunction
  *   The function used to determine the priority of an element
  * @param maxPriority
  *   The maximum priority value of the heap
  * @author
  *   renaud.delandtsheer@cetic.be
  */
class AggregatedBinaryHeap[T](priorityFunction: T => Int, val maxPriority: Int)
    extends AbstractHeap[T] {

  // The binary heap maintaining the lowest priority value at the head
  private[this] val binaryHeap = new BinaryHeap[Int](x => x, maxPriority)

  // The array that store the elements at their priority value
  private[this] val priorityToElements: Array[List[T]] =
    Array.tabulate(maxPriority)(_ => List.empty)

  /** Create a new BinaryHeap with a new priorityFunction. Then add all the element of this heap to
    * the new one.
    *
    * @param priorityFunction
    *   The new priority function
    * @return
    *   A BinaryHeap with the new priority function and all elements of this heap.
    */
  def withPriorityFunction(priorityFunction: T => Int): AggregatedBinaryHeap[T] = {
    val copy         = new AggregatedBinaryHeap[T](priorityFunction, maxPriority)
    val heapIterator = iterator
    while (heapIterator.hasNext) copy.insert(heapIterator.next())
    copy
  }

  override def isEmpty: Boolean = binaryHeap.isEmpty

  override def size: Int = priorityToElements.map(_.size).sum

  override def dropAll(): Unit = {
    for (i <- binaryHeap) priorityToElements(i) = List.empty
    binaryHeap.dropAll()
  }

  override def insert(elem: T): Unit = {
    require(
      priorityFunction(elem) < maxPriority,
      s"The priority value of this element exceed the maximum priority value allowed. ${priorityFunction(elem)} has to be lower than $maxPriority"
    )
    val priority              = priorityFunction(elem)
    val otherWithSamePriority = priorityToElements(priority)
    if (otherWithSamePriority.isEmpty) {
      priorityToElements(priority) = List(elem)
      binaryHeap.insert(priority)
    } else {
      priorityToElements(priority) = elem :: otherWithSamePriority
    }
  }

  override def getFirst: Option[T] =
    binaryHeap.getFirst match {
      case None => None
      case Some(priority) =>
        Some(priorityToElements(priority).head)
    }

  override def getFirsts: List[T] =
    binaryHeap.getFirst match {
      case None => List.empty
      case Some(priority) =>
        priorityToElements(priority)
    }

  override def popFirst(): Option[T] = {
    binaryHeap.getFirst match {
      case None => None
      case Some(priority) =>
        val list = priorityToElements(priority)
        val item = list.head
        priorityToElements(priority) = list.tail
        if (list.tail.isEmpty) {
          binaryHeap.popFirst()
        }
        Some(item)
    }

  }

  override def popFirsts(): List[T] = {
    binaryHeap.popFirst() match {
      case None => List.empty
      case Some(priority) =>
        val list = priorityToElements(priority)
        priorityToElements(priority) = List.empty
        list
    }
  }

  override def iterator: Iterator[T] = {
    binaryHeap.iterator.flatMap(priorityToElements)
  }
}
