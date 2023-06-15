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

/** This binary heap implementation is dedicated to problem where it's highly likely to have
  * multiple elements of the same priority.
  *
  * The heap is actually made of an array, storing lists containing items with same priority. A
  * binary heap is maintained to record the lowest priority in the heap. This is more efficient if
  * it often occurs that elements have the same priority. priority is assumed to start at zero.
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

  override def isEmpty: Boolean = binaryHeap.isEmpty

  override def size: Int = priorityToElements.map(_.size).sum

  override def dropAll(): Unit = {
    for (i <- binaryHeap) priorityToElements(i) = List.empty
    binaryHeap.dropAll()
  }

  override def insert(elem: T): Unit = {
    val priority              = priorityFunction(elem)
    val otherWithSamePriority = priorityToElements(priority)
    if (otherWithSamePriority.isEmpty) {
      priorityToElements(priority) = List(elem)
      binaryHeap.insert(priority)
    } else {
      priorityToElements(priority) = elem :: otherWithSamePriority
    }
  }

  override def getFirst: T = priorityToElements(binaryHeap.getFirst).head

  override def getFirsts: List[T] = {
    priorityToElements(binaryHeap.getFirst)
  }

  override def popFirst(): T = {
    val position = binaryHeap.getFirst
    val list     = priorityToElements(position)
    val toReturn = list.head
    priorityToElements(position) = list.tail
    if (list.tail.isEmpty) {
      binaryHeap.popFirst()
    }
    toReturn
  }

  override def popFirsts: List[T] = {
    val position = binaryHeap.popFirst()
    val toReturn = priorityToElements(position)
    priorityToElements(position) = List.empty
    toReturn
  }

  override def iterator: Iterator[T] = {
    var acc: List[T] = List()
    for (position <- binaryHeap) {
      var curr = priorityToElements(position)
      while (curr != List.empty) {
        acc = curr.head :: acc
        curr = curr.tail
      }
    }
    acc.iterator
  }
}
