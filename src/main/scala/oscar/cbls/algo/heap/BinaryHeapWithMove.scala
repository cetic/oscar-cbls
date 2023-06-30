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

import scala.collection.immutable.SortedMap

object BinaryHeapWithMove {
  def apply[T](priorityFunction: T => Long, maxSize: Int)(implicit
    o: Ordering[T],
    m: Manifest[T]
  ): BinaryHeapWithMove[T] = {
    new BinaryHeapWithMove[T](priorityFunction, maxSize)
  }
}

/** A mutable binary heap.
  *
  * By worsening a bit it's efficiency, it offers additional operations very useful for propagation.
  *
  * @param priorityFunction
  *   returns the priority value as a [[scala.Long]] of an element [[T]]
  * @param maxSize
  *   maximum size of the heap
  * @param m
  *   manifest of T, to create arrays of T's
  * @tparam T
  *   type of elements in the heap
  * @author
  *   renaud.delandtsheer@cetic.be
  */
class BinaryHeapWithMove[T](priorityFunction: T => Long, override val maxSize: Int)(
  implicit val o: Ordering[T],
  override implicit val m: Manifest[T]
) extends BinaryHeap[T](priorityFunction, maxSize) {
  // Stores the position of each item
  private var itemsPosition: SortedMap[T, Int] = SortedMap.empty

  override def withPriorityFunction(priorityFunction: T => Long): BinaryHeapWithMove[T] = {
    val copy         = new BinaryHeapWithMove[T](priorityFunction, maxSize)
    val heapIterator = iterator
    while (heapIterator.hasNext) copy.insert(heapIterator.next())
    copy
  }

  def contains(value: T): Boolean = itemsPosition.contains(value)

  /** Notifies that one element of the heap has changed.
    *
    * @param elem
    *   The element whose internal state has changed.
    */
  def notifyChange(elem: T): Unit = {
    require(itemsPosition.contains(elem), s"Item $elem is not in the heap")
    bubbleDown(bubbleUp(itemsPosition(elem))) // Moves it to the right place
  }

  /** Removes a specific element from the heap
    *
    * @param elem
    *   the element to remove
    * @return
    *   Whether or not an element has been removed
    */
  def removeElement(elem: T): Boolean = {
    itemsPosition.get(elem) match {
      case None => false
      case Some(elemPosition) =>
        if (elemPosition == size - 1) {
          currentSize -= 1
          itemsPosition -= elem
        } else {
          swapPositions(elemPosition, size - 1)
          currentSize -= 1
          itemsPosition -= elem
          // Swapped item not necessarily at top so bubble up and down
          bubbleDown(bubbleUp(elemPosition))
        }
        true
    }
  }

  /** Gets all the elements present in the heap */
  def getElements: Iterable[T] = {
    itemsPosition.keys
  }

  override def dropAll(): Unit = {
    itemsPosition = itemsPosition.empty
    super.dropAll()
  }

  override def swapPositions(position1: Int, position2: Int): Unit = {
    itemsPosition += ((heapArray(position1), position2))
    itemsPosition += ((heapArray(position2), position1))
    super.swapPositions(position1, position2)
  }

  override def insert(elem: T): Unit = {
    require(!itemsPosition.contains(elem), s"Can't add the same element twice !")
    itemsPosition += ((elem, size))
    super.insert(elem)
  }

  override def popFirst(): Option[T] = {
    super.popFirst() match {
      case None => None
      case Some(item) =>
        itemsPosition -= item
        Some(item)
    }
  }

  /** Checks if the state of the heap is correct. */
  def checkInternals(): Unit = {
    require(
      heapArray.take(size).distinct.length == size,
      "Heap error : there are multiple times the same elements, it's not tolerated"
    )
    for (i <- heapArray.indices if i < size - 1) {
      if (leftChild(i) < size) {
        require(
          priorityFunction(heapArray(i)) <= priorityFunction(heapArray(leftChild(i))),
          s"heap error : Priority of ${heapArray(leftChild(i))} should be higher or equal to ${priorityFunction(
              heapArray(i)
            )} got ${priorityFunction(heapArray(leftChild(i)))}\n Heap Array : $this\n Indices : $i"
        )
        require(father(leftChild(i)) == i, "heap error " + this)
      }
      if (rightChild(i) < size) {
        require(
          priorityFunction(heapArray(i)) <= priorityFunction(heapArray(rightChild(i))),
          s"heap error : Priority of ${heapArray(rightChild(i))} should be higher or equal to ${priorityFunction(
              heapArray(i)
            )} got ${priorityFunction(heapArray(rightChild(i)))}\n Heap Array : $this\n Indices : $i"
        )
        require(father(rightChild(i)) == i, "heap error " + this)
      }
    }

    for (t <- itemsPosition.keys) {
      assert(
        heapArray(itemsPosition(t)) == t,
        s"Item $t position = ${itemsPosition(t)} should be ${heapArray.indexOf(t)}"
      )
    }
  }

  override def toString: String = {
    heapArray.iterator.toList.mkString("{", ",", "}")
  }
}
