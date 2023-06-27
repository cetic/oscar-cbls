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

import scala.annotation.tailrec

object BinaryHeap {
  def apply[T](priorityFunction: T => Long, maxSize: Int)(implicit
    m: Manifest[T]
  ): BinaryHeap[T] = {
    new BinaryHeap[T](priorityFunction, maxSize)
  }
}

/** This is an implementation of a binary min-heap.
  *
  * It maintains the order of the heap such that the first element is the one with the minimal
  * priority value. All operations are in O(log(n)).
  *
  * @param priorityFunction
  *   a function that returns an integer for each element inserted in the heap this value is used to
  *   sort the heap content
  * @param maxSize
  *   the maximum number of elements that can be inserted in this heap
  * @param m
  *   the manifest of T, to create arrays of T's
  * @tparam T
  *   the type of elements included in the heap
  * @author
  *   renaud.delandtsheer@cetic.be, fabian.germeau@cetic.be
  */
class BinaryHeap[T](priorityFunction: T => Long, val maxSize: Int)(implicit val m: Manifest[T])
    extends AbstractHeap[T] {

  override def size: Int        = currentSize
  override def isEmpty: Boolean = currentSize == 0L

  // Keep the value of the heap. Initialized at max size for performance
  protected val heapArray: Array[T] = new Array[T](maxSize)
  // Keep the real size of the heap, aka the number of elements to consider in the array
  protected var currentSize: Int = 0

  def withPriorityFunction(priorityFunction: T => Long): BinaryHeap[T] = {
    val copy         = new BinaryHeap[T](priorityFunction, maxSize)
    val heapIterator = iterator
    while (heapIterator.hasNext) {
      val x = heapIterator.next()
      copy.insert(x)
    }
    copy
  }

  @inline protected def leftChild(position: Int): Int  = (position + 1) * 2 - 1
  @inline protected def rightChild(position: Int): Int = (position + 1) * 2
  @inline protected def father(position: Int): Int     = (position - 1) / 2

  /** Swap the position of two element in the heapArray.
    * @param position1
    *   The first position
    * @param position2
    *   The second position
    */
  protected def swapPositions(position1: Int, position2: Int): Unit = {
    val tmp: T = heapArray(position1)
    heapArray(position1) = heapArray(position2)
    heapArray(position2) = tmp
  }

  /** Restore the state of the heap by moving up as much as possible the element at the defined
    * position.
    *
    * It simply compares the priority of the element with his father's one.
    *   - If father > element ==> switch the position of the father and the element
    *   - Else ==> we are done
    * @param startPosition
    *   the position defining the element we want to bubble up
    * @return
    *   the new position of the element
    */
  protected def bubbleUp(startPosition: Int): Int = {
    var position   = startPosition
    var bubblingUp = true
    while (bubblingUp) {
      val priority: Long       = priorityFunction(heapArray(position))
      val fatherPosition: Int  = father(position)
      val fatherPriority: Long = priorityFunction(heapArray(fatherPosition))
      if (fatherPosition >= 0 && priority < fatherPriority) {
        swapPositions(position, fatherPosition)
        position = fatherPosition
      } else {
        bubblingUp = false
      }
    }
    position
  }

  /** Restore the state of the heap by moving down as much as possible the element at the defined
    * position.
    *
    * It simply compares the priority of the element with his left and right child's one.
    *   - If left < (right && element) ==> switch left and element
    *   - If right < (left && element) ==> switch right and element
    *   - Else ==> we are done
    *
    * @param startPosition
    *   the position defining the element we want to bubble down
    * @return
    *   the new position of the element
    */
  protected def bubbleDown(startPosition: Int = 0): Int = {
    var position     = startPosition
    var bubblingDown = true
    while (bubblingDown) {
      val leftChildPosition       = leftChild(position)
      val rightChildPosition      = rightChild(position)
      val priority                = priorityFunction(heapArray(position))
      lazy val leftChildPriority  = priorityFunction(heapArray(leftChildPosition))
      lazy val rightChildPriority = priorityFunction(heapArray(rightChildPosition))
      if (leftChildPosition < currentSize && priority > leftChildPriority) {
        if (rightChildPosition < currentSize && rightChildPriority < leftChildPriority) {
          swapPositions(position, rightChildPosition)
          position = rightChildPosition
        } else {
          swapPositions(position, leftChildPosition)
          position = leftChildPosition
        }
      } else if (rightChildPosition < currentSize && priority > rightChildPriority) {
        swapPositions(position, rightChildPosition)
        position = rightChildPosition
      } else {
        bubblingDown = false
      }
    }
    position
  }

  override def dropAll(): Unit = {
    currentSize = 0
  }

  override def insert(elem: T): Unit = {
    require(currentSize < maxSize, "The heap is full")
    heapArray(currentSize) = elem
    currentSize += 1
    bubbleUp(currentSize - 1)
  }

  override def getFirst: Option[T] = {
    currentSize match {
      case 0 => None
      case _ => Some(heapArray(0))
    }
  }

  override def getFirsts: List[T] = {
    @tailrec def exploreFirsts(
      value: Long,
      positionsToExplore: List[Int],
      firstItems: List[T]
    ): List[T] = {
      positionsToExplore match {
        case Nil => firstItems
        case position :: tail =>
          if (position < currentSize && priorityFunction(heapArray(position)) == value) {
            val newPositionsToExplore =
              List(rightChild(position), leftChild(position)) ::: tail
            exploreFirsts(value, newPositionsToExplore, heapArray(position) :: firstItems)
          } else {
            exploreFirsts(value, tail, firstItems)
          }
      }
    }
    exploreFirsts(priorityFunction(heapArray(0)), List(0), List.empty)
  }

  override def popFirst(): Option[T] = {
    currentSize match {
      case 0 => None
      case 1 =>
        currentSize = 0
        Some(heapArray(0))
      case _ =>
        swapPositions(0, currentSize - 1)
        currentSize -= 1
        bubbleDown()
        Some(heapArray(currentSize))
    }
  }

  override def popFirsts(): List[T] = {
    @tailrec def popFirsts(priorityToMatch: Long, firstItems: List[T]): List[T] = {
      if (currentSize >= 1 && priorityFunction(heapArray(0)) == priorityToMatch) {
        popFirsts(priorityToMatch, popFirst().get :: firstItems)
      } else {
        firstItems
      }
    }

    if (isEmpty) List.empty
    else popFirsts(priorityFunction(heapArray(0)), List.empty)
  }

  override def iterator: Iterator[T] = new BinaryHeapIterator(heapArray, currentSize)

}

/** An Iterator for the BinaryHeap
  * @param heapArray
  *   The heap as an Array
  * @param size
  *   The size of the heap
  * @tparam T
  *   The type of items in the heap
  */
class BinaryHeapIterator[T](heapArray: Array[T], size: Int) extends Iterator[T] {
  private var current: Int = 0

  def hasNext: Boolean = current < size

  def next(): T = {
    current = current + 1
    heapArray(current - 1)
  }
}
