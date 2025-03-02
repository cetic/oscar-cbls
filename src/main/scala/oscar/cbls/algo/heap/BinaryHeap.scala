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

/** The companion object for [[BinaryHeap]] */
object BinaryHeap {

  /** Creates a BinaryHeap of type A with the specified priorityFunction
    *
    * @param priorityFunction
    *   a function that returns the priority (an [[scala.Int]] value) of an element of type A
    * @param maxSize
    *   maximum size of the heap
    * @param m
    *   manifest of A, to create arrays of A's
    * @tparam A
    *   The type of the [[BinaryHeap]]
    * @return
    *   A [[BinaryHeap]]
    */
  def apply[A](priorityFunction: A => Long, maxSize: Int)(implicit
    m: Manifest[A]
  ): BinaryHeap[A] = {
    new BinaryHeap[A](priorityFunction, maxSize)
  }
}

/** A binary min-heap.
  *
  * It maintains the order of the heap such that the first element is the one with the minimal
  * priority value. All operations are in O(log(n)).
  *
  * @param priorityFunction
  *   a function that returns the priority (an [[scala.Long]] value) of an element of type A
  * @param maxSize
  *   maximum size of the heap
  * @param m
  *   manifest of A, to create arrays of A's
  * @tparam A
  *   type of elements of the heap
  * @author
  *   renaud.delandtsheer@cetic.be, fabian.germeau@cetic.be
  */
class BinaryHeap[A](priorityFunction: A => Long, val maxSize: Int)(implicit val m: Manifest[A])
    extends Heap[A] {

  override def size: Int        = currentSize
  override def isEmpty: Boolean = currentSize == 0L

  // Keeps the value of the heap. Initialized at max size for performance.
  // Only the first currentSize elements are considered to be in the heap.
  protected val heapArray: Array[A] = new Array[A](maxSize)
  protected var currentSize: Int    = 0

  /** Creates a copy of this BinaryHeap with a new priorityFunction.
    *
    * Creates a new BinaryHeap then add all the elements of this heap to the new one.
    *
    * @param priorityFunction
    *   The new priority function
    * @return
    *   A BinaryHeap with the new priority function and all elements of this heap.
    */
  def withPriorityFunction(priorityFunction: A => Long): BinaryHeap[A] = {
    val copy         = new BinaryHeap[A](priorityFunction, maxSize)
    val heapIterator = iterator
    while (heapIterator.hasNext) {
      val x = heapIterator.next()
      copy.insert(x)
    }
    copy
  }

  @inline protected final def leftChild(position: Int): Int  = (position + 1) * 2 - 1
  @inline protected final def rightChild(position: Int): Int = (position + 1) * 2
  @inline protected final def father(position: Int): Int     = (position - 1) / 2

  /** Swaps the position of two elements in the heapArray.
    * @param position1
    *   The first position
    * @param position2
    *   The second position
    */
  protected def swapPositions(position1: Int, position2: Int): Unit = {
    val tmp: A = heapArray(position1)
    heapArray(position1) = heapArray(position2)
    heapArray(position2) = tmp
  }

  /** Restores the state of the heap by moving up as much as possible the element at the defined
    * position.
    *
    * It simply compares the priority of the element with his father's one.
    *   - If father > element &rarr; switch the position of the father and the element
    *   - Else &rarr; we are done
    *
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

  /** Restores the state of the heap by moving down as much as possible the element at the defined
    * position.
    *
    * It simply compares the priority of the element with his left and right child's one.
    *   - If left < (right && element) &rarr; switch left and element
    *   - If right < (left && element) &rarr; switch right and element
    *   - Else &rarr; we are done
    *
    * @param startPosition
    *   position defining the element we want to bubble down
    * @return
    *   new position of the element
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

  /** Empties the heap
    *
    * Changes the currentSize, hence the number of element present, to zero.
    */
  override def dropAll(): Unit = {
    currentSize = 0
  }

  override def insert(elem: A): Unit = {
    require(currentSize < maxSize, "The heap is full")
    heapArray(currentSize) = elem
    currentSize += 1
    bubbleUp(currentSize - 1)
  }

  override def getFirst: Option[A] = {
    currentSize match {
      case 0 => None
      case _ => Some(heapArray(0))
    }
  }

  override def getFirsts: List[A] = {
    @tailrec def exploreFirsts(
      value: Long,
      positionsToExplore: List[Int],
      firstItems: List[A]
    ): List[A] = {
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

  override def popFirst(): Option[A] = {
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

  override def popFirsts(): List[A] = {
    @tailrec def popFirsts(priorityToMatch: Long, firstItems: List[A]): List[A] = {
      if (currentSize >= 1 && priorityFunction(heapArray(0)) == priorityToMatch) {
        popFirsts(priorityToMatch, popFirst().get :: firstItems)
      } else {
        firstItems
      }
    }

    if (isEmpty) List.empty
    else popFirsts(priorityFunction(heapArray(0)), List.empty)
  }

  override def iterator: Iterator[A] = new BinaryHeapIterator(heapArray, currentSize)

}

/** An Iterator for the BinaryHeap
  * @param heapArray
  *   Heap as an Array
  * @param size
  *   Size of the heap
  * @tparam A
  *   Type of items in the heap
  */
class BinaryHeapIterator[A](heapArray: Array[A], size: Int) extends Iterator[A] {
  private var current: Int = 0

  def hasNext: Boolean = current < size

  def next(): A = {
    current = current + 1
    heapArray(current - 1)
  }
}
