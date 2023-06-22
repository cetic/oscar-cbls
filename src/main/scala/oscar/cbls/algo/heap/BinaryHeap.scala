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
import scala.collection.Iterator
import scala.collection.immutable.SortedMap

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
  * @param X
  *   the manifest of T, to create arrays of T's
  * @tparam T
  *   the type of elements included in the heap
  * @author
  *   renaud.delandtsheer@cetic.be, fabian.germeau@cetic.be
  */
class BinaryHeap[T](priorityFunction: T => Long, maxSize: Int)(implicit val X: Manifest[T])
    extends AbstractHeap[T] {

  // TODO : decide if we need to keep it or not
  //
  //  def changePriority_=(KeyGetter: T => Long): Unit = {
  //    if (currentSize > 0L) {
  //      val content: List[T] = this.toList
  //      dropAll()
  //      priority = KeyGetter
  //      content foreach insert
  //    } else {
  //      priority = KeyGetter
  //    }
  //  }
  //
  //  def changePriority: T => Long = priority

  override def size: Int        = currentSize
  override def isEmpty: Boolean = currentSize == 0L

  // Keep the value of the heap. Initialized at max size for performance
  protected val heapArray: Array[T] = new Array[T](maxSize)
  // Keep the real size of the heap, aka the number of elements to consider in the array
  protected var currentSize: Int = 0

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
            firstItems
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

  override def popFirsts: List[T] = {
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
  private var current: Int = -1

  def hasNext: Boolean = current < size

  def next(): T = {
    current = current + 1
    heapArray(current)
  }
}

/** This binary heap is less efficient than the [[oscar.cbls.algo.heap.BinaryHeap]] but it offers
  * more operations, such as delete and update value. It should be only used in a propagation
  * context.
  *
  * @param priorityFunction
  *   a function that returns an integer for each element inserted in the heap this value is used to
  *   sort the heap content
  * @param maxSize
  *   the maximum number of elements that can be inserted in this heap
  * @param X
  *   the manifest of T, to create arrays of T's
  * @tparam T
  *   the type of elements included in the heap
  * @author
  *   renaud.delandtsheer@cetic.be
  */
class BinaryHeapWithMove[T](priorityFunction: T => Long, val maxSize: Int)(
  implicit val A: Ordering[T],
  override implicit val X: Manifest[T]
) extends BinaryHeap[T](priorityFunction, maxSize) {
  private var itemsPosition: SortedMap[T, Int] = SortedMap.empty

  def contains(value: T): Boolean = itemsPosition.contains(value)

  /** Notify that one element of the heap has changed.
    *
    * One element of the heap has changed, we need to restore the state of the heap by bubbling up
    * and down the element.
    *
    * @param elem
    *   The element whose internal state has changed.
    */
  def notifyChange(elem: T): Unit = {
    bubbleDown(bubbleUp(itemsPosition(elem)))
  }

  /** Remove the desired element from the heap
    *
    * It's similar to the popFirst method, the only difference is that the removed element wasn't
    * necessarily at the top of the heap so we need to bubble up and down
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
          bubbleDown(bubbleUp(elemPosition))
        }
        true
    }
  }

  /** Get all the elements present in the heap
    * @return
    *   All the elements present in the heap
    */
  def getElements: Iterable[T] = {
    itemsPosition.keys
  }

  override def swapPositions(position1: Int, position2: Int): Unit = {
    itemsPosition += ((heapArray(position1), position2))
    itemsPosition += ((heapArray(position2), position1))
    super.swapPositions(position1, position2)
  }

  override def insert(elem: T): Unit = {
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

  /** Check if the state of the heap is correct.
    */
  def checkInternals(): Unit = {
    for (i <- heapArray.indices if i < size - 1) {
      if (leftChild(i) < size) {
        require(
          priorityFunction(heapArray(i)) <= priorityFunction(heapArray(leftChild(i))),
          "heap error " + this + i
        )
        require(father(leftChild(i)) == i, "heap error " + this)
      }
      if (rightChild(i) < size) {
        require(
          priorityFunction(heapArray(i)) <= priorityFunction(heapArray(rightChild(i))),
          "heap error " + this
        )
        require(father(rightChild(i)) == i, "heap error " + this)
      }
    }

    for (t <- itemsPosition.keys) {
      assert(heapArray(itemsPosition(t)) == t)
    }
  }

  override def toString: String = {
    heapArray.iterator.toList.mkString("{", ",", "}")
  }
}

/** This binary heap is less efficient than the [[oscar.cbls.algo.heap.BinaryHeap]] but it offers
  * more operations, such as delete and update value. This implementation has been optimize to deal
  * with Integers items.
  *
  * It should be only used in a propagation context.
  * @author
  *   renaud.delandtsheer@cetic.be
  * @param priorityFunction
  * @param maxSize
  * @param maxItemValue
  */
class BinaryHeapWithMoveIntItem(priorityFunction: Int => Long, maxSize: Int, val maxItemValue: Int)
    extends BinaryHeap[Int](priorityFunction, maxSize) {

  private val itemsPosition: Array[Int] = Array.fill[Int](maxItemValue + 1)(-1)

  def contains(value: Int): Boolean = itemsPosition(value) != -1

  /** Notify that one element of the heap has changed.
    *
    * One element of the heap has changed, we need to restore the state of the heap by bubbling up
    * and down the element.
    *
    * @param elem
    *   The element whose internal state has changed.
    */
  def notifyChange(elem: Int): Unit = {
    bubbleDown(bubbleUp(itemsPosition(elem)))
  }

  /** Remove the desired element from the heap
    *
    * It's similar to the popFirst method, the only difference is that the removed element wasn't
    * necessarily at the top of the heap so we need to bubble up and down
    *
    * @param elem
    *   the element to remove
    * @return
    *   Whether or not an element has been removed
    */
  def removeElement(elem: Int): Boolean = {
    if (contains(elem)) {
      val startPosition: Int = itemsPosition(elem)
      if (startPosition == size - 1) {
        currentSize -= 1
        itemsPosition(elem) = -1
      } else {
        swapPositions(startPosition, size - 1)
        currentSize -= 1
        itemsPosition(elem) = -1
        bubbleDown(bubbleUp(startPosition))
      }
      true
    } else false
  }

  override def swapPositions(position1: Int, position2: Int): Unit = {
    itemsPosition(heapArray(position1)) = position2
    itemsPosition(heapArray(position2)) = position1
    super.swapPositions(position1, position2)
  }

  override def insert(elem: Int): Unit = {
    itemsPosition(elem) = size
    super.insert(elem)
  }

  /** removes the smallest element and returns its value
    * @return
    */
  override def popFirst(): Option[Int] = {
    super.popFirst() match {
      case None => None
      case Some(elem) =>
        itemsPosition(elem) = -1
        Some(elem)
    }
  }

  /** Check if the state of the heap is correct.
    */
  def checkInternals(): Unit = {
    for (i <- heapArray.indices if i < size - 1L) {
      if (leftChild(i) < size) {
        require(
          priorityFunction(heapArray(i)) <= priorityFunction(heapArray(leftChild(i))),
          "heap error " + this + i
        )
        require(father(leftChild(i)) == i, "heap error " + this)
      }
      if (rightChild(i) < size) {
        require(
          priorityFunction(heapArray(i)) <= priorityFunction(heapArray(rightChild(i))),
          "heap error " + this
        )
        require(father(rightChild(i)) == i, "heap error " + this)
      }
    }
  }
}
