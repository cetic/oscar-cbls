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

object BinaryHeapWithMoveIntItem {

  /** Creates an BinaryHeapWithMoveIntItem with the specified priorityFunction
    *
    * @param priorityFunction
    *   a function that returns the priority (an [[scala.Int]] value) of an element
    * @param maxSize
    *   maximum size of the heap
    * @param maxItemValue
    *   maximal value of inserted element
    * @return
    *   A [[BinaryHeapWithMoveIntItem]]
    */
  def apply(
    priorityFunction: Int => Long,
    maxSize: Int,
    maxItemValue: Int
  ): BinaryHeapWithMoveIntItem = {
    new BinaryHeapWithMoveIntItem(priorityFunction, maxSize, maxItemValue)
  }
}

/** A mutable binary heap optimized for Int items.
  *
  * By worsening a bit it's efficiency, it offers additional operations very useful for propagation.
  * Using an Array instead of a map for the positions
  * @author
  *   renaud.delandtsheer@cetic.be
  * @param priorityFunction
  *   a function that returns the priority (an [[scala.Long]] value) of an element [[scala.Int]]
  * @param maxSize
  *   maximum size of the heap
  * @param maxItemValue
  *   maximal value of inserted element. (for the itemsPositions array)
  */
class BinaryHeapWithMoveIntItem(priorityFunction: Int => Long, maxSize: Int, val maxItemValue: Int)
    extends BinaryHeap[Int](priorityFunction, maxSize) {

  require(maxItemValue < Int.MaxValue, "Can't create an array of size Int.MaxValue + 1")
  private val itemsPosition: Array[Int] = Array.fill[Int](maxItemValue + 1)(-1)

  override def withPriorityFunction(priorityFunction: Int => Long): BinaryHeapWithMoveIntItem = {
    val copy         = new BinaryHeapWithMoveIntItem(priorityFunction, maxSize, maxItemValue)
    val heapIterator = iterator
    while (heapIterator.hasNext) copy.insert(heapIterator.next())
    copy
  }

  /** Checks if the heap contains the specified value
    *
    * @param value
    *   The value to find
    * @return
    *   true or false
    */
  def contains(value: Int): Boolean = itemsPosition(value) != -1

  /** Notifies that one element of the heap has changed.
    *
    * @param elem
    *   The element whose internal state has changed.
    */
  def notifyChange(elem: Int): Unit = {
    require(contains(elem), s"Item $elem doesn't seem to be in the heap")
    bubbleDown(bubbleUp(itemsPosition(elem)))
  }

  /** Removes a specific element from the heap
    *
    * @param elem
    *   the element to remove
    * @return
    *   Whether or not an element has been removed
    */
  def removeElement(elem: Int): Boolean = {
    if (contains(elem)) {
      val startPosition: Int = itemsPosition(elem)
      // element already at the end of the heap
      if (startPosition == size - 1) {
        currentSize -= 1
        itemsPosition(elem) = -1
      } else {
        // swap the element with the last one and bubble up + down the swapped element
        swapPositions(startPosition, size - 1)
        currentSize -= 1
        itemsPosition(elem) = -1
        bubbleDown(bubbleUp(startPosition))
      }
      true
    } else {
      false
    }
  }

  override def dropAll(): Unit = {
    heapArray.take(size).foreach(item => itemsPosition(item) = -1)
    super.dropAll()
  }

  override def swapPositions(position1: Int, position2: Int): Unit = {
    itemsPosition(heapArray(position1)) = position2
    itemsPosition(heapArray(position2)) = position1
    super.swapPositions(position1, position2)
  }

  override def insert(elem: Int): Unit = {
    require(currentSize < maxSize, s"The heap is full")
    require(itemsPosition(elem) == -1, s"Can't add the same element twice !")
    itemsPosition(elem) = size
    super.insert(elem)
  }

  override def popFirst(): Option[Int] = {
    super.popFirst() match {
      case None => None
      case Some(elem) =>
        itemsPosition(elem) = -1
        Some(elem)
    }
  }

  /** Checks if the state of the heap is correct. */
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
