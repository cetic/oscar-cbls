package oscar.cbls.algo.heap

object BinaryHeapWithMoveIntItem {
  def apply(
    priorityFunction: Int => Long,
    maxSize: Int,
    maxItemValue: Int
  ): BinaryHeapWithMoveIntItem = {
    new BinaryHeapWithMoveIntItem(priorityFunction, maxSize, maxItemValue)
  }
}

/** This binary heap is less efficient than the [[oscar.cbls.algo.heap.BinaryHeap]] but it offers
  * more operations, such as delete and update value. This implementation has been optimize to deal
  * with Integers items.
  *
  * Very similar to [[oscar.cbls.algo.heap.BinaryHeapWithMove]], but instead of using a map for
  * itemsPosition, it uses an array hence the access is in 0(1) but the drop all in O(size).
  *
  * It should be only used in a propagation context.
  * @author
  *   renaud.delandtsheer@cetic.be
  * @param priorityFunction
  *   a function that returns an integer for each element inserted in the heap this value is used to
  *   sort the heap content
  * @param maxSize
  *   the maximum number of elements that can be inserted in this heap
  * @param maxItemValue
  *   The maximal value of inserted element. (for the itemsPositions array)
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
    require(contains(elem), s"Item $elem doesn't seem to be in the heap")
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
    *   - false Item wasn't in the heap
    *   - true operation succeeded
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
    } else false
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
