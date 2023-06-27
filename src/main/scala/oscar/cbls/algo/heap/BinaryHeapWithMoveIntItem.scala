package oscar.cbls.algo.heap

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
    require(itemsPosition(elem) != -1, s"Item $elem doesn't seem to be in the heap")
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
    require(itemsPosition(elem) == -1, s"Can't add the same element twice !")
    require(currentSize < maxSize, s"The heap is full")
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