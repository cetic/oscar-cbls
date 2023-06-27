package oscar.cbls.algo.heap

import scala.collection.immutable.SortedMap

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
    require(itemsPosition.contains(elem), s"Item $elem is not in the heap")
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

  /** Check if the state of the heap is correct.
   */
  def checkInternals(): Unit = {
    require(heapArray.take(size).distinct.length == size, "Heap error : there are multiple times the same elements, it's not tolerated")
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