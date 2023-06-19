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

package oscar.cbls.algo.dll

/** A mutable data structure allowing insert, and delete in O(1) based on a key mechanism
  * @author
  *   renaud.delandtsheer@cetic.be
  * @tparam T
  *   the type of the elements in the collection
  */
class DoublyLinkedList[T] extends Iterable[T] {

  // Private ?
  val phantom: DLLStorageElement[T] = new DLLStorageElement[T](null.asInstanceOf[T])

  phantom.setNext(phantom)

  /** returns the size of the PermaFilteredDLL this is a O(n) method because it is very rarely used.
    * and in this context, we want to keep the memory footprint as small as possible
    * @return
    *   The size of the DLL
    * @note
    *   O(n) complexity
    */
  override def size: Int = {
    var toReturn = 0
    var current  = phantom.next
    while (current != phantom) {
      toReturn += 1
      current = current.next
    }
    toReturn
  }

  /** adds the element at the start of the DLL
    * @param elem
    *   The element to add
    * @return
    *   TODO
    */
  def addElem(elem: T): DLLStorageElement[T] = {
    val d = new DLLStorageElement[T](elem)
    d.setNext(phantom.next)
    phantom.setNext(d)
    d
  }

  /** inserts <code>toInsert</code> after the position specified by <code>afterPosition</code>. if
    * <code>afterPosition</code> is the phantom position, it is inserted as the first element (since
    * start and end phantom are the same)
    */
  def insertAfter(toInsert: T, afterPosition: DLLStorageElement[T]): DLLStorageElement[T] = {
    val successor = afterPosition.next
    val d         = new DLLStorageElement[T](toInsert)
    d.setNext(successor)
    afterPosition.setNext(d)
    d
  }

  /** adds the element at the end of the DLL */
  def enqueue(elem: T): DLLStorageElement[T] = {
    val d = new DLLStorageElement[T](elem)
    phantom.prev.setNext(d)
    d.setNext(phantom)
    d
  }

  /** removes the first element in the list and returns it; throws exception if empty */
  def dequeue(): T = {
    val d = phantom.next
    assert(d != phantom)
    d.delete()
    d.elem
  }

  def +(elem: T): Unit             = { addElem(elem) }
  def ++(elems: Iterable[T]): Unit = { for (elem <- elems) addElem(elem) }

  def dropAll(): Unit = {
    phantom.setNext(phantom)
  }

  override def isEmpty: Boolean = phantom.next == phantom

  override def iterator = new DLLIterator[T](phantom, phantom)

  override def foreach[U](f: (T) => U): Unit = {
    var currentPos = phantom.next
    while (currentPos != phantom) {
      f(currentPos.elem)
      currentPos = currentPos.next
    }
  }
}

/** @author
  *   renaud.delandtsheer@cetic.be
  * @param elem
  *   the element to store
  * @tparam T
  *   the type of the element
  */
class DLLStorageElement[T](val elem: T) {
  var next: DLLStorageElement[T] = _
  var prev: DLLStorageElement[T] = _

  def setNext(d: DLLStorageElement[T]): Unit = {
    this.next = d
    d.prev = this
  }

  def delete(): Unit = {
    prev.setNext(next)
  }
}

/** @author
  *   renaud.delandtsheer@cetic.be
  * @param CurrentKey
  *   the current key in the iteratot
  * @param phantom
  *   the phantom element that indicates the beginning/end of the list
  * @tparam T
  *   the type of elements in the iterator
  */
class DLLIterator[T](var CurrentKey: DLLStorageElement[T], val phantom: DLLStorageElement[T])
    extends Iterator[T] {
  def next(): T = {
    CurrentKey = CurrentKey.next
    CurrentKey.elem
  }

  def hasNext: Boolean = { CurrentKey.next != phantom }
}
