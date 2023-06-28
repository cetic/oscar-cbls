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
  *
  * The idea of the DoublyLinkedList is the following:
  *
  *   - When we insert a value, we get a container of the element
  *   - Using this container, we can remove the element in O(1)
  * @author
  *   renaud.delandtsheer@cetic.be
  * @tparam T
  *   the type of the elements in the collection
  */
class DoublyLinkedList[T] extends Iterable[T] {

  // Private ?
  private val phantom: DLLStorageElement[T] = new DLLStorageElement[T](null.asInstanceOf[T])

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

  /** Insert <code>elem</code> at the start of the DLL and returns the container that contains the
    * element.
    * @param elem
    *   The element to add
    * @return
    *   The container of the element
    */
  def insertStart(elem: T): DLLStorageElement[T] = {
    val d = new DLLStorageElement[T](elem)
    d.setNext(phantom.next)
    phantom.setNext(d)
    d
  }

  /** Inserts <code>elem</code> after the position specified by <code>afterPosition</code>. If
    * <code>afterPosition</code> is the phantom position, it is inserted as the first element (since
    * start and end phantom are the same
    * @param elem
    *   The element to insert
    * @param afterPosition
    *   The container after which to insert
    * @return
    *   The container of the inserted element
    */
  def insertAfter(elem: T, afterPosition: DLLStorageElement[T]): DLLStorageElement[T] = {
    val successor = afterPosition.next
    val d         = new DLLStorageElement[T](elem)
    d.setNext(successor)
    afterPosition.setNext(d)
    d
  }

  /** Insert <code>elem</code> at the end of the DLL and returns the container that contains the
    * element.
    * @param elem
    *   The element to enqueue
    * @return
    *   The container of the inserted element
    */
  def insertEnd(elem: T): DLLStorageElement[T] = {
    val d = new DLLStorageElement[T](elem)
    phantom.prev.setNext(d)
    d.setNext(phantom)
    d
  }

  /** Removes the first element of the DLL and returns the corresponding value. Raise an exception
    * if the list is empty.
    * @throws java.lang.IllegalArgumentException
    *   when the list is empty
    * @return
    *   The value contained at the begining of the DLL
    */
  def popStart(): T = {
    val d = phantom.next
    require(d != phantom,"Cannot popStart an element on an empty DLL")
    d.delete()
    d.elem
  }

  /** Removes the last element of the DLL and returns the corresponding value. Raise an exception if
    * the list is empty.
    * @throws java.lang.IllegalArgumentException
    *   when the list is empty
    * @return
    *   The value contained at the begining of the DLL
    */
  def popEnd(): T = {
    val d = phantom.prev
    require(d != phantom,"You cannot pop an element in an empty DLLp")
    d.delete()
    d.elem
  }

  /** Syntaxic sugar for [[DoublyLinkedList.insertStart]]
    */
  def +(elem: T): Unit = { insertStart(elem) }

  /** Concatenate an iterable of elements to the DLL
    *
    * @param elems
    *   the elements to concatenate
    */
  def ++(elems: Iterable[T]): Unit = { for (elem <- elems) insertStart(elem) }

  /** Drop all the elements of the list */
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

/** The container structure of the list.
  *
  * @author
  *   renaud.delandtsheer@cetic.be
  * @param elem
  *   the element to store
  * @tparam T
  *   the type of the element
  */
class DLLStorageElement[T](val elem: T) {

  /** The next container of the list */
  var next: DLLStorageElement[T] = _

  /** The previous container of the list */
  var prev: DLLStorageElement[T] = _

  /** Changes the next storage container of this container
    *
    * @param d
    *   The new next storage container
    */
  def setNext(d: DLLStorageElement[T]): Unit = {
    this.next = d
    d.prev = this
  }

  /** Removes this storage container from the list */
  def delete(): Unit = {
    prev.setNext(next)
  }
}

/** An iterator based for [[DoublyLinkedList]]
  *
  * @author
  *   renaud.delandtsheer@cetic.be
  * @param currentKey
  *   the current key in the iterator
  * @param phantom
  *   the phantom element that indicates the beginning/end of the list
  * @tparam T
  *   the type of elements in the iterator
  */
class DLLIterator[T](var currentKey: DLLStorageElement[T], val phantom: DLLStorageElement[T])
    extends Iterator[T] {

  /** Get's the next element and moves the iterator pointer forward
    *
    * @return
    *   The element
    */
  def next(): T = {
    currentKey = currentKey.next
    currentKey.elem
  }

  /** @return
    *   true if the iterator has a next element, false otherwise
    */
  def hasNext: Boolean = { currentKey.next != phantom }
}
