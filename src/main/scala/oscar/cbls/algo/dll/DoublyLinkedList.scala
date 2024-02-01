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

import scala.collection.AbstractIterator

/** A mutable data structure allowing insertion and deletion in O(1) thanks to the following
  * mechanism:
  *   - When inserting an element, a container wrapping the element is returned.
  *   - Addition and removal of an element in the list require a container to be performed, except
  *     at the start or endpoints of the list.
  * @author
  *   renaud.delandtsheer@cetic.be
  * @tparam T
  *   the type of the elements in the collection
  */
class DoublyLinkedList[T] extends Iterable[T] {

  /** The container structure of the list.
    *
    * @author
    *   renaud.delandtsheer@cetic.be
    * @param value
    *   the value to store
    */
  class DLLStorageElement(val value: T) {

    /** The next container of the list */
    private var _next: DLLStorageElement = _

    /** The previous container of the list */
    private var _prev: DLLStorageElement = _

    /** returns the next element of the list */
    protected[DoublyLinkedList] def next: DLLStorageElement = _next

    /** returns the prev element of the list */
    protected[DoublyLinkedList] def prev: DLLStorageElement = _prev

    /** Changes the next storage container of this container
      *
      * @param d
      *   The new next storage container
      */
    protected[DoublyLinkedList] def next_=(d: DLLStorageElement): Unit = {
      this._next = d
      d._prev = this
    }

    /** Removes this storage container from the list */
    def delete(): Unit = {
      _prev.next = _next
    }
  }

  private val phantom: DLLStorageElement = new DLLStorageElement(null.asInstanceOf[T])

  phantom.next = phantom

  /** Returns the size of the list.
    *
    * This is rarely used, and since we want to keep the memory footprint as small as possible,
    * we're letting it have a O(n) complexity.
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

  /** Insert <code>value</code> at the start of the DLL and returns the container that contains the
    * value.
    * @param value
    *   The value to add
    * @return
    *   The container of the element
    */
  def insertStart(value: T): DLLStorageElement = {
    val d = new DLLStorageElement(value)
    d.next = phantom.next
    phantom.next = d
    d
  }

  /** Inserts <code>value</code> after the element specified by <code>elem</code>.
    *
    * @param value
    *   The value to insert
    * @param elem
    *   The container after which to insert
    * @return
    *   The container of the inserted element
    */
  def insertAfter(value: T, elem: DLLStorageElement): DLLStorageElement = {
    val successor = elem.next
    val d         = new DLLStorageElement(value)
    d.next = successor
    elem.next = d
    d
  }

  /** Insert <code>value</code> at the end of the DLL and returns the container that contains the
    * value.
    * @param value
    *   The element to enqueue
    * @return
    *   The container of the inserted element
    */
  def insertEnd(value: T): DLLStorageElement = {
    val d = new DLLStorageElement(value)
    phantom.prev.next = d
    d.next = phantom
    d
  }

  /** Removes the first element of the DLL and returns the corresponding value. Raise an exception
    * if the list is empty.
    * @throws java.lang.IllegalArgumentException
    *   when the list is empty
    * @return
    *   The value contained at the beginning of the DLL
    */
  def popStart(): T = {
    val d = phantom.next
    require(d != phantom, "Cannot popStart an element on an empty DLL")
    d.delete()
    d.value
  }

  /** Removes the last element of the DLL and returns the corresponding value. Raise an exception if
    * the list is empty.
    * @throws java.lang.IllegalArgumentException
    *   when the list is empty
    * @return
    *   The value contained at the beginning of the DLL
    */
  def popEnd(): T = {
    val d = phantom.prev
    require(d != phantom, "You cannot pop an element in an empty DLLp")
    d.delete()
    d.value
  }

  /** Syntactic sugar for [[DoublyLinkedList.insertStart]]
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
    phantom.next = phantom
  }

  /** Returns true if the dll is empty */
  override def isEmpty: Boolean = phantom.next == phantom

  /** Returns an iterator on the dll */
  override def iterator: Iterator[T] = new AbstractIterator[T] {

    private var currentKey = phantom

    /** Gets the next element and moves the iterator pointer forward
      *
      * @return
      *   The element
      */
    override def next(): T = {
      currentKey = currentKey.next
      currentKey.value
    }

    /** Returns true if the iterator has a next element, false otherwise */
    override def hasNext: Boolean = { currentKey.next != phantom }

  }

  /** Applies <code>f</code> to all the elements of the dll */
  override def foreach[U](f: T => U): Unit = {
    var currentPos = phantom.next
    while (currentPos != phantom) {
      f(currentPos.value)
      currentPos = currentPos.next
    }
  }
}
