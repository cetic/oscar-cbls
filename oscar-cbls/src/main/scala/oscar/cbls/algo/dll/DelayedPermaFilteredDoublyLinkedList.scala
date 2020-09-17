/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  *            Gaël Thouvenin
  ******************************************************************************/
package oscar.cbls.algo.dll

import oscar.cbls.algo.quick.QList

/**this is a mutable data structure that is able to represent sets through doubly-lined lists, with insert
  * and delete in O(1L) through reference
  * and to update in parallel another set that is a filter of the first one through a specified function
  * the filter can be specified anytime and filtering can be cascaded
  *
  * You should not perform any operation on the slave DLL,
  * although this will not be detected and reported as an error
  *
  * Beware that this is a mutable data structure, hence you should not perform any update on it while iterating on it.
  * @author renaud.delandtsheer@cetic.be
  * @author gael.thouvenin@student.umons.ac.be
  * */
class DelayedPermaFilteredDoublyLinkedList[T <: AnyRef] extends Iterable[T]{

  private[this] val phantom:DPFDLLStorageElement[T] = new DPFDLLStorageElement[T](null.asInstanceOf[T])
  phantom.setNext(phantom)

  /** this function is called on insert. It takes
    * -the inserted element,
    * -an insert function that performs the insert,
    * -a query function that can be called to check if the element is still in the list
    */
  class Filtered[F](val filter:(T,()=>Unit, ()=> Boolean) => Unit,
                 val map:T => F,
                 val filtered:DoublyLinkedList[F]){
    def notifyInsert(inserted: DPFDLLStorageElement[T]): Unit = {
      def injector():Unit = {
        inserted.filtered = QList(filtered.addElem(map(inserted.elem)),inserted.filtered)
      }
      def isStillValid:Boolean = {inserted.prev != null}
      filter(inserted.elem, injector _, isStillValid _)
    }
  }

  private[this] var filtered:QList[Filtered[_]] = _

  def headPhantom: DPFDLLStorageElement[T] = phantom

  /**returns the size of the PermaFilteredDLL
    * this is a O(n) method because it is very rarely used.
    * and in this context, we want to keep the memory footprint as small as possible*/
  override def size: Int ={
    var toReturn = 0
    var current = phantom.next
    while(current != phantom){
      toReturn += 1
      current = current.next
    }
    toReturn
  }

  /**adds an a item in the PermaFilteredDLL, and if accepted by the filter, adds it in the slave PermaFilteredDLL.
    * returns a reference that should be used to remove the item from all those structures at once.
    */
  def addElem(elem:T):DPFDLLStorageElement[T] = {
    val d = new DPFDLLStorageElement[T](elem)
    d.setNext(phantom.next)
    phantom.setNext(d)

    var toNotify = filtered
    while(toNotify!=null){
      toNotify.head.notifyInsert(d)
      toNotify = toNotify.tail
    }
    d
  }

  /**adds an element to the data structure, cfr. method addElem*/
  def +(elem:T): Unit = {addElem(elem)}

  /**adds a bunch of items to the data structures*/
  def ++(elems:Iterable[T]): Unit = {for(elem <- elems) addElem(elem)}

  override def isEmpty:Boolean = phantom.next == phantom

  override def iterator = new DPFDLLIterator[T](phantom,phantom)

  def delayedPermaFilter[F](filter:(T,()=>Unit, ()=> Boolean) => Unit,
                         map:T => F = (t:T) => t.asInstanceOf[F]):DoublyLinkedList[F] = {

    val newFiltered = new Filtered(filter,map,new DoublyLinkedList[F])
    filtered = QList(newFiltered,filtered)

    var currentStorageElement:DPFDLLStorageElement[T] = phantom.next
    while(currentStorageElement != phantom){
      newFiltered.notifyInsert(currentStorageElement)
      currentStorageElement = currentStorageElement.next
    }

    newFiltered.filtered
  }

  def permaFilter[F](filter:T => Boolean,
                     map:T => F = (t:T) => t.asInstanceOf[F]):DoublyLinkedList[F] = {
    def calledWhenAddInFirst(added:T,injector:()=>Unit,isStillValid:() => Boolean):Unit = {
      if(filter(added)){injector()}
    }
    //////////
    delayedPermaFilter(calledWhenAddInFirst,map)
  }

  /**
   * @param fn the function to execute on each items included in this list
   * @tparam U the output type of the function
   * @return a list containing the result of executing fn on each element of the DLL. the list is in reverse order.
   */
  def mapToList[U](fn:T => U): List[U] = {
    val it = iterator
    var toReturn:List[U] = List.empty
    while(it.hasNext){
      toReturn = fn(it.next()) :: toReturn
    }
    toReturn
  }

  override def foreach[U](f: T => U): Unit = {
    var currentPos = headPhantom.next
    while(currentPos != headPhantom){
      f(currentPos.elem)
      currentPos = currentPos.next
    }
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 * @param elem Element to store
 * @tparam T Types of elements
 */
class DPFDLLStorageElement[T](val elem:T){
  var next:DPFDLLStorageElement[T] = _
  var prev:DPFDLLStorageElement[T] = _
  var filtered: QList[DLLStorageElement[_]] = _

  def setNext(d:DPFDLLStorageElement[T]): Unit = {
    this.next = d
    d.prev = this
  }

  def delete(): Unit = {
    prev.setNext(next)
    prev = null //this is checked by the delayed perma filter, so DO NOT REMOVE THIS SEEMIGNLY USELESS INSTRUCTION
    while(filtered != null) {
      filtered.head.delete()
      filtered = filtered.tail
    }
  }
}

class DPFDLLIterator[T](var CurrentKey:DPFDLLStorageElement[T],
                        val phantom:DPFDLLStorageElement[T]) extends Iterator[T]{
  def next():T = {
    CurrentKey = CurrentKey.next
    CurrentKey.elem
  }

  def hasNext:Boolean = {CurrentKey.next != phantom}
}
