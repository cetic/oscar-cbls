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
package oscar.invariants

/* Own DoubleLinkedList
 * 
 */

import scala.collection.immutable._

class MyDLLElementContainer[C<:A,A](private var elem:C, var previous: MyDLLElementContainer[_<:A,A], var next: MyDLLElementContainer[_<:A,A]) {

  def foreach(f: (A) => Unit): Unit = {
    f(elem)
    if (next != null) next.foreach(f)
  }
  def apply = elem
}

class MyDLL[A] {

  var first: MyDLLElementContainer[_<:A,A] = null
  var  last: MyDLLElementContainer[_<:A,A] = null
  private var _size = 0
  def size = _size
  def add[C <: A](elem: C ) = {
    val ec = new MyDLLElementContainer[C,A](elem, last, null)
    if (last != null)
      last.next = ec
    else{
      first = ec
    }
    last = ec
    _size += 1
    ec
  }
//  def addElement(ec: MyDLLElementContainer[_<:A,A]) = {
//    ec.previous = null
//    ec.next = first
//    if (first != null)
//      first.previous = ec
//    first = ec
//    ec
//  }
  def remove(ec: MyDLLElementContainer[_<:A,A]): Unit = {
    if (ec.previous == null)
      first = ec.next
    else
      ec.previous.next = ec.next

    if (ec.next == null)
      last = ec.previous
    else
      ec.next.previous = ec.previous
    _size -= 1
  }
  def foreach(f: (A) => Unit): Unit = { if (first != null) first.foreach(f) }

}
