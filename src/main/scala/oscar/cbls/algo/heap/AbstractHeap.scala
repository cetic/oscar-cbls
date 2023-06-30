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

/** Defines all the methods needed to run a heap.
  *
  * @tparam T
  *   The type of items present in the heap
  */
abstract class AbstractHeap[T] extends Iterable[T] {

  /** Empties the heap */
  def dropAll(): Unit

  /** Inserts a new element in the heap given its priority
    *
    * @param elem
    *   the element to add
    */
  def insert(elem: T): Unit

  /** Gets the first element of the heap without removing it
    *
    * @return
    *   - None (heap is empty)
    *   - Some(The first element of the heap)
    */
  def getFirst: Option[T]

  /** Returns the firsts elements of the heap without removing them.
    *
    * @return
    *   - An empty List (heap is empty)
    *   - The List of element having the highest priority
    */
  def getFirsts: List[T]

  /** Pops the first element of the heap.
    *
    * @return
    *   - None (heap is empty)
    *   - Some(the first element of the heap)
    */
  def popFirst(): Option[T]

  /** Pops the firsts elements of the heap.
    *
    * @return
    *   - An empty List (heap is empty)
    *   - The firsts elements of the heap
    */
  def popFirsts(): List[T]
}
