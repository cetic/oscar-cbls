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

/** This abstract class defines all the needed method that needs to be implemented in order to
  * define a heap.
  *
  * Basically all the classical methods, insertion, access, remove
  * @tparam T
  *   The type of items present in the heap
  */
abstract class AbstractHeap[T] extends Iterable[T] {

  /** Change the current size to 0, hence no element has to be considered
    */
  def dropAll(): Unit

  /** Add a new element to the heap given his priority. Insertion at the end and bubble up.
    *
    * @param elem
    *   the element to add
    */
  def insert(elem: T): Unit

  /** Return the first element of the heap without removing it
    *
    * @return
    *   - None (heap is empty)
    *   - Some(The first element of the heap)
    */
  def getFirst: Option[T]

  /** Return the firsts elements of the heap without removing them. Meaning all the elements having
    * the highest priority.
    *
    * @return
    *   - An empty List (heap is empty)
    *   - The List of element having the highest priority
    */
  def getFirsts: List[T]

  /** Pop the first element of the heap. Swap it with the last element of the heap and bubble it
    * down
    *
    * @return
    *   - None (heap is empty)
    *   - Some(the first element of the heap)
    */
  def popFirst(): Option[T]

  /** Pop the firsts elements of the heap. Recursively pop one element and store its value. If the
    * new head of the heap's priority isn't the same as the first element popped we are done.
    *
    * @return
    *   - An empty List (heap is empty)
    *   - The firsts elements of the heap
    */
  def popFirsts(): List[T]
}
