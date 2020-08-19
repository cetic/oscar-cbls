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
  *           Gaël Thouvenin
 ******************************************************************************/

package oscar.cbls.algo.set

import scala.collection.Iterator

/**Circular set is a dedicated data structure to represent set of integers in an efficient way
 * although the memory footprint will not be efficient at all
 * stores elements from 0 to size-1L
 * ensure that the maxsize is not so big because it consumes O(maxsize) memory.
  * @author renaud.delandtsheer@cetic.be
  * @author gael.thouvenin@student.umons.ac.be
 */
class CircularIntSet(val maxsize:Int) extends scala.collection.mutable.SortedSet[Int]{

  private val containsvar:Array[Boolean] = new Array[Boolean](maxsize)
  private[set] val next:Array[Int] = new Array[Int](maxsize) //gives the id of the next element, so that they constitute a cycle in the array
  private[set] val prev:Array[Int] = new Array[Int](maxsize) //gives the id of the next element, so that they constitute a cycle in the array

  private var handle:Int = -1
  private var sizevar:Int = 0
  private var inserted:Boolean = false

  def -=(elem: Int):this.type = {
    if(containsvar(elem)) {
      containsvar(elem) = false
      if (handle == elem) {
        if (sizevar == 1) {
          handle = -1
          next(elem) = -1
          prev(elem) = -1
          sizevar = 0
          return this
        } else {
          handle = next(elem)
        }
      }

      next(prev(elem)) = next(elem)
      prev(next(elem)) = prev(elem)
      next(elem) = -1
      prev(elem) = -1
      sizevar -= 1
    }
    this
  }

  def +=(elem: Int):this.type = {
    if (sizevar == 0) {
      containsvar(elem) = true
      next(elem) = elem
      prev(elem) = elem
      sizevar = 1
      handle = elem
      inserted = true
    }else if (!containsvar(elem)) {
      containsvar(elem) = true
      insertAfter(handle, elem)
      sizevar += 1
      inserted = true
    }
  this
  }

  private def insertAfter(elem:Int, newElem:Int): Unit ={
    val elemAfter:Int = next(elem)
    next(elem) = newElem
    next(newElem) = elemAfter
    prev(elemAfter) = newElem
    prev(newElem) = elem
  }

  /**
   *  Orders the collection. Should be called only if an element has been inserted since last call,
   *  i.e. if inserted == true
   */
  private def reorder():Unit =
  {
    val values = for(e <- this) yield e
    val sortedValues = values.toList.sorted
    sortedValues match {
      case _ :: _ :: _ =>
        for (i <- 1 until sortedValues.size - 1) {
          next(sortedValues(i)) = sortedValues(i + 1)
          prev(sortedValues(i)) = sortedValues(i - 1)
        }
        prev(sortedValues.head) = sortedValues.last
        next(sortedValues.head) = sortedValues(1)
        prev(sortedValues.last) = sortedValues(sortedValues.size - 2)
        next(sortedValues.last) = sortedValues.head
        handle = sortedValues.head

      case _ if size > 0L => handle = sortedValues.head
    }
    inserted = false
  }

  override def size: Int = sizevar

  def contains(elem: Int): Boolean = this.containsvar(elem)

  /**
   * Returns a NON-NECESSARILY ordered iterator over the set
   * @return
   */
  def iterator: Iterator[Int] = new CircularIntSetIterator(handle, this)

  override implicit def ordering: Ordering[Int] = Ordering.Int

  /**
   * Creates and returns a sorted version of this set
   * @param from lower bound (inclusive) of the elements to keep
   * @param until upper bound (inclusive) of the elements to keep
   * @return sorted version of this set, truncated such as each element \in [from, until]
   */
  override def rangeImpl(from: Option[Int], until: Option[Int]): scala.collection.mutable.SortedSet[Int] = {
    if(inserted) reorder()
    new CircularIntSet(maxsize) ++ (for(e <- this if e >= from.getOrElse(0) && e <= until.getOrElse(maxsize)) yield e)
  }

  /**
   * Returns a sorted iterator over the set such as each element has a value over start
   * @param start lower bound (inclusive) of the elements to keep
   * @return an iterator over the elements > start
   */
  override def keysIteratorFrom(start: Int): Iterator[Int] =
  {
    if(inserted) reorder()
    new CircularIntSetIterator(handle, this).filter(x => x >= start)
  }
}

class CircularIntSetIterator(handle:Int, on:CircularIntSet) extends Iterator[Int]{
  var current: Int = handle
  var initposition:Boolean = true

  def hasNext: Boolean = on.size > 0 & ( initposition || current != handle )

  def next(): Int = {
    initposition = false
    current = on.next(current)
    on.prev(current)
  }
}
