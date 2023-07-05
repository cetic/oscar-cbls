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
package oscar.cbls.algo.magicArray

import scala.annotation.tailrec

object MagicBoolArray {

  /** Create a Magical Array Of Boolean of given length, with value initialized to initialVal
    * @param n
    *   the length
    * @param initVal
    *   The initial value int the array
    * @return
    *   a Magical Array Of Boolean or null if length is less than zero
    */
  def apply(n: Int, initVal: Boolean = false): MagicBoolArray = {
    require(n >= 0L, "cannot create magic array of negative size")
    new MagicBoolArray(n, initVal)
  }
}

/** This represents an array of booleans where setting all values to true or false can (almost
  * always) be done in constant time.
  *
  * It works the following way: each boolean is stored as an integer while the array maintains a
  * pivot. The value of an element of the array is true if the respective integer is greater or
  * equal to the pivot and, and false otherwise. To set all the value to false, the pivot is set to
  * abs(pivot) + 1, while to set all the value to true, it is set to
  * -abs(pivot) - \1. If the pivot reaches a threshold (Long.MaxValue - 10) the array is
  * reinitialized, which costs O(length).
  * @author
  *   Jannou Brohée on 3/10/16
  * @param length
  *   The length of the array
  * @param initVal
  *   The initial values in the array. The default value is false
  */
class MagicBoolArray(val length: Int, initVal: Boolean = false) {

  private[this] val threshold: Long = Long.MaxValue - 10L

  private var pivot: Long = 1L

  private[this] val internalArray: Array[Long] = Array.fill[Long](length)(if (initVal) 1L else 0L)

  protected val indices: Range = 0 until length

  /** Sets the new value of the element at specified index.
    * @param id
    *   the index of the element
    * @param value
    *   the new value of the element
    */
  def update(id: Int, value: Boolean): Unit = {
    assert(id < length && 0 <= id)
    if (value) internalArray(id) = pivot
    else internalArray(id) = pivot - 1L
  }

  /** Returns the value of the element at specified index.
    * @param id
    *   the index of the element
    */
  def apply(id: Int): Boolean = {
    require(0 <= id && id < length, "got id:" + id + " length:" + length)
    internalArray(id) >= pivot
  }

  /** Sets each element to the given value.
    * @param value
    *   The value to set
    * @note
    *   complexity is O(1)
    */
  def all_=(value: Boolean): Unit = {
    if (value) {
      if (Math.abs(pivot) == threshold) {
        pivot = 0L
        resetArray()
      } else {
        pivot = -Math.abs(pivot) - 1L
      }
    } else {
      if (Math.abs(pivot) == threshold) {
        pivot = 1L
        resetArray()
      } else {
        pivot = Math.abs(pivot) + 1L
      }
    }
  }

  /** Resets the array when the pivot is about to reach the threshold. */
  @inline
  private[this] def resetArray(): Unit = {
    var i = internalArray.length
    while (i > 0) {
      i -= 1
      internalArray(i) = 0L
    }
  }

  /** Returns the values stored in this array in the form of a Scala <code>Array</code>.
    *
    * @note
    *   Complexity: O(length)
    */
  def all: Array[Boolean] = Array.tabulate(length)(i => internalArray(i) >= pivot)

  /** Creates an iterator over the indices of elements whose value is true.
    *
    * @note
    *   complexity is O(length)
    */
  def indicesAtTrue: Iterator[Int] = {
    indicesAtTrueAsList.iterator
  }

  /** Creates a List containing the indices of elements whose value is true.
    *
    * @note
    *   complexity is O(length)
    */
  def indicesAtTrueAsList: List[Int] = {
    @tailrec
    def indicesAtTrue_aux(id: Int = 0, res: List[Int] = Nil): List[Int] = {
      if (id == length)
        res
      else {
        if (internalArray(id) >= pivot)
          indicesAtTrue_aux(id + 1, id :: res)
        else
          indicesAtTrue_aux(id + 1, res)
      }
    }
    indicesAtTrue_aux()
  }

  /** Provides a string with all the indices that are true. */
  override def toString: String = s"[${indicesAtTrue.mkString(",")}]"
}
