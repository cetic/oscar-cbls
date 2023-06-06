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

//import oscar.cbls.algo.quick.QList

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

/** This represents an array of boolean with where setting all values to true or false can (almost
  * every time) be done in constant time.
  *
  * It works the following way: Boolean are stored as Integer values and the array contains an
  * pivot. The value is true if the integer is greater or equal to the pivot and the value is false
  * else. To put all the value at false, the idea is to change the value of the pivot to abs(pivot)
  * + 1. To put all the value at true, the idea is to change the value of the pivot to -abs(pivot) -
  * \1. If the pivot arrives to a threshold (Long.MaxValue - 10) the array is reinitialized (it
  * costs O(length))
  * @author
  *   Jannou Brohée on 3/10/16.
  * @param length
  *   Maximum length of magical array
  * @param initVal
  *   The initial values in the array. The default value is false.
  */
class MagicBoolArray(val length: Int, initVal: Boolean = false) {

  private[this] val threshold: Long = Long.MaxValue - 10L

  // Made public for testing purposes
  // TODO make it private?
  var pivot: Long = 1L

  private[this] val internalArray: Array[Long] = Array.fill[Long](length)(if (initVal) 1L else 0L)

  // TODO private?
  val indices: Range = 0 until length

  /** Set the new value of element at specific index
    * @param id
    *   the index of the element
    * @param value
    *   the new element's value (true/false)
    * @note
    *   in O(1) // trivial
    */
  def update(id: Int, value: Boolean): Unit = {
    assert(id < length && 0 <= id)
    val oldInternalArray = internalArray(id)
    if (value) internalArray(id) = pivot
    else internalArray(id) = pivot - 1L
  }

  /** Return the value of the element at specific index
    * @param id
    *   the index of the element
    * @return
    *   true or false
    * @note
    *   complexity is O(1)
    */
  def apply(id: Int): Boolean = {
    require(0 <= id && id < length, "got id:" + id + "length:" + length)
    internalArray(id) >= pivot
  }

  /** Sets the value of each element to "value"
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

  /** Resets the array when the pivot commes to close to the threshold
    */
  @inline
  private[this] def resetArray(): Unit = {
    var i = internalArray.length
    while (i > 0) {
      i -= 1
      internalArray(i) = 0L
    }
  }

  // def all: Boolean = ???

  /** Creates an iterator over the indexes of elements which value is true. This is a O(this.length)
    * method
    * @return
    *   the new iterator
    */
  def indicesAtTrue: Iterator[Int] = {
    indicesAtTrueAsList.iterator
  }

  /** Creates a List over the indexes of elements which value is true. This is a O(this.length)
    * method
    * @return
    *   the new iterator
    */
  def indicesAtTrueAsList: List[Int] = {
    @tailrec
    def indicesAtTrue_aux(id: Int = length, res: List[Int] = Nil): List[Int] = {
      if (id == 0)
        res
      else {
        if (internalArray(id) >= pivot)
          indicesAtTrue_aux(id - 1, id :: res)
        else
          indicesAtTrue_aux(id - 1, res)
      }
    }
    indicesAtTrue_aux()
  }

  /**
    * Provides a string with all the indices that are true
    *
    * @return the indices that are true
    */override def toString: String = s"[${indicesAtTrue.mkString(",")}]"
}
