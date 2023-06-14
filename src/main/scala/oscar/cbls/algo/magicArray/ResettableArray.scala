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

import scala.reflect.ClassTag

/** An array where putting all values to their default value is O(1).
  *
  * It works the following way: the array uses a [[MagicBoolArray]] to track all the ids for which
  * the value has been updated. If the value has been updated, the value is taken in the internal
  * array, else the default value is taken. To put all values to their default values, we just
  * update all the values of the internal [[MagicBoolArray]].
  *
  * @tparam T
  *   The type of the elements of the array
  * @param length
  *   the size of the array
  * @param defaultValue
  *   the default value for all the elements of the array
  */

class ResettableArray[T: ClassTag](length: Int, defaultValue: Int => T) {

  private val internalArray: Array[T]       = Array.tabulate(length)(defaultValue)
  private val changedValues: MagicBoolArray = MagicBoolArray(length, false)

  /** Set the new value of element at specific index
    * @param id
    *   the index of the element
    * @param value
    *   the new element's value
    * @note
    *   in O(1) // trivial
    */
  def update(id: Int, value: T): Unit = {
    assert(id < length && 0 <= id)
    internalArray(id) = value
    changedValues(id) = true
  }

  /** Return the value of the element at specific index
    * @param id
    *   the index of the element
    * @return
    *   the value
    * @note
    *   complexity is O(1)
    */
  def apply(id: Int): T = {
    if (changedValues(id)) {
      internalArray(id)
    } else {
      defaultValue(id)
    }
  }

  /** resets the array to the default values complexity is O(1) (except one in every MaxLong calls
    * where is it O(n))
    */
  def reset(): Unit = {
    changedValues.all = false
  }
}
