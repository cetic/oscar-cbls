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

/** An array where resetting all values to their default takes O(1) time.
  *
  * It works in the following way: the array uses a [[MagicBoolArray]] to track all the indices for
  * which the value has been updated, as well as an internal array to store the actual values. When
  * querying the array for an element, the value is returned from the internal array if the value
  * has been updated, otherwise the default value is returned. To reset all values to their default,
  * the internal [[MagicBoolArray]] update method is invoked.
  *
  * @tparam T
  *   The type of the elements of the array
  * @param length
  *   the size of the array
  * @param defaultValue
  *   callback specifying the default value for each element of the array
  */

class ResettableArray[T: ClassTag](length: Int, defaultValue: Int => T) {

  private val internalArray: Array[T]       = Array.tabulate(length)(defaultValue)
  private val changedValues: MagicBoolArray = MagicBoolArray(length)

  /** Sets the new value of the element at the specified index.
    * @param id
    *   the index of the element
    * @param value
    *   the new element's value
    */
  def update(id: Int, value: T): Unit = {
    assert(id < length && 0 <= id)
    internalArray(id) = value
    changedValues(id) = true
  }

  /** Returns the value of the element at specified index.
    * @param id
    *   the index of the element
    * @return
    *   the value
    */
  def apply(id: Int): T = {
    if (changedValues(id)) {
      internalArray(id)
    } else {
      defaultValue(id)
    }
  }

  /** Resets the array to the default values.
    * @note
    *   complexity is O(1), except once every MaxLong calls, where is it O(n)
    */
  def reset(): Unit = {
    changedValues.all = false
  }
}
