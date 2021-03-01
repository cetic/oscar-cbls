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

package oscar.algo.reversible

/**
 * @author pschaus
 */
class ReversibleInterval(s: ReversibleContext, val minValue: Int, val maxValue: Int) {
  
  private val _maxValue = if (maxValue - minValue - 1 < Int.MaxValue) maxValue 
  else sys.error("the domain contains more than Int.MaxValue values")
  
  private val _min = new ReversibleInt(s, minValue)
  private val _max = new ReversibleInt(s, _maxValue)
  
  def size: Int = _max - _min + 1
  
  @inline def min: Int = {
	assert(!isEmpty)
    _min.value
  }
  
  @inline def max: Int = {
    assert(!isEmpty)
    _max.value
  }
  

  @inline def isEmpty = {
    _max.value < _min.value
  }
  
  /**
   * @param value
   * @return smallest value in the domain >= value, value-1 is returned if no such value
   */ 
  def nextValue(value: Int): Int = {
    if (isEmpty || value > _max) value -1
    else if (value < _min) _min
    else value
  }
  
  /**
   * @param value
   * @return largest value in the domain <= value, value+1 is returned if no such value
   */ 
  def prevValue(value: Int): Int = {
    if (isEmpty || value < _min) value + 1
    else if (value > _max) _max
    else value
  }
  
  def removeValue(value: Int): Unit = {
    if (value == min) updateMin(value+1)
    if (!isEmpty && value == max) updateMax(value-1)
  }
  
  def hasValue(value: Int) = !isEmpty && value <= max && value >= min
  
  def iterator: Iterator[Int] = (min to max).iterator
  
  def updateMax(value: Int): Unit = {
    if (value < _min) _min.value = _max.value + 1
    else if (value < _max) {
      _max.value = value
    }
  }
  
  def updateMin(value: Int): Unit = {
    if (value > _max) _max.value = _min.value - 1
    else if (value > _min) {
      _min.value = value
    }
  }
  
  def assign(value: Int): Unit = {
    if (hasValue(value)) {
      _min.value = value
      _max.value = value
    }
  }
}
