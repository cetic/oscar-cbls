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

import scala.collection.Iterable

/**
 * Initializes a set with all values min..max in it
 * @param s
 * @param minValue
 * @param maxValue >= minValue
 * @author Pierre Schaus
 */
class ReversibleSparseSet(s: ReversibleContext, val minValue: Int, val maxValue: Int) extends Iterable[Int] {

  private[this] val offset = minValue
  private[this] val _min = new ReversibleInt(s, minValue)
  private[this] val _max = new ReversibleInt(s, maxValue)
  private[this] val _size = new ReversibleInt(s, maxValue - minValue + 1)

  private[this] val values = Array.tabulate(size)(i => i)
  private[this] val indexes = Array.tabulate(size)(i => i)

  override def size: Int = _size.value

  def min: Int = {
    if (!hasValue(_min.value)) updateMinValRemoved(_min.value)
    _min.value
  }
  @inline private def min_=(v: Int): Unit = {
    _min.value = v
  }

  def max: Int = {
    if (!hasValue(_max.value)) updateMaxValRemoved(_max.value)
    _max.value
  }

  @inline private def max_=(v: Int): Unit = {
    _max.value = v
  }

  @inline private def checkVal(v: Int) = {
    assert(v >= offset)
    assert(v <= offset + values.size - 1)
    true
  }

  def hasValue(v: Int) = {
    if (v < offset || v >= offset + indexes.size) false
    else indexes(v - offset) < _size.value;
  }

  override def isEmpty = _size.value == 0

  /**
   * remove all elements in the set
   */
  def makeEmpty(): Unit = _size.value = 0

  @inline private def exchangePositions(val1: Int, val2: Int): Unit = {
    assert(checkVal(val1))
    assert(checkVal(val2))
    val v1 = val1 - offset
    val v2 = val2 - offset
    val i1 = indexes(v1)
    val i2 = indexes(v2)
    values(i1) = v2
    values(i2) = v1
    indexes(v1) = i2
    indexes(v2) = i1
  }

  @inline private def updateBoundsValRemoved(v: Int): Unit = {
    updateMaxValRemoved(v)
    updateMinValRemoved(v)
  }

  @inline private def updateMaxValRemoved(v: Int): Unit = {

    /*
    if (!isEmpty) {
      assert(!hasValue(v));
      var cv = v
      while (cv >= _min.value) {
        if (hasValue(cv)) {
          max = cv
          return
        }
        cv -= 1
      }
    }*/

    if (!isEmpty) {
      // max = iterator.max
      var i = 0
      var M = values(i)
      val s = _size.value
      while (i < s) {
        if (values(i) > M) {
          M = values(i)
        }
        i += 1
      }
      max = M + offset
    }
  }

  @inline private def updateMinValRemoved(v: Int): Unit = {

    /*
    if (!isEmpty) {
      var cv = v
      while (cv <= _max.value) {
        if (hasValue(cv)) {
          min = cv
          return
        }
        cv += 1
      }
    }*/

    if (!isEmpty) {
      // min = iterator.min
      var i = 0
      var m = values(i)
      val s = _size.value
      while (i < s) {
        if (values(i) < m) {
          m = values(i)
        }
        i += 1
      }
      min = m + offset
    }
  }

  def removeValue(v: Int): Boolean = {
    if (!hasValue(v)) return false; //the value has already been removed
    exchangePositions(v, values(size - 1) + offset)
    _size.decr()
    true
  }

  /**
   * @param v
   * @return smallest value in the domain >= value, value-1 is returned if no such value
   */
  def nextValue(v: Int): Int = {
    assert(checkVal(v))
    assert(!isEmpty)
    var cv = v
    while (cv <= max) {
      if (hasValue(cv)) {
        return cv
      }
      cv += 1
    }
    v - 1
  }

  /**
   * @param v
   * @return largest value in the domain <= value, value+1 is returned if no such value
   */
  def prevValue(v: Int): Int = {
    assert(checkVal(v))
    assert(!isEmpty)
    var cv = v
    while (cv >= min) {
      if (hasValue(cv)) {
        return cv
      }
      cv -= 1
    }
    v + 1
  }

  def removeAllBut(v: Int): Unit = {
    // we only have to put in first position this value and set the size to 1
    assert(checkVal(v));
    assert(hasValue(v));
    val value = values(0)
    val index = indexes(v - offset)
    indexes(v - offset) = 0
    values(0) = v - offset
    indexes(value) = index
    values(index) = value
    min = v
    max = v
    _size.value = 1
  }

  def updateMin(minv: Int): Unit = {
    assert(checkVal(minv))
    assert(!isEmpty)
    if (minv < min) {
      return // the min does not change
    } else if (minv > max) {
      _size.value = 0 // the set becomes empty since the new min is larger than the current max
    } else if (minv == max) {
      // the new min is equal to the current max hence only one value in the set
      removeAllBut(minv)
    } else {
      var cv = min
      while (cv < minv) {
        removeValue(cv)
        cv += 1
      }
      if (hasValue(minv)) {
        min = minv
      }
    }
  }

  def updateMax(maxv: Int): Unit = {
    assert(checkVal(max));
    assert(!isEmpty);
    if (maxv >= max) {
      return
    } else if (maxv < min) {
      _size.value = 0 // the set becomes empty since the new max is smaller than the current min
    } else if (maxv == min) {
      // the new max is equal to the current min hence only one value in the set
      removeAllBut(maxv)
    } else {
      var cv = max
      while (cv > maxv) {
        removeValue(cv)
        cv -= 1
      }
      if (hasValue(maxv)) {
        max = maxv
      }
    }
  }

  /**
   * @return a safe iterator (ok to remove values while iterating)
   */
  def iterator: Iterator[Int] = {
    this.toArray.iterator
  }
  
  /**
   * @return an array representation of values present in the set
   */
  def toArray: Array[Int] = {
    val res = Array.ofDim[Int](size)
    fillArray(res)
    res
  }
  
  /**
   * @return set the first values of dest to the ones 
   *         of the set and return the size of the set
   */
  def fillArray(dest: Array[Int]): Int = {
    System.arraycopy(values, 0, dest, 0, size)
    if (offset != 0) {
      var i = 0
      while (i < size) {
        dest(i) += offset
        i += 1
      }
    }
    size
  }

  /**
   * @return an array representation of values present that have been removed from the set
   */
  def removedToArray: Array[Int] = {
    val res = Array.ofDim[Int](values.length-size)
    removedFillArray(res)
    res
  }

  /**
   * @return set the first values of dest to the ones removed
   *         from the set and return the size of the set
   */
  def removedFillArray(dest: Array[Int]): Int = {
    System.arraycopy(values, size, dest, 0, values.length-size)
    if (offset != 0) {
      var i = 0
      while (i < values.length-size) {
        dest(i) += offset
        i += 1
      }
    }
    values.length-size
  }

  /**
   * get the i_th value in the sparse-set
   */
  @inline final def apply(i: Int) = {
    values(i) + offset
  } 

  def delta(oldSize: Int): Iterator[Int] = {
    var ind = size
    new Iterator[Int] {
      def next(): Int = {
        val v = values(ind)
        ind += 1
        v + offset
      }
      def hasNext: Boolean = {
        ind < oldSize && ind < values.size
      }
    }
  }

  /**
   * You can safely remove the current value while iterating using this function.
   * If you remove any other value, you may encounter the same value multiple times.
   */
  @inline final override def foreach[U](f: Int => U): Unit = {
    var i = _size.value
    while (i != 0) {
      i -= 1
      f(values(i) + offset)
    }
  }
}
