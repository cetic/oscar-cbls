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
  *******************************************************************************/

package oscar.cp.core.variables

import java.util.ConcurrentModificationException

import oscar.algo.Inconsistency

import scala.util.Random
import oscar.algo.reversible.ReversibleBoolean
import oscar.algo.reversible.ReversibleInt
import oscar.algo.reversible.ReversiblePointer
import oscar.algo.reversible.TrailEntry
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.watcher.WatcherListL2
import oscar.cp.core.watcher.WatcherListL1
import oscar.cp.core.watcher.Watcher
import oscar.cp.core.delta.DeltaIntVar

import scala.collection.mutable

/**
 *  @author Renaud Hartert ren.hartert@gmail.com
 *  @author Pierre Schaus pschaus@gmail.com
 */

class CPIntVarAdaptableDomainContinuous(variable: CPIntVarAdaptable, min: Int, max: Int, size: Int) extends TrailEntry {
  final override def restore(): Unit = variable.restoreContinuous(min, max, size)
}

class CPIntVarAdaptableDomainSparse(variable: CPIntVarAdaptable, min: Int, max: Int, size: Int) extends TrailEntry {
  final override def restore(): Unit = variable.restoreSparse(min, max, size)
}

final class CPIntVarAdaptable( final override val store: CPStore, minValue: Int, maxValue: Int, origContinuous: Boolean, final override val name: String = "") extends CPIntVar {

  final override val context = store

  // Registered constraints
  private[this] val onBindL2 = new WatcherListL2(store)
  private[this] val onBoundsL2 = new WatcherListL2(store)
  private[this] val onDomainL2 = new WatcherListL2(store)

  private[this] val onBindL1 = new WatcherListL1(store)
  private[this] val onBoundsL1 = new WatcherListL1(store)
  private[this] val onDomainL1 = new WatcherListL1(store)

  // Number of constraints registered on the variable
  private[this] val degree = new ReversibleInt(store, 0) // should not change often

  // Domain representation
  private[variables] var values: Array[Int] = null
  private[variables] var positions: Array[Int] = null
  private[variables] var offset = minValue
  private[variables] var _continuous = origContinuous // can be true with a sparse representation
  private[variables] var _min = minValue
  private[variables] var _max = maxValue
  private[variables] var _size = maxValue - minValue + 1

  // Switch to a sparse set if necessacry
  if (!origContinuous) buildSparse()

  // Used to trail changes in the domain
  private[this] var lastMagic: Long = -1L

  @inline private def trail(): Unit = {
    val contextMagic = store.magic
    if (lastMagic != contextMagic) {
      lastMagic = contextMagic
      if (_continuous) store.trail(new CPIntVarAdaptableDomainContinuous(this, _min, _max, _size))
      else store.trail(new CPIntVarAdaptableDomainSparse(this, _min, _max, _size))
    }
  }

  // Restore the domain to continuous domain
  @inline final def restoreContinuous(oldMin: Int, oldMax: Int, oldSize: Int): Unit = {
    _min = oldMin; _max = oldMax; _size = oldSize
    if (!_continuous) buildSparse() // recreate a sparse domain on backtrack
  }

  // Restore the domain to a sparse domain
  @inline final def restoreSparse(oldMin: Int, oldMax: Int, oldSize: Int): Unit = {
    _min = oldMin; _max = oldMax; _size = oldSize
  }

  @inline final override def size: Int = _size

  @inline final override def min: Int = _min

  @inline final override def max: Int = _max

  @inline final override def isContinuous: Boolean = _continuous

  @inline final def setContinuous(): Unit = _continuous = true

  @inline final override def isBound: Boolean = _size == 1

  final override def isBoundTo(value: Int): Boolean = _size == 1 && _min == value

  @inline final override def hasValue(value: Int): Boolean = {
    if (value < _min || value > _max) false
    else if (_continuous) true
    else positions(value - offset) < _size
  }

  final override def randomValue(rand: Random): Int = {
    val r = rand.nextInt(_size)
    if (_continuous) _min + r
    else values(r)
  }

  final override def transform(v: Int): Int = v

  final override def iterator: Iterator[Int] = {
    if (_continuous) iteratorContinuous
    else iteratorSparse
  }

  @inline private def iteratorContinuous: Iterator[Int] = new Iterator[Int] {
    private[this] val max = _max
    private[this] var i = _min - 1
    final override def next(): Int = { i += 1; i }
    final override def hasNext: Boolean = i < max
  }

  @inline private def iteratorSparse: Iterator[Int] = new Iterator[Int] {
    private[this] val array = new Array[Int](_size)
    private[this] var i = _size
    System.arraycopy(values, 0, array, 0, _size)
    final override def next(): Int = { i -= 1; array(i) }
    final override def hasNext: Boolean = i > 0
  }

  /**
   *  @return an array containing all the values in the domain.
   *          The result array is not sorted.
   */
  final override def toArray: Array[Int] = {
    val array = new Array[Int](_size)
    copyDomain(array)
    array
  }

  /**
   *  @param array.length >= this.size
   *  @return Fills the array with the values contained in the domain and
   *          returns the number of values (this.size).
   *          The array is not sorted.
   */
  final override def fillArray(array: Array[Int]): Int = copyDomain(array)

  // Copy the domain in the array and return the size of the domain
  @inline private def copyDomain(array: Array[Int]): Int = {
    if (_continuous) {
      var i = _size
      while (i > 0) { i -= 1; array(i) = i + _min }
    } else System.arraycopy(values, 0, array, 0, _size)
    _size
  }

  final override def constraintDegree: Int = degree.value

  /**
   * Reduce the domain to the singleton {value}, and notify appropriately all the propagators registered to this variable
   * @param value
   * @return  Suspend if val was in the domain, Failure otherwise
   */
  final override def assign(value: Int): Unit = {
    if (value < _min || value > _max)
      throw Inconsistency

    if (_size == 1) {}
    else if (_continuous) assignContinuous(value) // assign continuous
    else if (positions(value - offset) >= _size) throw Inconsistency // gard on sparse
    else assignSparse(value) // assign sparse
  }

  @inline private def assignContinuous(value: Int): Unit = {
    // Trail before changes
    trail()
    // Update the domain
    val oldMin = _min
    val oldMax = _max
    _min = value
    _max = value
    _size = 1
    // Notify AC3
    onBoundsL2.enqueue()
    onBindL2.enqueue()
    // Notify AC5
    onBindL1.enqueueBind()
    onBoundsL1.enqueueBounds()
    // Notify removed values if necessary
    if (!onDomainL1.isEmpty) {
      var i = oldMin
      while (i <= oldMax) {
        if (i != value) onDomainL1.enqueueRemove(i)
        i += 1
      }
    }
    // at the end because of watchers
    onDomainL2.enqueue()
  }

  @inline private def assignSparse(value: Int): Unit = {
    // Notify removed values if necessary
    if (!onDomainL1.isEmpty) {
      var i = _size
      while (i > 0) {
        i -= 1
        val v = values(i)
        if (v != value) {
          onDomainL1.enqueueRemove(v) // FIXME should be notified after the actual update
        }
      }
    }
    // Update domain
    val id = value - offset
    val position = positions(id)
    val v = values(0)
    positions(id) = 0
    values(0) = value
    positions(v - offset) = position
    values(position) = v
    trail() // trail before changes 
    _min = value
    _max = value
    _size = 1    
    // Notify AC3
    onBoundsL2.enqueue()
    onBindL2.enqueue()
    // Notify AC5
    onBindL1.enqueueBind()
    onBoundsL1.enqueueBounds()
    // Notify domain watchers
    onDomainL2.enqueue()
  }

  /**
   * Remove val from the domain, and notify appropriately all the propagators registered to this variable
   * @param value
   * @return  Suspend if the domain is not equal to the singleton {val}, Failure otherwise
   */
  final override def removeValue(value: Int): Unit = {
    if (value < _min || value > _max) {}
    else if (_size == 1) throw Inconsistency
    else if (_continuous) removeContinuous(value)
    else removeSparse(value)
  }

  @inline private def removeContinuous(value: Int): Unit = {
    if (value == _min) updateMinContinuous(value + 1)
    else if (value == _max) updateMaxContinuous(value - 1)
    else { // Switch the domain representation
      buildSparse()
      removeSparse(value)
    }
  }

  @inline private def buildSparse(): Unit = {
    _continuous = false
    val nValues = _max - _min + 1
    offset = _min
    values = Array.tabulate(nValues)(i => i + offset)
    positions = Array.tabulate(nValues)(i => i)
  }

  @inline private def removeSparse(value: Int): Unit = {
    val id1 = value - offset
    val pos1 = positions(id1)
    if (pos1 < _size) {
      // Trail before changes 
      trail()
      // Update the domain
      _size -= 1
      val v = values(_size)
      val id2 = v - offset
      val pos2 = positions(id2)
      values(pos1) = v
      values(pos2) = value
      positions(id1) = pos2
      positions(id2) = pos1
      // Notify watchers
      if (_size == 1) {
        // Notify bind watchers
        onBindL1.enqueueBind()
        onBindL2.enqueue()
        // Notify bound watchers
        onBoundsL1.enqueueBounds()
        onBoundsL2.enqueue()
        // Update min or max
        if (value == _min) _min = _max
        else _max = _min
      } // Min changed
      else if (_min == value) {
        // Notify bound watchers
        onBoundsL1.enqueueBounds()
        onBoundsL2.enqueue()
        // Update min
        var i = _min - offset + 1
        while (positions(i) >= _size) i += 1
        _min = i + offset
      } // Max change
      else if (_max == value) {
        // Notify bound watchers
        onBoundsL1.enqueueBounds()
        onBoundsL2.enqueue()
        // Update max
        var i = _max - offset - 1
        while (positions(i) >= _size) i -= 1
        _max = i + offset
      }
      // Notify domain watchers
      onDomainL2.enqueue()
      onDomainL1.enqueueRemove(value)
    }
  }

  /**
   * Remove from the domain all values < val, and notify appropriately all the propagators registered to this variable
   * @param value
   * @return  Suspend if there is at least one value >= val in the domain, Failure otherwise
   */
  final override def updateMin(value: Int): Unit = {
    if (value <= _min) {}
    else if (value > _max) throw Inconsistency
    else if (_continuous) updateMinContinuous(value)
    else updateMinSparse(value)
  }

  @inline private def updateMinContinuous(value: Int): Unit = {
    if (value == _max) assignContinuous(value)
    else {
      // Trail before changes 
      trail()
      // Update domain
      val oldMin = _min
      _size -= (value - _min)
      _min = value
      // Notify bounds watchers
      onBoundsL1.enqueueBounds()
      onBoundsL2.enqueue()
      // Notify remove watchers if necessary
      if (!onDomainL1.isEmpty) {
        var i = oldMin
        while (i < value) {
          onDomainL1.enqueueRemove(i)
          i += 1
        }
      }
      // Notify domain watchers
      onDomainL2.enqueue()
    }
  }

  @inline private def updateMinSparse(value: Int): Unit = {
    if (value == _max) assignSparse(value)
    else {
      trail() // trail before changes  
      // Remove values
      val valueId = value - offset
      var i = _min - offset
      while (i < valueId) {
        val pos1 = positions(i)
        if (pos1 < _size) {
          // Update the domain
          _size -= 1
          val v1 = i + offset
          val v2 = values(_size)
          val id2 = v2 - offset
          val pos2 = positions(id2)
          values(pos1) = v2
          values(pos2) = v1
          positions(i) = pos2
          positions(id2) = pos1
          onDomainL1.enqueueRemove(v1) // FIXME should be notified after the actual update
        }
        i += 1
      }
      // Search new min
      while (positions(i) >= _size) i += 1
      _min = i + offset

      // Notify bind events
      if (_size == 1) {
        onBindL1.enqueueBind()
        onBindL2.enqueue()
      }

      // Notify bounds events
      onBoundsL1.enqueueBounds()
      onBoundsL2.enqueue()

      // Notify domain events
      onDomainL2.enqueue()
    }
  }

  /**
   * Remove from the domain all values > val, and notify appropriately all the propagators registered to this variable
   * @param value
   * @return  Suspend if there is at least one value <= val in the domain, Failure otherwise
   */
  final override def updateMax(value: Int): Unit = {
    if (value >= _max) {}
    else if (value < _min) throw Inconsistency
    else if (_continuous) updateMaxContinuous(value)
    else updateMaxSparse(value)
  }

  @inline private def updateMaxContinuous(value: Int): Unit = {
    if (value == _min) assignContinuous(value)
    else {
      // Trail before changes 
      trail()
      // Update domain
      val oldMax = _max
      _size -= (_max - value)
      _max = value
      // Notify bounds watchers
      onBoundsL1.enqueueBounds()
      onBoundsL2.enqueue()
      // Notify remove watchers if necessary
      if (!onDomainL1.isEmpty) {
        var i = oldMax
        while (i > value) {
          onDomainL1.enqueueRemove(i)
          i -= 1
        }
      }
      // Notify domain watchers
      onDomainL2.enqueue()
    }
  }

  @inline private def updateMaxSparse(value: Int): Unit = {
    if (value == _min) assignSparse(value)
    else {
      trail() // trail before changes  
      // Remove values
      val valueId = value - offset
      var i = _max - offset
      while (i > valueId) {
        val pos1 = positions(i)
        if (pos1 < _size) {
          // Update the domain
          _size -= 1
          val v1 = i + offset
          val v2 = values(_size)
          val id2 = v2 - offset
          val pos2 = positions(id2)
          values(pos1) = v2
          values(pos2) = v1
          positions(i) = pos2
          positions(id2) = pos1
          onDomainL1.enqueueRemove(v1) // FIXME should be notified after the actual update
        }
        i -= 1
      }
      // Search new min
      while (positions(i) >= _size) i -= 1
      _max = i + offset

      // Notify bind events

      if (_size == 1) {
        onBindL1.enqueueBind()
        onBindL2.enqueue()
      }

      // Notify bounds events
      onBoundsL1.enqueueBounds()
      onBoundsL2.enqueue()

      // Notify domain events
      onDomainL2.enqueue()
    }
  }

  final override def toString(): String = {
    val buffer = new StringBuffer
    if (name.length > 0) buffer.append(s"$name ")
    if (_size == 1) buffer.append(_min)
    else if (_continuous) buffer.append(s"[${_min}, ${_max}]")
    else {
      val array = new Array[Int](_size)
      System.arraycopy(values, 0, array, 0, _size)
      buffer.append("{")
      buffer.append(array.sorted.mkString(", "))
      buffer.append("}")
    }
    buffer.toString
  }

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  final override def callPropagateWhenBind(c: Constraint): Unit = {
    degree.incr()
    onBindL2.register(c)
  }

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * the maximum or the minimum value of the domain changes
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  final override def callPropagateWhenBoundsChange(c: Constraint): Unit = {
    degree.incr()
    onBoundsL2.register(c)
  }

  final override def callPropagateWhenBoundsChange(c: Constraint, cond: => Boolean): Unit = {
    degree.incr()
    onBoundsL2.register(c, cond)
  }
  
  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * one of the value is removed from the domain
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  final override def callPropagateWhenDomainChanges(c: Constraint): Unit = {
    degree.incr()
    onDomainL2.register(c)
  }

  final override def callPropagateOnChangesWithDelta(c: Constraint): DeltaIntVar = {
    val snap = delta(c)
    degree.incr()
    onDomainL2.register(c)
    snap
  }

  final override def callPropagateOnChangesWithDelta(c: Constraint, cond: => Boolean): DeltaIntVar = {
    val snap = delta(c)
    degree.incr()
    onDomainL2.register(c, cond)
    snap
  }

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * one of the value is removed from the domain
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  final override def callPropagateWhenDomainChanges(c: Constraint, cond: => Boolean): Unit = {
    degree.incr()
    onDomainL2.register(c, cond)
  }

  final override def awakeOnChanges(watcher: Watcher): Unit = {
    degree.incr()
    onDomainL2.register(watcher)
  }

  /**
   * Level 1 registration: ask that the updateBounds(CPIntVar) method of the constraint c is called whenever
   * the minimum or maximum value of the domain changes.
   * @param c
   * @see oscar.cp.core.Constraint#updateBounds(CPIntVar)
   */
  final override def callUpdateBoundsWhenBoundsChange(c: Constraint): Unit = {
    callUpdateBoundsWhenBoundsChange(c, this)
  }

  final override def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntVar): Unit = {
    degree.incr()
    onBoundsL1.register(c, variable)
  }

  /**
   * Level 1 registration: ask that the valRemove(CPIntVar, int) method of the constraint c is called for each
   * value deletion from the domain
   * @param c
   * @see oscar.cp.core.Constraint#valRemove(CPIntVar, int)
   */
  final override def callValRemoveWhenValueIsRemoved(c: Constraint): Unit = {
    callValRemoveWhenValueIsRemoved(c, this)
  }

  final override def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPIntVar): Unit = {
    degree.incr()
    onDomainL1.register(c, variable)
  }

  /**
   * Level 1 registration: ask that the valBind(CPIntVar) method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @see oscar.cp.core.Constraint#valBind(CPIntVar)
   */
  final override def callValBindWhenBind(c: Constraint): Unit = {
    callValBindWhenBind(c, this)
  }

  final override def callValBindWhenBind(c: Constraint, variable: CPIntVar): Unit = {
    degree.incr()
    onBindL1.register(c, variable)
  }

  /**
   * Level 1 registration: ask that the valRemoveIdx(CPIntVar, int, int) method of the constraint c is called for each
   * value deletion from the domain
   * @param c
   * @param idx, an index that will be given as parameter to valRemoveIdx(CPIntVar, int, int)
   * @see Constraint#valRemoveIdx(CPIntVar, int, int)
   */
  final override def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int): Unit = {
    callValRemoveIdxWhenValueIsRemoved(c, this, idx)
  }

  final override def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPIntVar, idx: Int): Unit = {
    degree.incr()
    onDomainL1.register(c, variable, idx)
  }

  /**
   * Level 1 registration: ask that the updateBoundsIdx(CPIntVar, int) method of the constraint c is called whenever
   * the minimum or maximum value of the domain changes
   * @param c
   * @param idx, an index that will be given as parameter to updateBoundsIdx(CPIntVar, int)
   * @see Constraint#updateBoundsIdx(CPIntVar, int)
   */
  final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int): Unit = {
    callUpdateBoundsIdxWhenBoundsChange(c, this, idx)
  }

  final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntVar, idx: Int): Unit = {
    degree.incr()
    onBoundsL1.register(c, variable, idx)
  }

  /**
   * Level 1 registration: ask that the valBindIdx(CPIntVar, int) method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @param idx, an index that will be given as parameter to valBindIdx(CPIntVar, int)
   * @see Constraint#valBindIdx(CPIntVar, int)
   */
  final override def callValBindIdxWhenBind(c: Constraint, idx: Int): Unit = {
    callValBindIdxWhenBind(c, this, idx)
  }

  final override def callValBindIdxWhenBind(c: Constraint, variable: CPIntVar, idx: Int): Unit = {
    degree.incr()
    onBindL1.register(c, variable, idx)
  }

  final def restrict(newDomain: Array[Int], newSize: Int): Unit = {
    assert(newSize > 0 && newSize <= size)

    // Restrict the domain
    if (_continuous) buildSparse()
    trail()
    val oldSize = _size
    _size = 0
    var i = newSize
    while (i > 0) {
      i -= 1
      val val1 = newDomain(i)
      val pos1 = positions(val1 - offset)
      assert(pos1 < oldSize, "newDomain must be a subset of the actual domain.")
      val val2 = values(_size)
      val pos2 = positions(val2 - offset)
      values(pos1) = val2
      values(pos2) = val1
      positions(val1 - offset) = pos2
      positions(val2 - offset) = pos1
      _size += 1
    }

    // Notify the constraints
    if (_size != oldSize) {

      // Notify on change events
      onDomainL2.enqueue()

      // Notify on bind events
      if (_size == 1) {
        onBindL1.enqueueBind()
        onBindL2.enqueue()
        onBoundsL1.enqueueBounds()
        onBoundsL2.enqueue()
        _min = values(0)
        _max = _min
      } else {
        // Notify on bound events
        val minOffset = _min - offset
        val maxOffset = _max - offset
        val minChanged = positions(minOffset) >= _size
        val maxChanged = positions(maxOffset) >= _size
        // Update min
        if (minChanged) {
          var i = minOffset + 1
          while (positions(i) >= _size) i += 1
          _min = i + offset
        }
        // Update max
        if (maxChanged) {
          var i = maxOffset - 1
          while (positions(i) >= _size) i -= 1
          _max = i + offset
        }
        if (minChanged || maxChanged) {
          onBoundsL1.enqueueBounds()
          onBoundsL2.enqueue()
        }
      }

      // Notify on remove events
      if (!onDomainL1.isEmpty) {
        var i = _size
        while (i < oldSize) {
          val value = values(i)
          onDomainL1.enqueueRemove(value)
          i += 1
        }
      }
    }
  }

  final override def isEmpty: Boolean = _size == 0

  final override def valueAfter(value: Int): Int = {
    if (value >= _max) value
    else if (value < _min) _min
    else if (_continuous) value + 1
    else {
      var i = value - offset + 1
      while (positions(i) >= _size) i += 1
      offset + i
    }
  }

  final override def valueBefore(value: Int): Int = {
    if (value <= _min) value
    else if (value > _max) _max
    else if (_continuous) value - 1
    else {
      var i = value - offset - 1
      while (positions(i) >= _size) i -= 1
      offset + i
    }
  }

  @inline final def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = {
    if (_continuous) (oldMin to _min - 1).iterator ++ (_max + 1 to oldMax).iterator
    else {
      val newarray = new Array[Int](oldSize - _size)
      fillDeltaArray(oldMin, oldMax, oldSize, newarray)
      newarray.iterator
    }
  }

  @inline final override def fillDeltaArray(oldMin: Int, oldMax: Int, oldSize: Int, arr: Array[Int]): Int = {
    var i = 0
    if (_continuous) {
      var j = oldMin
      while (j < _min) {
        arr(i) = j
        i += 1
        j += 1
      }
      j = _max + 1
      while (j <= oldMax) {
        arr(i) = j
        i += 1
        j += 1
      }
      return i
    } else {
      val M = math.min(oldSize, values.length) - _size
      System.arraycopy(values, _size, arr, 0, M)
      i = M
      var j = oldMin
      while (j < offset) {
        arr(i) = j
        i += 1
        j += 1
      }
      j = offset + values.length
      while (j <= oldMax) {
        arr(i) = j
        i += 1
        j += 1
      }
    }
    i
  }

  def _foreach[U](f: Int => U): Unit = {
    throw new RuntimeException("This should never be called, as it is implemented in CPIntVar")
  }
}