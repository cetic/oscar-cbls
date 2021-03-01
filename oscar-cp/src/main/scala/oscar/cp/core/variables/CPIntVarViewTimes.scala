/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cp.core.variables


import oscar.algo.Inconsistency

import scala.util.Random
import oscar.cp.core.Constraint
import oscar.cp.core.CPStore
import oscar.cp.core.watcher.Watcher
import oscar.cp.core.delta.DeltaIntVar

/**
 * Represents a view on variable applying an offset on it.
 * @author Cyrille Dejemeppe Cyrille.Dejemeppe@gmail.com
 * @author Steven Gay steven.gay@uclouvain.be
 * @author Renaud Hartert ren.hartert@gmail.com
 */
final class CPIntVarViewTimes(v: CPIntVar, a: Int) extends CPIntVar with CPIntVarViewLinear {
  private[this] val linearViewData = {
    val above = v match {
      case linear: CPIntVarViewLinear => linear.linearView
      case _ => (1, 0, v)
    }
    (above._1*a, above._2*a, above._3)
  }
  def linearView: (Int, Int, CPIntVar) = linearViewData

  require(a != 0, "a should be different than 0")
  
  final override val store: CPStore = v.store
  final override val context = store

  final override val name: String = s"${v.name} * $a"

  final override def transform(v: Int) = a * this.v.transform(v)

  final override def isBound = v.isBound

  final override def size = v.size

  final override def isEmpty = v.isEmpty

  final override def constraintDegree = v.constraintDegree

  final override def isBoundTo(value: Int): Boolean = if (value % a != 0) false else v.isBoundTo(value / a)
  
  final override def hasValue(value: Int): Boolean = if (value % a != 0) false else v.hasValue(value / a)

  // Scala's division always rounds to the integer closest to zero, but we need flooring/ceiling versions.
  // The following divisions are just a little faster than using the modulo version,
  // and safer+faster than using casting to Double and using Double's ceil/floor 
  @inline private def floor_div(a: Int, b: Int) = {
    val q = a / b
    if (a < 0 && q * b != a) q - 1
    else q
  }

  @inline private def ceiling_div(a: Int, b: Int) = {
    val q = a / b
    if (a > 0 && q * b != a) q + 1
    else q
  }
  
  final override def restrict(newDomain: Array[Int], newSize: Int): Unit = {
    assert(newSize > 0 && newSize <= size )
    val mapped = new Array[Int](newSize)
    var i = newSize
    while (i > 0) {
      i -= 1
      val value = newDomain(i)
      assert(value % a == 0) // always true
      mapped(i) = value / a
    }
    v.restrict(mapped, newSize)
  }

  override final def valueAfter(value: Int): Int = v.valueAfter(floor_div(value, a)) * a

  override final def valueBefore(value: Int): Int = v.valueBefore(ceiling_div(value, a)) * a

  override final def randomValue(rand: Random): Int = v.randomValue(rand) * a

  override final def updateMin(value: Int) = v.updateMin(ceiling_div(value, a))

  override final def updateMax(value: Int) = v.updateMax(floor_div(value, a))

  override final def assign(value: Int) = if (value % a == 0) v.assign(value / a) else throw Inconsistency

  override final def removeValue(value: Int) = if (value % a == 0) v.removeValue(value / a)

  override final def min = a * v.min

  override final def max = a * v.max

  override final def iterator: Iterator[Int] = v.iterator.map(_ * a)

  final override def toString() = "view with multiplicator " + a + " on (" + v + ")";

  override final def callPropagateWhenBind(c: Constraint) = v.callPropagateWhenBind(c)

  override final def callPropagateWhenBoundsChange(c: Constraint) = v.callPropagateWhenBoundsChange(c)
  
  final override def callPropagateWhenBoundsChange(c: Constraint, cond: => Boolean): Unit = v.callPropagateWhenBoundsChange(c, cond)
  
  final override def callPropagateWhenDomainChanges(c: Constraint): Unit = v.callPropagateWhenDomainChanges(c)
  
  final override def callPropagateWhenDomainChanges(c: Constraint, cond: => Boolean): Unit = v.callPropagateWhenDomainChanges(c, cond)

  final override def callPropagateOnChangesWithDelta(c: Constraint): DeltaIntVar = {
    val snap = delta(c)
    v.callPropagateWhenDomainChanges(c)
    snap
  }
  
  final override def callPropagateOnChangesWithDelta(c: Constraint, cond: => Boolean): DeltaIntVar = {
    val snap = delta(c)
    v.callPropagateWhenDomainChanges(c, cond)
    snap
  }
  
  final override def awakeOnChanges(watcher: Watcher): Unit = v.awakeOnChanges(watcher)

  // this method is useful when you have a view defined on a view
  override final def callValBindWhenBind(c: Constraint, variable: CPIntVar) = v.callValBindWhenBind(c, variable)

  override final def callValBindWhenBind(c: Constraint) = v.callValBindWhenBind(c, this)

  // this method is useful when you have a view defined on a view
  override final def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntVar) = v.callUpdateBoundsWhenBoundsChange(c, variable)

  override final def callUpdateBoundsWhenBoundsChange(c: Constraint) = v.callUpdateBoundsWhenBoundsChange(c, this)

  // this method is useful when you have a view defined on a view
  override final def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPIntVar) = v.callValRemoveWhenValueIsRemoved(c, variable)

  override final def callValRemoveWhenValueIsRemoved(c: Constraint) = v.callValRemoveWhenValueIsRemoved(c, this)

  // this method is useful when you have a view defined on a view
  override final def callValBindIdxWhenBind(c: Constraint, variable: CPIntVar, idx: Int) = v.callValBindIdxWhenBind(c, variable, idx)

  override final def callValBindIdxWhenBind(c: Constraint, idx: Int) = v.callValBindIdxWhenBind(c, this, idx)

  // this method is useful when you have a view defined on a view
  override final def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntVar, idx: Int) = v.callUpdateBoundsIdxWhenBoundsChange(c, variable, idx);

  override final def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int) = v.callUpdateBoundsIdxWhenBoundsChange(c, this, idx)

  // this method is useful when you have a view defined on a view
  override final def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPIntVar, idx: Int) = v.callValRemoveIdxWhenValueIsRemoved(c, variable, idx)

  override final def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int) = v.callValRemoveIdxWhenValueIsRemoved(c, this, idx)

  // ----------------------------------

  override final def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = {
    assert(oldMin % a == 0)
    assert(oldMax % a == 0)
    v.delta(oldMin / a, oldMax / a, oldSize).map(_ * a)
  }
  
  def fillDeltaArray(oldMin: Int, oldMax: Int, oldSize: Int, arr: Array[Int]): Int = {
    val m = v.fillDeltaArray(oldMin / a, oldMax / a, oldSize,arr)
    var i = 0
    while (i < m) {
      arr(i) *= a
      i += 1
    }
    m
  }

  @inline def _foreach[U](f: Int => U): Unit = v.foreach(i => f(i * a))
}
  
