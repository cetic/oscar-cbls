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

package oscar.cp.constraints;

import scala.math.max
import scala.math.min
import oscar.algo.reversible.ReversibleInt
import oscar.algo.reversible.ReversibleSparseSet
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint

/**
 * A full Arc-Consistent Element Constraint: y(x) == z
 *
 * @author Renaud Hartert ren.hartert@gmail.com
 * @author Pierre Schaus pschaus@gmail.com
 */
class ElementVarAC(y: Array[CPIntVar], x: CPIntVar, z: CPIntVar) extends Constraint(x.store, "ACElementVar") {

  override def associatedVars(): Iterable[CPVar] = y ++ Array(x, z)

  // Range of possible indexes
  private[this] val minId = max(0, x.min)
  private[this] val maxId = min(x.max, y.length - 1)
  private[this] val nIndexes = maxId - minId + 1

  // Range of possible values
  private[this] val minValue = max(z.min, (y.map(_.min).min))
  private[this] val maxValue = min(z.max, (y.map(_.max).max))
  private[this] val nValues = maxValue - minValue + 1

  // Number of supports for the value v i.e number of indices i such that v is in y(i)
  private[this] val nSupports = Array.fill(nValues)(new ReversibleInt(s, 0)) // for some reasons, does not work with cache

  // For all indices i in x: intersect(i) is the size of the intersection between y(i) and z
  private[this] val intersect = Array.fill(nIndexes)(new ReversibleSparseSet(s, z.min, z.max))

  // Used to iterate on the domain of the variables
  private[this] val values = new Array[Int](y.map(_.size).max max x.size max z.size)
  
  // Replaces this constraint by an Equality constraint.
  private[this] val equality = new ElementEq(y, x, z, values)

  final override def setup(l: CPPropagStrength): Unit = {
    z.updateMax(y.map(_.max).max)
    z.updateMin(y.map(_.min).min)
    x.updateMin(0)
    x.updateMax(y.size - 1)
    adjustX()

    init()
    if(isActive) {
      x.callValRemoveWhenValueIsRemoved(this)
      z.callValRemoveWhenValueIsRemoved(this)
      for (i <- x.min to x.max; if x hasValue i) {
        y(i).callValRemoveIdxWhenValueIsRemoved(this, i)
      }
    }
  }

  @inline private def init(): Unit = {
    //resetData() // Mandatory if propagate is called after the initial call
    initData()

    var i = x.fillArray(values)
    while (i > 0) {
      i -= 1
      val v = values(i)
      if (intersect(v - minId).size == 0) {
        x.removeValue(v)
      }
    }

    if (x.isBound) return bindX()

    i = z.fillArray(values)
    while (i > 0) {
      i -= 1
      val v = values(i)
      if (nSupports(v - minValue).value == 0) {
        z.removeValue(v)
      }
    }
  }

  // Initializes data structures
  private def initData(): Unit = {
    for (i <- x) {
      val keep =
        (for (v <- y(i); if (z.hasValue(v))) yield {
          nSupports(v - minValue).incr()
          v
        }).toSet
      for (v <- intersect(i - minId).min to intersect(i - minId).max; if !(keep.contains(v))) {
        intersect(i - minId).removeValue(v)
      }
    }
  }

  // Reset the content of both data structures
  private def resetData(): Unit = {
    for (i <- 0 until intersect.size)
      intersect(i).makeEmpty()
    for (v <- 0 until nSupports.size)
      nSupports(v).setValue(0)
  }

  // If y(i) has an intersection with z, the number of supports of the  value v is reduced by 1
  // Remove v from y(i)
  final override def valRemoveIdx(cpvar: CPIntVar, i: Int, v: Int): Unit = {
    // we must check that x has value to avoid reducing twice for the same removal
    // y(i) might loose the value and i is also removed ...
    if (x.hasValue(i))
      reduceSupports(v)
    reduceIntersect(i, v)
  }

  final override def valRemove(cpvar: CPIntVar, v: Int): Unit = {
    if (cpvar == x) removeFromX(v)
    else removeFromZ(v)
  }

  // Reduces the number of supports of the value v
  @inline private def reduceSupports(value: Int): Unit = {
    if (!(value < minValue || value > maxValue)) {
      val v = value - minValue
      val nSup = nSupports(v)
      if (nSup.value > 0 && nSup.decr() == 0) z.removeValue(value)
    }
  }

  // Removes the value v from the intersection between y(i) and z
  @inline private def reduceIntersect(i: Int, v: Int): Unit = {
    val id = i - minId
    val removed = intersect(id).removeValue(v)
    if (removed && intersect(id).isEmpty) x.removeValue(i)
  }

  // Removes v from all the intersections
  @inline private def removeFromZ(v: Int): Unit = {
    nSupports(v - minValue).value = 0
    var i = x.fillArray(values)
    while (i > 0) {
      i -= 1
      reduceIntersect(values(i), v)
    }
  }

  // If x is bound, this constraint is replaced by an Equality constraint
  // else, the number of supports for all values v in y(i) is reduced by 1
  @inline private def removeFromX(id: Int): Unit = {
    if (x.isBound) bindX()
    else {
      // Decrement the number of supports for each value in y(id)
      var i = y(id).fillArray(values)
      while (i > 0) {
        i -= 1
        reduceSupports(values(i))
      }
    }
  }

  @inline private def bindX(): Unit = {
    s.post(equality)
    deactivate()
  }

  // Removes each value i in x that is not a valid id in y
  @inline private def adjustX(): Unit = {
    x.updateMin(0)
    x.updateMax(y.length - 1)
    if (x.isBound)
      bindX()
  }
}

class ElementEq(ys: Array[CPIntVar], x: CPIntVar, z: CPIntVar, values: Array[Int]) extends Constraint(x.store, "ElementEq") {

  override def associatedVars(): Iterable[CPVar] = ys ++ Array(z, x)

  private[this] var y: CPIntVar = null
  
  final override def setup(l: CPPropagStrength): Unit = {
    y = ys(x.min)
    propagate()
    if(isActive) {
      y.callValRemoveWhenValueIsRemoved(this)
      z.callValRemoveWhenValueIsRemoved(this)
    }
  }
  
  final override def propagate(): Unit = {
    var i = y.fillArray(values)
    while (i > 0) {
      i -= 1
      val value = values(i)
      if (!z.hasValue(value))
        y.removeValue(value)
    }
    i = z.fillArray(values)
    while (i > 0) {
      i -= 1
      val value = values(i)
      if (!y.hasValue(value))
        z.removeValue(value)
    }
  }
  
  // FIXME: should be idempotent (not allowed yet for L1 events)
  final override def valRemove(intVar: CPIntVar, value: Int): Unit = {
    if (intVar == y) z.removeValue(value)
    else y.removeValue(value)
  } 
}

