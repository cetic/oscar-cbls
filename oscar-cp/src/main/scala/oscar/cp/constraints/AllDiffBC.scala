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

package oscar.cp.constraints;

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.Constraint
import oscar.cp.core.CPSolver
import oscar.cp.util.ArrayUtils
import oscar.algo.reversible.ReversibleInt

import scala.math.min
import scala.math.max
import oscar.algo.reversible.ReversibleInt

/**
 * Based on Claude-Guy Quimper Implem (personal webpage)
 *
 * @author Pierre Schaus - pschaus@gmail.com
 */
class AllDiffBC(val x: Array[CPIntVar]) extends Constraint(x(0).store, "AllDiffBC") {

  override def associatedVars(): Iterable[CPVar] = x

  protected[AllDiffBC] class Interval(var min: Int, var max: Int, var minRank: Int, var maxRank: Int) {
    override def toString = "["+min+","+max+"]"
  }

  private[this] val n = x.size
  private[this] var nb = 0
  private[this] var lastLevel = -1;

  private[this] val INCONSISTENT = 0;
  private[this] val CHANGES = 1;
  private[this] val NO_CHANGES = 2;
  
  // bounds[1..nb] hold set of min & max in the niv intervals
  // while bounds[0] and bounds[nb+1] allow sentinels
  private[this] val bounds = Array.fill(2 * n + 2)(0)
  private[this] val iv = Array.fill(n)(new Interval(0, 0, 0, 0))
  private[this] val minSorted = iv.map(i => i)
  private[this] val maxSorted = iv.map(i => i)

  private[this] val t = Array.fill(2 * n + 2)(0) // tree links
  private[this] val d = Array.fill(2 * n + 2)(0) // diffs between critical capacities
  private[this] val h = Array.fill(2 * n + 2)(0) // hall interval links

  override def setup(l: CPPropagStrength): Unit = {

    for (i <- 0 until x.size) {
      x(i).callPropagateWhenBoundsChange(this)
    }

    propagate()

  }

  // sort the intervals of minSorted such that minSorted(i).min < minSorted(i+1).min forall i
  def sortMin(): Unit = {
    var current = n - 1
    var sorted = false
    while (!sorted) {
      sorted = true
      var i = 0
      while (i < current) {
        if (minSorted(i).min > minSorted(i + 1).min) {
          val t = minSorted(i)
          minSorted(i) = minSorted(i + 1)
          minSorted(i + 1) = t
          sorted = false
        }
        i += 1
      }
      current -= 1
    }
  }

  // sort the intervals of maxSorted such that maxSorted(i).max < maxSorted(i+1).max forall i
  def sortMax(): Unit = {
    var current = 0
    var sorted = false
    while (!sorted) {
      sorted = true
      var i = n - 1
      while (i > current) {
        if (maxSorted(i - 1).max > maxSorted(i).max) {
          val t = maxSorted(i)
          maxSorted(i) = maxSorted(i - 1)
          maxSorted(i - 1) = t
          sorted = false
        }
        i -= 1
      }
      current += 1
    }
  }

  def sortIt(): Unit = {
    sortMin();
    sortMax();
    var min = minSorted(0).min
    var max = maxSorted(0).max + 1;
    bounds(0) = min - 2
    var last = min - 2
    nb = 0
    var i = 0
    var j = 0
    var ok = true
    while (ok) { // merge minSorted[] and maxSorted[] into bounds[]
      if (i < n && min <= max) { // make sure minSorted exhausted first
        if (min != last) {
          nb += 1
          bounds(nb) = min
          last = min
        }
        minSorted(i).minRank = nb
        i += 1
        if (i < n) {
          min = minSorted(i).min
        }
      } else {
        if (max != last) {
          nb += 1
          bounds(nb) = max
          last = max
        }
        maxSorted(j).maxRank = nb
        j += 1
        if (j == n) {
          ok = false
        } else {
          max = maxSorted(j).max + 1
        }
      }
    }
    bounds(nb + 1) = bounds(nb) + 2
  }

  @inline private def pathSet(t: Array[Int], start: Int, end: Int, to: Int): Unit = {
    var l = start
    while (l != end) {
      val k = l
      l = t(k)
      t(k) = to
    }
  }

  @inline private def pathMin(t: Array[Int], ind: Int): Int = {
    var i = ind
    while (t(i) < i) {
      i = t(i)
    }
    i
  }

  @inline private def pathMax(t: Array[Int], ind: Int): Int = {
    var i = ind
    while (t(i) > i) {
      i = t(i)
    }
    i
  }

  def filterlower(): Int = {
    var changes = false
    var i = 1
    while (i <= nb + 1) {
      t(i) = i - 1
      h(i) = i - 1
      d(i) = bounds(i) - bounds(i - 1)
      i += 1
    }
    i = 0
    while (i < n) {
      val x = maxSorted(i).minRank
      val y = maxSorted(i).maxRank
      var z = pathMax(t, x + 1)
      val j = t(z)
      d(z) -= 1
      if (d(z) == 0) {
        t(z) = z + 1
        z = pathMax(t, t(z))
        t(z) = j
      }

      pathSet(t, x + 1, z, z); // path compression
      if (d(z) < bounds(z) - bounds(y)) return INCONSISTENT; // no solution
      if (h(x) > x) {
        val w = pathMax(h, h(x))
        maxSorted(i).min = bounds(w)
        pathSet(h, x, w, w); // path compression
        changes = true;
      }
      if (d(z) == bounds(z) - bounds(y)) {
        pathSet(h, h(y), j - 1, y); // mark hall interval
        h(y) = j - 1; //("hall interval [%d,%d)\n",bounds[j],bounds[y]);
      }
      i += 1
    }
    if (changes) CHANGES;
    else NO_CHANGES;
  }

  def filterUpper(): Int = {
    var changes = false
    var i = 0
    while (i <= nb) {
      t(i) = i + 1
      h(i) = i + 1
      d(i) = bounds(i + 1) - bounds(i);
      i += 1
    }
    i = n-1
    while (i >= 0) { // visit intervals in decreasing min order
      val x = minSorted(i).maxRank
      val y = minSorted(i).minRank
      var z = pathMin(t, x - 1)
      val j = t(z)
      d(z) -= 1
      if (d(z) == 0) {
        t(z) = z - 1
        z = pathMin(t, z - 1)
        t(z) = j
      }
      pathSet(t, x - 1, z, z)
      if (d(z) < bounds(y) - bounds(z)) return INCONSISTENT; // no solution
      if (h(x) < x) {
        val w = pathMin(h, h(x))
        minSorted(i).max = bounds(w) - 1;
        pathSet(h, x, w, w);
        changes = true;
      }
      if (d(z) == bounds(y) - bounds(z)) {
        pathSet(h, h(y), j + 1, y);
        h(y) = j + 1;
      }
      i -= 1
    }
    if (changes) CHANGES;
    else NO_CHANGES;
  }

  override def propagate(): Unit = {
    // not incremental
    var statusLower = CHANGES
    var statusUpper = CHANGES
    var i = 0
    while (i < x.size) {
      iv(i).min = x(i).min
      iv(i).max = x(i).max
      i += 1
    }
    sortIt()
    statusLower = filterlower()
    
    if (statusLower != INCONSISTENT) {
      statusUpper = filterUpper()
    }

    if ((statusLower == INCONSISTENT) || (statusUpper == INCONSISTENT)) {
      throw Inconsistency
    } else if ((statusLower == CHANGES) || (statusUpper == CHANGES)) {
      i = 0
      while (i < x.size) {
        x(i).updateMax(iv(i).max)
        x(i).updateMin(iv(i).min)
        i += 1
      }
    }
  }

}
