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

package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.Constraint

/**
 * Based on Claude-Guy Quimper Implem (personal webpage)
 *
 * @author Pierre Schaus - pschaus@gmail.com
 */
class GCCUpperBC(val x: Array[CPIntVar],minval: Int, upperCard: Array[Int]) extends Constraint(x(0).store, "GCCUpperBC") {

  override def associatedVars(): Iterable[CPVar] = x

  private[this] val minDomVal = x.map(_.min).min
  private[this] val maxDomVal = x.map(_.max).max
  private[this] val upCard = Array.fill(maxDomVal-minDomVal+1)(2*x.length)

  for {
    i <- 0 until upperCard.length
    v = i + minval - minDomVal
    if (v >= 0 && v < upCard.size )
  } {
    upCard(i + minval - minDomVal) = upperCard(i)
  }
  
  val u = new PartialSum(minDomVal,upCard)
  
  protected[GCCUpperBC] class PartialSum(firstVal: Int,elem: Array[Int]) {
  
    val psum = Array.ofDim[Int](elem.length+1+2+2)
    val firstValue = firstVal -3
    val lastValue = firstVal + elem.length + 1
    psum(0) = 0
    psum(1) = 1
    psum(2) = 2
    var i = 2
    while (i < elem.length +2) {
      psum(i+1) = psum(i) + elem(i-2)
      i += 1
    }
    psum(i+1) = psum(i) + 1
    psum(i+2) = psum(i+1) + 1
    def sum(from: Int, to: Int): Int = {
      if (from <= to) {
        psum(to - firstValue) - psum(from - firstValue -1)
      } else {
        psum(to - firstValue -1) - psum(from - firstValue)
      }
    }
    
    val minValue = firstValue + 3
    
    val maxValue = lastValue - 2
  }
  

  protected[GCCUpperBC] class Interval(val idx: Int, private var _min: Int, private var _max: Int, var minRank: Int, var maxRank: Int) {
    @inline def update(): Unit = {
      _min = x(idx).min
      _max = x(idx).max
    }

    @inline def min: Int = _min
    @inline def setMin(v: Int): Unit = {
      _min = v
      x(idx).updateMin(_min)
    }

    @inline def max: Int = _max
    @inline def setMax(v: Int): Unit = {
      _max = v
      x(idx).updateMax(_max)
    }

    override def toString = "["+min+","+max+"]"
  }

  private[this] val n = x.size
  private[this] var nb = 0
  
  // bounds[1..nb] hold set of min & max in the niv intervals
  // while bounds[0] and bounds[nb+1] allow sentinels
  private[this] val bounds = Array.fill(2 * n + 2)(0)
  private[this] val iv = Array.tabulate(n)(i => new Interval(i, 0, 0, 0, 0))
  private[this] val minSorted = iv.map(i => i)
  private[this] val maxSorted = iv.map(i => i)

  private[this] val t = Array.fill(2 * n + 2)(0) // tree links
  private[this] val d = Array.fill(2 * n + 2)(0) // diffs between critical capacities
  private[this] val h = Array.fill(2 * n + 2)(0) // hall interval links

  override def setup(l: CPPropagStrength): Unit = {
    // Remove values with upperCard == 0
    for (i <- 0 until upperCard.length)
      if(upperCard(i) == 0)
        for(v <- x)
          v.removeValue(minval+i)

    for (i <- 0 until x.size)
      x(i).callPropagateWhenBoundsChange(this)

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
    sortMin()
    sortMax()
    var min = minSorted(0).min
    var max = maxSorted(0).max + 1
    
    
    bounds(0) = u.firstValue + 1
    var last = bounds(0)
    
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
    bounds(nb + 1) = u.lastValue + 1
  }

  @inline private def pathSet(t: Array[Int], start: Int, end: Int, to: Int): Unit = {
    var l = start
    while (l != end) {
      val k = l
      l = t(k)
      t(k) = to
      if(k == l) {
        println(x.map(i => (i.min, i.max)).mkString(","))
        println(upperCard.mkString(","))
        throw new Exception("pathSet encountered an infinite loop on "+l+"!")
      }
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

  def filterlower(): Unit = {
    var i = 1
    var last_t = 0
    var last_h = 0
    while (i <= nb + 1) {
      d(i) = u.sum(bounds(i-1),bounds(i)-1)
      if(d(i) > 0) {
        t(i) = last_t
        h(i-1) = last_h
        last_t = i
        last_h = i - 1
      }
      else {
        t(i) = i + 1
        h(i - 1) = i
      }

      i += 1
    }

    i = 0
    while (i < n) {
      val x = maxSorted(i).minRank
      var y = maxSorted(i).maxRank
      var z = pathMax(t, x + 1)
      val j = t(z)
      d(z) -= 1
      if (d(z) == 0) {
        t(z) = z + 1
        z = pathMax(t, t(z))
        t(z) = j
      }

      pathSet(t, x + 1, z, z) // path compression
      

      // bounds(z) - bounds(y)
      if (d(z) <  u.sum(bounds(y),bounds(z)-1)) {
        throw Inconsistency
      }
      if (h(x) > x) {
        val w = pathMax(h, h(x))
        maxSorted(i).setMin(bounds(w)) //updates the domain
        pathSet(h, x, w, w) // path compression
      }
      // bounds(z) - bounds(y)
      if (d(z) == u.sum(bounds(y),bounds(z)-1)) {
        y = pathMax(h, y)
        val tmp = pathMax(h, j)
        val w = h(tmp)
        pathSet(h, h(y), w, y)
        pathSet(h, j, tmp, y)
        h(y) = w
      }
      i += 1
    }
  }

  def filterUpper(): Unit = {
    var i = nb
    var last_t = nb + 1
    var last_h = nb + 1
    while (i >= 0) {
      d(i) = u.sum(bounds(i),bounds(i+1)-1)
      if(d(i) > 0) {
        t(i) = last_t
        h(i+1) = last_h
        last_t = i
        last_h = i + 1
      }
      else {
        t(i) = i - 1
        h(i + 1) = i
      }

      i -= 1
    }

    i = n-1
    while (i >= 0) { // visit intervals in decreasing min order
      val x = minSorted(i).maxRank
      var y = minSorted(i).minRank
      var z = pathMin(t, x - 1)
      val j = t(z)
      d(z) -= 1
      if (d(z) == 0) {
        t(z) = z - 1
        z = pathMin(t, z - 1)
        t(z) = j
      }
      pathSet(t, x - 1, z, z)
      // bounds(y) - bounds(z)
      if (d(z) < u.sum(bounds(z),bounds(y)-1))
        throw Inconsistency // no solution

      if (h(x) < x) {
        val w = pathMin(h, h(x))
        minSorted(i).setMax(bounds(w) - 1) //updates the domain
        pathSet(h, x, w, w)
      }

      // bounds(y) - bounds(z)
      if (d(z) == u.sum(bounds(z),bounds(y)-1)) {
        y = pathMin(h, y)
        val tmp = pathMin(h, j)
        val w = h(tmp)
        pathSet(h, h(y), w, y)
        pathSet(h, j, tmp, y)
        h(y) = w
      }
      i -= 1
    }
  }

  override def propagate(): Unit = {
    var i = 0
    while (i < x.length) {
      iv(i).update()
      i += 1
    }

    sortIt()

    filterlower()
    filterUpper()
  }

}
