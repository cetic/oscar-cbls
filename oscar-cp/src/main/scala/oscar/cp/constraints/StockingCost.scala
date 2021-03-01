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

import oscar.cp.core._

import scala.collection.mutable.PriorityQueue
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.sum

/**
  * The StockingCost constraint holds when each item is produced before
  * its due date ($X_i <= d_i$), the capacity of the machine is respected
  * (i.e. no more than $c$ variables $X_i$ have the same value), and $H$
  * is an upper bound on the total stocking cost ($sum_i(d_i - X_i) <= H$).
  * 
  * This constraint is useful for modeling
  * Production Planning Problem such as Lot Sizing Problems
  * 
  * @param Y, the variable $X_i$ is the date of production of item $i$ on the machine
  * @param deadline, the integer $d_i$ is the due-date for item $i$
  * @param H, the variable $H$ is an upper bound on the total number of slots all the items are need in stock.
  * @param c is the maximum number of items the machine can produce during one time slot (capacity), 
  *        if an item is produced before its due date, then it must be stocked.
  * @author Ratheil Houndji and Pierre Schaus pschaus@gmail.com
  */
class StockingCost(val Y: Array[CPIntVar], val deadline: Array[Int], val H: CPIntVar, val c: Int) extends Constraint(Y(0).store, "StockingCost") {

  override def associatedVars(): Iterable[CPVar] = Y ++ Array(H)

  val allDiffBC = new AllDiffBC(Y)
  //priorityL2 = 0
  val n = Y.size
  var domMaxMax = Int.MinValue
  var domMinMin = Int.MaxValue
  var k = 0
  while (k < Y.size) {
    val m = Y(k).min
    val M = Y(k).max
    if (m < domMinMin) domMinMin = m
    if (M > domMaxMax) domMaxMax = M
    k += 1
  }
  val X = Array.tabulate(n)(Y(_))
  val Xmax = Array.fill(n+1)(Int.MinValue)
  val d = Array.fill(n)(0)
  val vopt = Array.fill(n)(0)
  
  override def setup(l: CPPropagStrength): Unit = {

    X(0).store.add(H === -sum(0 until X.size)(i => (X(i) - deadline(i))))

    X.foreach(_.callPropagateWhenBoundsChange(this))
    H.callPropagateWhenBoundsChange(this)
    propagate()
  }  
  
  //  --------------map  ---------------------
  var magic = 1
  val mapMagic = Array.fill(domMaxMax-domMinMin+1)(0)
  val map = Array.fill(domMaxMax-domMinMin+1)(0)
 
  def clearMap(): Unit = {
    magic += 1
  }
  
  def index(k: Int) = k-domMinMin
  
  def insert(k: Int,v: Int): Unit = {
    map(index(k)) = v
    mapMagic(index(k)) = magic 
  }
  
  def hasKey(k: Int) = {
    if (k < domMinMin) false
    else mapMagic(index(k)) == magic 
  }
  
  def get(k: Int) = map(index(k))
  
  def printMap (): Unit = {
	  println("keys:"+(domMinMin to domMaxMax).filter(hasKey(_)).mkString(","))
  }
  
  //  --------------incremental sort  ---------------------

  val sortX = Array.tabulate(X.size)(i => i)
    
  def sortIncremental(): Unit = {
    var nn = X.size
    var i = 0
    do {
      var newn = 0
      i = 1
      while (i < nn) {
        if (Y(sortX(i - 1)).max < Y(sortX(i)).max) {
          val tmp = sortX(i - 1)
          sortX(i - 1) = sortX(i)
          sortX(i) = tmp
          newn = i
        }
        i += 1
      }
      nn = newn
    } while (nn > 0);
    k = 0;
    while (k < n) {
      X(k) = Y(sortX(k))
      Xmax(k) = X(k).max
      d(k) = deadline(sortX(k))
      k += 1
    }
    
  }  

  override def propagate(): Unit = {
    allDiffBC.propagate()
    sortIncremental()
    var t = Xmax(0)
    var i = 0
    var j = 0 //open items {j, ... ,i\} must be placed in some slots 
    var k = 0 //items {k, ... ,i\} have same vOpt
    var u = t+1
    clearMap()
    var Hopt = 0
    var ind = 0
    while (j < i || i < n) {
      while (i < n && Xmax(i) == t) {
        i += 1
      }
      // place at most $c$ items into slot $t$ 
      ind = j
      while(ind <= (i-1).min(j+c-1)){ //update Hopt
        Hopt += d(ind) - t
        ind += 1
      }
      
      if (i - j <= c) { // all the open items can be placed in $t$
        val full = (i-j) == c
        ind = k
        while(ind < i){
          vopt(ind) = t
          ind += 1
        }

        j = i
        k = i
        if (full) {
          insert(t, u)
          if (Xmax(i) < t-1) {
            u = Xmax(i)+1
          }
        } else {
          u = Xmax(i)+1
        }
        t = Xmax(i)
      } else { // all the open items can not be placed in $t$
        insert(t,u) //place $c$ items into slot $t$
        j += c 
        t -= 1
      }
    }
    H.updateMin(Hopt)
    val slack = H.max - H.min
    i = 0
    while (i < n) {
      var newmin = vopt(i)  - slack
      
      if (hasKey(newmin)) {
        newmin = vopt(i).min(get(newmin)) 
      }
      X(i).updateMin(newmin)
      i += 1
    }
  }

}