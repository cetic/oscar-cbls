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
import oscar.algo.reversible._
import oscar.cp.core.variables.{CPIntVar, CPVar}

/**
 * Implementation of Sum Constraint:
 * @author Pierre Schaus pschaus@gmail.com
 */
class WeightedSum(val W: Array[Int], val X: Array[CPIntVar], val y: CPIntVar) extends Constraint(y.store, "WeightedSum2") {

  override def associatedVars(): Iterable[CPVar] = X ++ Array(y)

  val x: Array[CPIntVar] = X.map(i => i.asInstanceOf[CPIntVar])
  val w = W.map(i => i)
  val sumBounds = new ReversibleInt(s,0)
  val nBounds = new ReversibleInt(s,0)
  
  
  override def setup(l: CPPropagStrength): Unit = {
    //priorityL2 = CPStore.MAXPRIORL2-1
    X.foreach(_.callPropagateWhenBoundsChange(this))
    y.callPropagateWhenBoundsChange(this)
    val oc = propagate()
    oc
  }
  
  private def setBound(i: Int): Unit = {
    sumBounds.value = sumBounds.value + w(i)*x(i).min
    val tmp = x(nBounds.value)
    val tmpw = w(nBounds.value)
    x(nBounds.value) = x(i)
    w(nBounds.value) = w(i)
    x(i) = tmp
    w(i) = tmpw
    nBounds.incr()
  }
  
  override def propagate(): Unit = {

    var ymin: Int = sumBounds.value
    var ymax: Int = sumBounds.value
    var i = nBounds.value
    while (i < x.size) {
	  ymin += (if (w(i) >= 0) (w(i) * x(i).min) else (w(i) * x(i).max))
      ymax += (if (w(i) >= 0) (w(i) * x(i).max) else (w(i) * x(i).min))
      if (x(i).isBound) {
        setBound(i)
      }
      i += 1
    }
    
    y.updateMax(ymax)
    y.updateMin(ymin)
    
    if (y.max == ymax && y.min == ymin) {
      return
    }

    // w1 x1 + w2 x2 = y
    // x1 = (y - w2 x2)/w1

    i = nBounds.value
    while (i < x.size) {
      if (!x(i).isBound) {
        if (w(i) != 0) { //if w[i] == 0 we cannot prune x[i]
          val maxsumxi = ymax - (if (w(i) >= 0) w(i) * x(i).max else w(i) * x(i).min)
          val minsumxi = ymin - (if (w(i) >= 0) w(i) * x(i).min else w(i) * x(i).max)
          if (w(i) >= 0) {
            val num1 = y.max - minsumxi
            val maxi = (num1.toDouble / w(i)).floor.toInt
            //val maxi = if (num1 % w(i) == 0) num1 / w(i) else (num1.toDouble/w(i)).floor.toInt
            x(i).updateMax(maxi)
            val num2 = y.min - maxsumxi
            val mini = (num2.toDouble / w(i)).ceil.toInt
            //val mini = if (num2 % w(i) == 0) num2/w(i) else (num2.toDouble/w(i)).ceil.toInt
            x(i).updateMin(mini)
          } else {
            val num1 = y.min - maxsumxi
            val maxi = (num1.toDouble / w(i)).floor.toInt
            //val maxi = if (num1 % w(i) == 0) num1 / w(i) else (num1.toDouble/w(i)).floor.toInt 
            x(i).updateMax(maxi)
            val num2 = y.max - minsumxi
            val mini = (num2.toDouble / w(i)).ceil.toInt
            //val mini = if (num2 % w(i) == 0) num2/w(i) else (num2.toDouble/w(i)).ceil.toInt
            x(i).updateMin(mini)
          }
        }
      } else setBound(i)
      i += 1
    }
  }
  
}


