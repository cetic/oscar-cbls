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
package oscar.cp.core.variables

import scala.util.Random
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.watcher.Watcher
import oscar.cp.core.delta.DeltaIntVar

/**
 * Represents a view on variable applying an offset on it.
 * @author Pierre Schaus pschaus@gmail.com
 */
class CPIntVarViewOffset(v: CPIntVar, offset: Int) extends CPIntVar with CPIntVarViewLinear {

  private[this] val linearViewData = {
    val above = v match {
      case linear: CPIntVarViewLinear => linear.linearView
      case _ => (1, 0, v)
    }
    (above._1, above._2 + offset, above._3)
  }
  def linearView: (Int, Int, CPIntVar) = linearViewData

  override val store: CPStore = v.store
  final override val context = store

  override val name: String = s"${v.name} + $offset"
    
  final override def transform(v: Int) = offset + this.v.transform(v)    
	
	final override def isBound = v.isBound
	
  final override def size = v.size
	
  final override def isEmpty = v.isEmpty
	
	final override def constraintDegree = v.constraintDegree
	
	final override def isBoundTo(value: Int): Boolean = v.isBoundTo(value - offset)
	
	final override def hasValue(value: Int): Boolean = v.hasValue(value - offset)
	
	final override def valueAfter(value: Int): Int = v.valueAfter(value - offset) + offset
	
	final override def valueBefore(value: Int): Int = v.valueBefore(value - offset) + offset
	
	final override def randomValue(rand: Random): Int = v.randomValue(rand) + offset
	
	final override def updateMin(value: Int) = v.updateMin(value - offset)
	
	final override def assign(value: Int) = v.assign(value - offset)

	final override def updateMax(value: Int) = v.updateMax(value - offset)
	
	final override def removeValue(value: Int) = v.removeValue(value - offset)
	
	final override def min = v.min + offset
	
	final override def max = v.max + offset
	
	final override def iterator = {
		v.iterator.map(_ + offset)
	}
  
  @inline final def restrict(newDomain: Array[Int], newSize: Int): Unit = {
    assert(newSize > 0 && newSize <= size)
    val mapped = new Array[Int](newSize)
    var i = newSize
    while (i > 0) {
      i -= 1
      mapped(i) = newDomain(i) - offset
    }
    v.restrict(mapped, newSize)
  }
  
  final override def fillArray(array: Array[Int]): Int = {
    val m = v.fillArray(array)
    var i = 0
    while (i < m) {
      array(i) += offset
      i += 1
    }
    m
  } 
  
  final override def toArray: Array[Int] = {
    val array = new Array[Int](size)
    fillArray(array)
    array
  }
  
	
	final override def toString() = s"view with shift $offset on ($v)";
		
	final override def callPropagateWhenBind(c: Constraint) = v.callPropagateWhenBind(c)
	
	final override def callPropagateWhenBoundsChange(c: Constraint) = v.callPropagateWhenBoundsChange(c)
  
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
	
	// this method is useful when you have a view final override defined on a view
	final override def callValBindWhenBind(c: Constraint, variable: CPIntVar) = v.callValBindWhenBind(c, variable)
	
	final override def callValBindWhenBind(c: Constraint) = v.callValBindWhenBind(c,this)
	
	
	// this method is useful when you have a view final override defined on a view
	final override def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntVar) = v.callUpdateBoundsWhenBoundsChange(c, variable)
	
	final override def callUpdateBoundsWhenBoundsChange(c: Constraint) = v.callUpdateBoundsWhenBoundsChange(c,this)
	
	// this method is useful when you have a view final override defined on a view
	final override def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPIntVar) = v.callValRemoveWhenValueIsRemoved(c,variable)
		
	final override def callValRemoveWhenValueIsRemoved(c: Constraint) = v.callValRemoveWhenValueIsRemoved(c,this)
	
	// this method is useful when you have a view final override defined on a view
	final override def callValBindIdxWhenBind(c: Constraint, variable: CPIntVar,idx: Int) = v.callValBindIdxWhenBind(c, variable,idx)
	
	final override def callValBindIdxWhenBind(c: Constraint, idx: Int) = v.callValBindIdxWhenBind(c,this,idx)
	
	// this method is useful when you have a view final override defined on a view
	final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntVar, idx: Int) = v.callUpdateBoundsIdxWhenBoundsChange(c, variable, idx);
		
	final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int) = v.callUpdateBoundsIdxWhenBoundsChange(c,this,idx)
	

	
	// this method is useful when you have a view final override defined on a view
	final override def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPIntVar, idx: Int) = v.callValRemoveIdxWhenValueIsRemoved(c,variable,idx)
	
	final override def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int) = v.callValRemoveIdxWhenValueIsRemoved(c,this,idx)

	// ----------------------------------
	
	final override def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = v.delta(oldMin - offset,oldMax - offset,oldSize).map(_ + offset)
  
  def fillDeltaArray(oldMin: Int, oldMax: Int, oldSize: Int, arr: Array[Int]): Int = {
    val m = v.fillDeltaArray(oldMin - offset,oldMax - offset,oldSize,arr)
    var i = 0
    while (i < m) {
      arr(i) += offset
      i += 1
    }
    m
  }

  @inline def _foreach[U](f: Int => U): Unit = v.foreach(i => f(i + offset))
}
  
