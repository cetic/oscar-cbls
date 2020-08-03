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
import oscar.algo.vars.IntVarLike
import oscar.cp.constraints.InSet
import oscar.cp.constraints.InSetReif

import scala.util.Random
import oscar.cp._
import oscar.cp.core.{CPPropagStrength, SubConstraint}
import oscar.cp.core.delta.PropagatorIntVar
import oscar.cp.core.watcher.Watcher
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.delta.DeltaIntVarAdaptable

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
abstract class CPIntVar extends CPVar with IntVarLike {

  def transform(v: Int): Int

  def constraintDegree: Int

  /**
   * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
   */
  def isBound: Boolean

  /**
   *
   * @param v
   * @return true if the variable is bound to value v, false if variable is not bound or bound to another value than v
   */
  def isBoundTo(v: Int): Boolean

  /**
   * Test if a value is in the domain
   * @param value
   * @return  true if the domain contains the value val, false otherwise
   */
  def hasValue(value: Int): Boolean

  /**
   * @param value
   * @return the smallest value > val in the domain, None if there is not value > val in the domain
   */
  def valueAfter(value: Int): Int

  /**
   * @param value
   * @return the largest value < val in the domain, None if there is not value < val in the domain
   */
  def valueBefore(value: Int): Int

  /**
   * @return A random value in the domain of the variable (uniform distribution)
   */
  def randomValue(rand: Random): Int

  /**
   * @return A random value in the domain of the variable (uniform distribution)
   */
  def randomValue: Int = randomValue(store.getRandom)

  /**
   * @return  the size of the domain
   */
  def size: Int

  def getSize = size

  def isEmpty: Boolean

  /**
   * @return  the minimum value in the domain
   */
  def min: Int

  def getMin = min

  /**
   * @return  the maximum value in the domain
   */
  def max: Int

  def getMax = max

  /**
   * Reduce the domain to the singleton {val}, and notify appropriately all the propagators registered to this variable
   * @param value
   * @throws Inconsistency
   */
  def assign(value: Int): Unit

  /**
   * Remove from the domain all values < val, and notify appropriately all the propagators registered to this variable
   * @param value
   * @throws Inconsistency
   */
  def updateMin(value: Int): Unit

  /**
   * Remove from the domain all values > val, and notify appropriately all the propagators registered to this variable
   * @param value
   * @throws Inconsistency
   */
  def updateMax(value: Int): Unit

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callPropagateWhenBind(c: Constraint): Unit

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * the maximum or the minimum value of the domain changes
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callPropagateWhenBoundsChange(c: Constraint): Unit

  def callPropagateWhenBoundsChange(c: Constraint, cond: => Boolean): Unit

  /**
   * Level 1 registration: ask that the valBind(CPIntVar) method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @see oscar.cp.core.Constraint#valBind(CPIntVar)
   */
  def callValBindWhenBind(c: Constraint): Unit

  def callValBindWhenBind(c: Constraint, variable: CPIntVar): Unit

  /**
   * Level 1 registration: ask that the updateBounds(CPIntVar) method of the constraint c is called whenever
   * the minimum or maximum value of the domain changes.
   * @param c
   * @see oscar.cp.core.Constraint#updateBounds(CPIntVar)
   */
  def callUpdateBoundsWhenBoundsChange(c: Constraint): Unit

  def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntVar): Unit

  /**
   * Level 1 registration: ask that the updateBoundsIdx(CPIntVar, int) method of the constraint c is called whenever
   * the minimum or maximum value of the domain changes
   * @param c
   * @param idx, an index that will be given as parameter to updateBoundsIdx(CPIntVar, int)
   * @see Constraint#updateBoundsIdx(CPIntVar, int)
   */
  def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int): Unit

  def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntVar, idx: Int): Unit

  /**
   * Level 1 registration: ask that the valBindIdx(CPIntVar, int) method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @param idx, an index that will be given as parameter to valBindIdx(CPIntVar, int)
   * @see Constraint#valBindIdx(CPIntVar, int)
   */
  def callValBindIdxWhenBind(c: Constraint, idx: Int): Unit

  def callValBindIdxWhenBind(c: Constraint, variable: CPIntVar, idx: Int): Unit

  /**
   * Number of values in common in both domains
   * @param other
   * @return Number of values in common in both domains
   */
  def intersectionSize(other: CPIntVar): Int = {
    if (other.min > max) return 0
    if (other.max < min) return 0
    var res = 0
    var v = other.min.max(min)
    while (v <= other.max.min(max)) {
      if (hasValue(v) && other.hasValue(v)) {
        res += 1
      }
      v += 1
    }
    res
  }
  
  def iterator: Iterator[Int]

  /**
   * While iterating on a CPIntVar, you are allowed to remove **only the value being currently iterated on**.
   * Any other value will throw a ConcurrentModificationException.
   */
  @inline final override def foreach[U](f: Int => U): Unit = {
    /**
     * we really want this to be optimized!
     * this ugly hack force the inlining of the function, which allows it to be lightning fast.
     */

    val (multiplier, offset, v) = this match {
      case linear: CPIntVarViewLinear => linear.linearView
      case _ => (1, 0, this)
    }

    if(v.isInstanceOf[CPIntVarAdaptable]) {
      val va = v.asInstanceOf[CPIntVarAdaptable]
      var origSize = va._size
      var lastSeen: Int = 0
      var wasContinous = false
      var needSparse = !va._continuous

      if(va._continuous) {
        wasContinous = true
        var i = va._min
        while (i <= va._max && !needSparse) {
          lastSeen = i
          f(lastSeen * multiplier + offset)

          if(origSize != va._size) {
            if(va.hasValue(lastSeen) || origSize - 1 != va._size)
              throw new ConcurrentModificationException()
            if(!va._continuous)
              needSparse = true
            origSize = va._size
          }

          i += 1
        }
      }

      if(needSparse) {
        var i = origSize
        // if it **was** continuous, then the biggest values are at the end. We need to stop as soon as
        // we reach the value we last saw in the continuous iteration
        while (i != 0 && (!wasContinous || va.values(i-1) > lastSeen)) {
          i -= 1
          val thisValue = va.values(i)
          f(va.values(i) * multiplier + offset)
          if(origSize != va._size) {
            origSize -= 1
            if(origSize != va._size || va.hasValue(thisValue))
              throw new ConcurrentModificationException()
          }
        }
      }
    }
    else if(v.isInstanceOf[CPBoolVar] || v.isInstanceOf[CPIntVarSingleton]) {
      //at most two different values here!
      val origSize = v.size
      val origMin = v.min
      f(multiplier * origMin + offset)
      if(origSize != v.size && v.min == origMin)
        throw new ConcurrentModificationException()
      if (origMin != v.max)
        f(multiplier * v.max + offset)
    }
    else {
      // fallback
      this._foreach(f)
    }
  }

  /**
   * Fallback for foreach, when the class is not hard-coded inside the main implementation above, or in other
   * particular circumstances.
   */
  protected def _foreach[U](f: Int => U): Unit

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * one of the value is removed from the domain
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callPropagateWhenDomainChanges(c: Constraint): Unit
  def callPropagateWhenDomainChanges(c: Constraint, cond: => Boolean): Unit

  def callPropagateOnChangesWithDelta(c: Constraint): DeltaIntVar
  def callPropagateOnChangesWithDelta(c: Constraint, cond: => Boolean): DeltaIntVar
  
  def awakeOnChanges(watcher: Watcher): Unit


  def callOnChanges(propagate: DeltaIntVar => Boolean, idempotent: Boolean = true)(implicit constraint: Constraint): PropagatorIntVar = {
    val propagator = new PropagatorIntVar(this, 0, propagate)
    propagator.idempotent = idempotent
    callPropagateWhenDomainChanges(propagator)
    propagator
  }

  def callOnChangesIdx(id: Int, propagate: DeltaIntVar => Boolean, idempotent: Boolean = true)(implicit constraint: Constraint): PropagatorIntVar = {
    val propagator = new PropagatorIntVar(this, id, propagate)
    propagator.idempotent = idempotent
    propagator.priority = CPStore.MaxPriorityL2
    callPropagateWhenDomainChanges(propagator)
    propagator
  }

  def filterWhenDomainChangesWithDelta(idempotent: Boolean = false, priority: Int = CPStore.MaxPriorityL2 - 2)(filter: DeltaIntVar => Boolean): DeltaIntVar = {
    val propagator = new PropagatorIntVar(this, 0, filter)
    propagator.idempotent = idempotent
    propagator.priorityL2 = priority
    callPropagateWhenDomainChanges(propagator)
    propagator.snapshot
  }

  /**
   * @param idempot
   * @param priority
   * @param filter Filters. Should return true when there will never be more filtering to do: the filter will be deactivated
   */
  def filterWhenDomainChanges(idempot: Boolean = true, priority: Int = CPStore.MaxPriorityL2 - 2)(filter: => Boolean)(implicit constraint: Constraint): Unit = {
    new SubConstraint(this.store, "filterWhenDomainChanges on  " + this) {
      idempotent = idempot
      priorityL2 = priority

      def setup(l: CPPropagStrength) = callPropagateWhenDomainChanges(this)
      override def propagate() = if (filter) this.deactivate()
      def associatedVars(): Iterable[CPVar] = Iterable(CPIntVar.this)
    }.setup(store.propagStrength)
  }

  /**
   * @param idempot
   * @param priority
   * @param filter Filters. Should return true when there will never be more filtering to do: the filter will be deactivated
   */
  def filterWhenBoundsChange(idempot: Boolean = false, priority: Int = CPStore.MaxPriorityL2 - 2)(filter: => Boolean): Unit = {
    new SubConstraint(this.store, "filterWhenBoundsChange on  " + this) {
      idempotent = idempot
      priorityL2 = priority
      def setup(l: CPPropagStrength) = callPropagateWhenBoundsChange(this)
      override def propagate() = if(filter) deactivate()
      def associatedVars(): Iterable[CPVar] = Iterable(CPIntVar.this)
    }.setup(store.propagStrength)
  }

  /**
   * @param idempot
   * @param priority
   * @param filter Filters. Should return true when there will never be more filtering to do: the filter will be deactivated
   */
  def filterWhenBind(idempot: Boolean = false, priority: Int = CPStore.MaxPriorityL2-2)(filter: => Boolean): Unit = {
    new SubConstraint(this.store, "filterWhenBind on  " + this) {
      idempotent = idempot
      priorityL2 = priority
      def setup(l: CPPropagStrength) = callPropagateWhenBind(this)
      override def propagate() = if(filter) deactivate()
      def associatedVars(): Iterable[CPVar] = Iterable(CPIntVar.this)
    }.setup(store.propagStrength)
  }

  /**
   * Level 1 registration: ask that the valRemove(CPIntVar, int) method of the constraint c is called for each
   * value deletion from the domain
   * @param c
   * @see oscar.cp.core.Constraint#valRemove(CPIntVar, int)
   */
  def callValRemoveWhenValueIsRemoved(c: Constraint): Unit

  def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPIntVar): Unit

  /**
   * Level 1 registration: ask that the valRemoveIdx(CPIntVar, int, int) method of the constraint c is called for each
   * value deletion from the domain
   * @param c
   * @param idx, an index that will be given as parameter to valRemoveIdx(CPIntVar, int, int)
   * @see Constraint#valRemoveIdx(CPIntVar, int, int)
   */
  def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int): Unit

  def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPIntVar, idx: Int): Unit

  /**
   * Remove val from the domain, and notify appropriately all the propagators registered to this variable
   * @param value
   * @throws Inconsistency
   */
  def removeValue(value: Int): Unit
  
  def removeValues(values: Array[Int], nValues: Int): Unit = {
    var i = nValues
    while (i > 0) {
      i -= 1
      removeValue(values(i))
    }
  }
  
  final def removeValues(values: Array[Int]): Unit = removeValues(values, values.length)
  
  /** 
   *  Restrict the domain to be equal to the `newSize` first values contained in `newDomain`.
   *  Observe that the restricted new domain must be a subset of the actual domain of the variable.
   */
  def restrict(newDomain: Array[Int], newSize: Int): Unit
  
  def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int]
  
  def fillDeltaArray(oldMin: Int, oldMax: Int, oldSize: Int, arr: Array[Int]): Int

  def delta(constraint: Constraint,id: Int = 0): DeltaIntVar = {
    val delta = new DeltaIntVarAdaptable(this, id)
    constraint.registerDelta(delta)
    delta
  }

  // ------------------------ some useful methods for java -------------------------


  /**
    * x!=y
    */
  def diff(y: CPIntVar) = new oscar.cp.constraints.DiffVar(this, y)
  /**
    * x!=y
    */
  def diff(y: Int) = new oscar.cp.constraints.DiffVal(this, y)
  /**
    * x==y
    */
  def eq(y: CPIntVar) = new oscar.cp.constraints.Eq(this, y)
  /**
    * x==y
    */
  def eq(y: Int) = new oscar.cp.constraints.EqVal(this, y)

  /**
    * x<y
    */
  def le(y: CPIntVar) = new oscar.cp.constraints.Le(this, y)

  /**
    * x<y
    */
  def le(y: Int) = new oscar.cp.constraints.Le(this, y)

  /**
    * x<=y
    */
  def leEq(y: CPIntVar) = new oscar.cp.constraints.LeEq(this, y)

  /**
    * x<=y
    */
  def leEq(y: Int) = new oscar.cp.constraints.LeEq(this, y)

  /**
    * x>y
    */
  def gr(y: CPIntVar) = new oscar.cp.constraints.Gr(this, y)

  /**
    * x>y
    */
  def gr(y: Int) = new oscar.cp.constraints.Gr(this, y)

  /**
    * x>=y
    */
  def grEq(y: CPIntVar) = new oscar.cp.constraints.GrEq(this, y)

  /**
    * x>=y
    */
  def grEq(y: Int) = new oscar.cp.constraints.GrEq(this, y)



  /**
   * Reified constraint
   * @param v
   * @return  a boolean variable b in the same store linked to x by the relation x == v <=> b == true
   */
  def isEq(v: Int): CPBoolVar = {
    val b = CPBoolVar()(store)
    store.post(new oscar.cp.constraints.EqReif(this, v, b))
    b
  }

  /**
   * Reified constraint
   * @param v
   * @return  a boolean variable b in the same store linked to x by the relation x != v <=> b == true
   */
  def isDiff(v: Int): CPBoolVar = {
    val b = CPBoolVar()(store)
    store.post(new oscar.cp.constraints.DiffReif(this, v, b))
    b
  }

  /**
   * Reified constraint
   * @param y
   * @return  a boolean variable b in the same store linked to x by the relation x != y <=> b == true
   */
  def isDiff(y: CPIntVar): CPBoolVar = {
    val b = CPBoolVar()(store)
    store.post(new oscar.cp.constraints.DiffReifVar(this, y, b))
    b
  }

  /**
   * Reified constraint
   * @param v
   * @return  a boolean variable b in the same store linked to x by the relation x >= v <=> b == true
   */
  def isGrEq(v: Int): CPBoolVar = {
    val b = CPBoolVar()(store)
    store.post(new oscar.cp.constraints.GrEqCteReif(this, v, b))
    b
  }

  /**
    * Reified constraint
    * @param v
    * @return  a boolean variable b in the same store linked to x by the relation x > v <=> b == true
    */
  def isGr(v: Int): CPBoolVar = this.isGrEq(v+1)

  /**
   * Reified constraint
   * @param v
   * @return  a boolean variable b in the same store linked to x by the relation x <= v <=> b == true
   */
  def isLeEq(v: Int): CPBoolVar = {
    val b = CPBoolVar()(store)
    store.post(new oscar.cp.constraints.LeEqCteReif(this, v, b))
    b
  }

  /**
    * Reified constraint
    * @param v
    * @return  a boolean variable b in the same store linked to x by the relation x < v <=> b == true
    */
  def isLe(v: Int): CPBoolVar = this.isLeEq(v-1)


  /**
   * Reified constraint
   * @param y a variable in the same store as x
   * @return  a boolean variable b in the same store linked to x by the relation x >= y <=> b == true
   */
  def isGrEq(y: CPIntVar): CPBoolVar = {
    val b = CPBoolVar()(store)
    store.post(new oscar.cp.constraints.GrEqVarReif(this, y, b))
    b
  }

  /**
    * Reified constraint
    * @param y a variable in the same store as x
    * @return  a boolean variable b in the same store linked to x by the relation x > y <=> b == true
    */
  def isGr(y: CPIntVar): CPBoolVar = this.isGrEq(y+1)

  /**
   * Reified constraint
   * @param v
   * @return  a boolean variable b in the same store linked to x by the relation x <= v <=> b == true
   */
  def isLeEq(v: CPIntVar): CPBoolVar = {
    val b = CPBoolVar()(store)
    store.post(new oscar.cp.constraints.GrEqVarReif(v, this, b))
    b
  }

  /**
    * Reified constraint
    * @param v
    * @return  a boolean variable b in the same store linked to x by the relation x < v <=> b == true
    */
  def isLe(v: CPIntVar): CPBoolVar = this.isLeEq(v-1)


  /**
   * x must take a value from set
   */
  def in(set: Set[Int]): Constraint = new InSet(this, set)

  
  /**
   * b <=> x belongs to set
   */
  def isIn(set: Set[Int]): CPBoolVar = {
    val b = CPBoolVar()(store)
    store.post(new InSetReif(this, set, b))
    b
  }
}

object CPIntVar {

  /** Minimum value that can be contained in an integer domain. */
  final val MinValue: Int = -1000000000
  
  /** Maximum value that can be contained in an integer domain. */
  final val MaxValue: Int = 1000000000
  
  /**
   * Creates a new CP Integer Variable with an iterable as initial domain
   * @param values the iterable defining the possible values for the variable
   * @param name the name of the variable
   * @param store the CPStore in which the variable is created
   * @return a fresh CPIntVar defined in the CPStore store with values as initial domain.
   * The domain of the variable does not contains a given value more than once.
   */
  def apply(values: Iterable[Int], name: String)(implicit store: CPStore): CPIntVar = {
    values match {
      case range: Range  => rangeDomain(range, name, store)
      case set: Set[Int] => setDomain(set, name, store)
      case iterable      => iterableDomain(iterable, name, store)
    }
  }

  def sparse(values: Iterable[Int], name: String)(implicit store: CPStore): CPIntVar = {
    val min = values.min
    val max = values.max
    val variable = new CPIntVarAdaptable(store, min, max, false, name)
    if (max - min + 1 > values.size) {
      val set = values.toSet
      var v = min + 1
      while (v < max) {
        if (!set.contains(v)) {
          variable.removeValue(v)
        }
        v += 1
      }
    }
    variable
  }

  def sparse(values: Iterable[Int])(implicit store: CPStore): CPIntVar = sparse(values, "")(store)

  def sparse(minValue: Int, maxValue: Int, name: String)(implicit store: CPStore): CPIntVar = {
    new CPIntVarAdaptable(store, minValue, maxValue, false, name)
  }

  def sparse(minValue: Int, maxValue: Int)(implicit store: CPStore): CPIntVar = sparse(minValue, maxValue, "")(store)

  /**
   * Creates a new CP Integer Variable with an iterable as initial domain
   * @param values the iterable defining the possible values for the variable
   * @param store the CPStore in which the variable is created
   * @return a fresh CPIntVar defined in the CPStore store with values as initial domain.
   * The domain of the variable does not contains a given value more than once.
   */
  def apply(values: Iterable[Int])(implicit store: CPStore): CPIntVar = apply(values, "")(store)

  /**
   * Creates a new CP Integer Variable with an array as initial domain
   * @param values the array defining the possible values for the variable
   * @param name the name of the variable
   * @param store the CPStore in which the variable is created
   * @return a fresh CPIntVar defined in the CPStore store with values as initial domain.
   * The domain of the variable does not contains a given value more than once.
   */
  def apply(values: Array[Int], name: String)(implicit store: CPStore): CPIntVar = {
    iterableDomain(values, name, store)
  }

  /**
   * Creates a new CP Integer Variable with an array as initial domain
   * @param values the array defining the possible values for the variable
   * @param store the CPStore in which the variable is created
   * @return a fresh CPIntVar defined in the CPStore store with values as initial domain.
   * The domain of the variable does not contains a given value more than once.
   */
  def apply(values: Array[Int])(implicit store: CPStore): CPIntVar = apply(values, "")(store)

  /**
   * Creates a new CP Integer Variable with all the values contained in (minValue to maxValue) as initial domain
   * @param minValue the minimal value of the domain
   * @param maxValue the maximal value of the domain
   * @param name the name of the variable
   * @param store the CPStore in which the variable is created
   * @return a fresh CPIntVar defined in the CPStore store with all the values contained in (minValue to maxValue)
   * as initial domain.
   */
  def apply(minValue: Int, maxValue: Int, name: String)(implicit store: CPStore): CPIntVar = {
    new CPIntVarAdaptable(store, minValue, maxValue, true, name)
  }

  /**
   * Creates a new CP Integer Variable with all the values contained in (minValue to maxValue) as initial domain
   * @param minValue the minimal value of the domain
   * @param maxValue the maximal value of the domain
   * @param store the CPStore in which the variable is created
   * @return a fresh CPIntVar defined in the CPStore store with all the values contained in (minValue to maxValue)
   * as initial domain.
   */
  def apply(minValue: Int, maxValue: Int)(implicit store: CPStore): CPIntVar = apply(minValue, maxValue, "")(store)

  /**
   * Creates a new CP Integer Variable assigned to value
   * @param value the single value contained in the domain
   * @param name the name of the variable
   * @param store the CPStore in which the variable is created
   * @return a fresh CPIntVar defined in the CPStore store with a single value as initial domain.
   */
  def apply(value: Int, name: String)(implicit store: CPStore): CPIntVar = {
    new CPIntVarSingleton(store, value, name) // CPIntVarImpl(store, value, value, name)
  }

  /**
   * Creates a new CP Integer Variable assigned to value
   * @param value the single value contained in the domain
   * @param store the CPStore in which the variable is created
   * @return a fresh CPIntVar defined in the CPStore store with a single value as initial domain.
   */
  def apply(value: Int)(implicit store: CPStore): CPIntVar = {
    new CPIntVarSingleton(store, value, "") // CPIntVarImpl(store, value, value, name)
  }

  @deprecated("use apply(values: Iterable[Int], name: String)(implicit store: CPStore) instead", "1.0")
  def apply(store: CPStore, values: Iterable[Int], name: String): CPIntVar = apply(values, name)(store)

  @deprecated("use apply(values: Iterable[Int])(implicit store: CPStore) instead", "1.0")
  def apply(store: CPStore, values: Iterable[Int]): CPIntVar = apply(store, values, "")

  @deprecated("use apply(values: Array[Int], name: String)(implicit store: CPStore) instead", "1.0")
  def apply(store: CPStore, values: Array[Int], name: String): CPIntVar = apply(values, name)(store)

  @deprecated("use apply(values: Array[Int])(implicit store: CPStore) instead", "1.0")
  def apply(store: CPStore, values: Array[Int]): CPIntVar = apply(store, values, "")

  @deprecated("use apply(minValue: Int, maxValue: Int, name: String)(implicit store: CPStore) instead", "1.0")
  def apply(store: CPStore, minValue: Int, maxValue: Int, name: String): CPIntVar = apply(minValue, maxValue, name)(store)

  @deprecated("use apply(minValue: Int, maxValue: Int)(implicit store: CPStore) instead", "1.0")
  def apply(store: CPStore, minValue: Int, maxValue: Int): CPIntVar = apply(minValue, maxValue, "")(store)

  @deprecated("use apply(value: Int, name: String)(implicit store: CPStore) instead", "1.0")
  def apply(store: CPStore, value: Int, name: String): CPIntVar = apply(value, name)(store)

  @deprecated("use apply(value: Int)(implicit store: CPStore) instead", "1.0")
  def apply(store: CPStore, value: Int): CPIntVar = apply(value, "")(store)

  /** Builds a CPIntVar from a range */
  private def rangeDomain(domain: Range, name: String, store: CPStore): CPIntVar = {
    if (domain.max - domain.min < domain.size - 1) iterableDomain(domain, name, store)
    else new CPIntVarAdaptable(store, domain.min, domain.max, true, name)
  }

  /** Builds a CPIntVar from an iterable */
  private def iterableDomain(domain: Iterable[Int], name: String, store: CPStore): CPIntVar = setDomain(domain.toSet, name, store)

  /** Builds a CPIntVar from a set */
  private def setDomain(domain: Set[Int], name: String, store: CPStore): CPIntVar = {
    val min = domain.min
    val max = domain.max
    if (max - min + 1 == domain.size) new CPIntVarAdaptable(store, domain.min, domain.max, true, name)
    else {
      val x = new CPIntVarAdaptable(store, domain.min, domain.max, false, name)
      for (v <- min to max if !domain.contains(v)) {
        x.removeValue(v)
      }
      x
    }
  }
}

  
