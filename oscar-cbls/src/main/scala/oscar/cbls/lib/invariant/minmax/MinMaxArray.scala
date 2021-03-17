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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  *            Yoann Guyot
  ******************************************************************************/
package oscar.cbls.lib.invariant.minmax

import oscar.cbls._
import oscar.cbls.algo.heap.BinomialHeapWithMoveInt
import oscar.cbls.core.computation.{Bulked, ChangingIntValue, ChangingSetValue, Domain, IntInvariant, IntNotificationTarget, IntValue, InvariantHelper, SetNotificationTarget, SetValue, VaryingDependencies}
import oscar.cbls.core.propagation.{Checker, KeyForElementRemoval}

import scala.collection.immutable.SortedSet

/**
 * Maintains Max(Var(i) | i in cond)
 * @param varss is an array of IntVar, which can be bulked
 * @param cond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
case class MaxArray(varss: Array[IntValue], cond: SetValue = null, default: Long = Long.MinValue)
  extends MiaxArray(varss, cond, varss.map(_.min).min) {

  override def Ord(v: IntValue): Int = -v.valueInt

  override def ExtremumName: String = "Max"

  //More precise bounds
  override def performBulkComputation(bulkedVar: Array[IntValue]) =
    if (cond == null){
      (bulkedVar.foldLeft(Long.MinValue)((acc, intvar) => if (intvar.min > acc) intvar.min else acc),
        bulkedVar.foldLeft(Long.MinValue)((acc, intvar) => if (intvar.max > acc) intvar.max else acc))
    }else super.performBulkComputation(bulkedVar)

  override def checkInternals(c: Checker): Unit = {
    for (v <- this.varss) {
      c.check(this.value >= v.value,
        Some(s"output.value (${this.value}) >= ${v.name}.value (${v.value})"))
    }
  }
}

/**
 * Maintains Min(Var(i) | i in cond)
 * @param varss is an array of IntVar, which can be bulked
 * @param cond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
case class MinArray(varss: Array[IntValue], cond: SetValue = null, default: Long = Long.MaxValue)
  extends MiaxArray(varss, cond, varss.map(_.max).max) {

  override def Ord(v: IntValue): Int = v.valueInt

  override def ExtremumName: String = "Min"

  //More precise bounds
  override def performBulkComputation(bulkedVar: Array[IntValue]) =
    if (cond == null){
      (bulkedVar.foldLeft(Long.MaxValue)((acc, intvar) => if (intvar.min < acc) intvar.min else acc),
        bulkedVar.foldLeft(Long.MaxValue)((acc, intvar) => if (intvar.max < acc) intvar.max else acc))
    }else super.performBulkComputation(bulkedVar)

  override def checkInternals(c: Checker): Unit = {
    for (v <- this.varss) {
      c.check(this.value <= v.value,
        Some(s"this.value (${this.value}) <= ${v.name}.value (${v.value})"))
    }
  }
}

/**
 * Maintains Miax(Var(i) | i in cond)
 * Exact ordering is specified by implementing abstract methods of the class.
 * @param vars is an array of IntVar, which can be bulked
 * @param cond is the condition, can be null
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
abstract class MiaxArray(vars: Array[IntValue], cond: SetValue, default: Long)
  extends IntInvariant with Bulked[IntValue, Domain]
  with VaryingDependencies
  with IntNotificationTarget
  with SetNotificationTarget{

  var keyForRemoval: Array[KeyForElementRemoval] = new Array(vars.length)
  var h: BinomialHeapWithMoveInt = new BinomialHeapWithMoveInt(i => Ord(vars(i)), vars.length, vars.length)

  if (cond != null) {
    registerStaticDependency(cond)
    registerDeterminingDependency(cond)
  }

  /**
   * since the value of the bulkcomputationResult depends on the presence or absence of cond,
   * we register two bcr, so that you can join the correct bulk whataver happens.
   */
  restrictDomain(bulkRegister(vars,if (cond == null) 0L else 1L).union(default))

  if (cond != null) {
    for (i <- cond.value) {
      h.insert(i)
      keyForRemoval(i) = registerDynamicDependency(vars(i), i)
    }
  } else {
    for (i <- vars.indices) {
      h.insert(i)
      keyForRemoval(i) = registerDynamicDependency(vars(i), i)
    }
  }

  finishInitialization()

  override def performBulkComputation(bulkedVar: Array[IntValue]) =
    InvariantHelper.getMinMaxBounds(bulkedVar)

  def ExtremumName: String
  def Ord(v: IntValue): Int

  if (h.isEmpty) {
    this := default
  } else {
    this := vars(h.getFirst).value
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Long, NewVal: Long): Unit = {
    //mettre a jour le heap
    h.notifyChange(index)
    this := vars(h.getFirst).value
  }

  override def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]): Unit = {
    for (added <- addedValues) notifyInsertOn(v: ChangingSetValue, added)
    for(deleted <- removedValues) notifyDeleteOn(v: ChangingSetValue, deleted)
  }

  def notifyInsertOn(v: ChangingSetValue, value: Int): Unit = {
    assert(v == cond)
    keyForRemoval(value) = registerDynamicDependency(vars(value), value)

    //mettre a jour le heap
    h.insert(value)
    this := vars(h.getFirst).value
  }

  def notifyDeleteOn(v: ChangingSetValue, value: Int): Unit = {
    assert(v == cond)

    keyForRemoval(value).performRemove()
    keyForRemoval(value) = null

    //mettre a jour le heap
    h.delete(value)
    if (h.isEmpty) {
      this := default
    } else {
      this := vars(h.getFirst).value
    }
  }
}
