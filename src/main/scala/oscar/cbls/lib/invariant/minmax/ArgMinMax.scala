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

import oscar.cbls.algo.heap.BinomialHeapWithMoveInt
import oscar.cbls.core.computation.{Bulked, ChangingIntValue, ChangingSetValue, IntNotificationTarget, IntValue, InvariantHelper, SetInvariant, SetNotificationTarget, SetValue, VaryingDependencies}
import oscar.cbls.core.propagation.{Checker, KeyForElementRemoval}

import scala.collection.immutable.SortedSet

/**
 * Maintains {i in indices of (vars Inter cond) | vars[i] == max(vars(i in indices of (vars Inter cond))}
 * @param vars is an array of IntVar
 * @param cond is the condition, supposed fully acceptant if not specified
 * @param default is the value returned when cond is empty
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
case class ArgMax(vars: Array[IntValue], cond: SetValue = null, default: Int = Int.MinValue)
  extends ArgMiax(vars, cond, default) {
  override def ord(v: Int): Int = -v
}

/**
 * Maintains {i in indices of (vars Inter cond) | vars[i] == min(vars(i in indices of (vars Inter cond))}
 * @param vars is an array of IntVar
 * @param cond is the condition, supposed fully accepting if not specified (must be specified if vars is bulked)
 * @param default is the value returned when cond is empty
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
case class ArgMin[X <: IntValue](vars: Array[X], cond: SetValue = null, default: Int = Int.MaxValue)
  extends ArgMiax(vars, cond, default) {

  override def ord(v: Int): Int = v
}

/**
 * Maintains {i in indices of (varss Inter cond) | varss[i] == miax(varss(i in indices of (varss Inter cond))}
 * Extact ordering is specified by implementiing abstract methods of the class.
 * @param vars is an array of IntVar, which can be bulked
 * @param cond is the condition, can be null
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
abstract class ArgMiax[X <: IntValue](vars: Array[X], cond: SetValue, default: Int)
  extends SetInvariant(initialDomain = vars.indices.start to vars.indices.last)
  with Bulked[X, Unit]
  with VaryingDependencies
  with IntNotificationTarget
  with SetNotificationTarget{

  override def toString:String = {
    s"$name(${InvariantHelper.arrayToString(vars)},$cond,$default)"
  }

  var keyForRemoval: Array[KeyForElementRemoval] = new Array(vars.length)
  var h: BinomialHeapWithMoveInt = new BinomialHeapWithMoveInt(i => ord(vars(i).valueInt), vars.length, vars.length)

  if (cond != null) {
    registerStaticDependency(cond)
    registerDeterminingDependency(cond)
  }

  bulkRegister(vars)

  finishInitialization()

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

  def ord(v: Int): Int

  val firsts = h.getFirsts
  this := firsts.foldLeft(SortedSet.empty[Int])((acc, index) => acc + index)
  var miax = if (firsts.isEmpty) default else vars(h.getFirst).valueInt

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Long, NewVal: Long): Unit = {
    //mettre a jour le heap
    h.notifyChange(index)

    if (vars(h.getFirst).value != miax) {
      miax = vars(h.getFirst).valueInt
      this := h.getFirsts.foldLeft(SortedSet.empty[Int])((acc, index) => acc + index)
    } else if (OldVal == miax) {
      this.deleteValue(index)
      if (this.newValue.isEmpty) {

        for(first <- h.getFirsts){
          this :+= first
        }

        if (this.newValue.isEmpty) {
          miax = default
        } else {
          miax = vars(h.getFirst).valueInt
        }
      }
    } else if (NewVal == miax) {
      this.insertValue(index)
    }
  }

  override def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]): Unit = {
    for (added <- addedValues) notifyInsertOn(v: ChangingSetValue, added)
    for(deleted <- removedValues) notifyDeleteOn(v: ChangingSetValue, deleted)
  }

  @inline
  def notifyInsertOn(v: ChangingSetValue, value: Int): Unit = {
    assert(v == cond && cond != null)
    keyForRemoval(value) = registerDynamicDependency(vars(value), value)

    //mettre a jour le heap
    h.insert(value)

    if (vars(h.getFirst).value != miax) {
      miax = vars(h.getFirst).valueInt
      this := h.getFirsts.foldLeft(SortedSet.empty[Int])((acc, index) => acc + index)
    } else if (vars(value).value == miax) {
      this.insertValue(value)
      miax = vars(h.getFirst).valueInt
    }
  }

  @inline
  def notifyDeleteOn(v: ChangingSetValue, value: Int): Unit = {
    assert(v == cond && cond != null)

    keyForRemoval(value).performRemove()
    keyForRemoval(value) = null

    //mettre a jour le heap
    h.delete(value)

    if (h.isEmpty) {
      miax = default
      this := SortedSet.empty[Int]
    } else if (vars(h.getFirst).value != miax) {
      miax = vars(h.getFirst).valueInt
      this := h.getFirsts.foldLeft(SortedSet.empty[Int])((acc, index) => acc + index)
    } else if (vars(value).value == miax) {
      this.deleteValue(value)
      if (this.newValue.isEmpty) {
        for(first <- h.getFirsts){
          this :+= first
        }
        miax = vars(h.getFirst).valueInt
      }
    }
  }

  override def checkInternals(c: Checker): Unit = {
    var count: Long = 0L
    for (i <- vars.indices) {
      if (cond == null || (cond != null && cond.value.contains(i))) {
        if (vars(i).value == miax) {
          c.check(this.value.contains(i),
            Some(s"this.value.contains($i)"))
          count += 1L
        } else {
          c.check(ord(miax) < ord(vars(i).valueInt),
            Some(s"Ord($miax) < Ord(vars($i).value (${vars(i).value}))"))
        }
      }
    }
    c.check(this.value.size == count, Some("this.value.size == count"))
    h.checkInternals()
    c.check(h.getFirsts.length == this.value.size, Some("h.getFirsts.length == this.value.size"))
    if (cond != null)
      c.check(this.value.subsetOf(cond.value), Some("this.newValue.subsetOf(cond.newValue)"))
  }
}
