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
package oscar.cbls.lib.invariant.seq

import oscar.cbls.core.computation.{ChangingIntValue, ChangingSeqValue, ChangingSetValue, Domain, DomainHelper, IntNotificationTarget, IntValue, SeqNotificationTarget, SeqUpdate, SeqValue, SetInvariant}
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.SortedSet

object PositionsOf{

  /**
   * Maintains the position of value of variable a in the sequence v.
   * @param v a sequence
   * @param a an intValue, which can be a CBLSIntVar for instance
   * @return a ChangingSetValue that is maintained as the set of position in v where the value is the one of a
   */
  def apply(v: SeqValue, a:IntValue):ChangingSetValue =
    new PositionsOf(v, a)

  /**
   * Maintains the position of value of variable a in the sequence v.
   * @param v a sequence
   * @param a an integer
   * @return a ChangingSetValue that is maintained as the set of position in v where the value is a
   */
  def apply(v: SeqValue, a:Int):ChangingSetValue  =
    new PositionsOfConst(v, a)
}

/**
 * the position of value a in sequence v; default if not in the sequence
 * @param v is a SeqValue
 * @param a is the value that is to locate in the sequence
 */
class PositionsOf(v: SeqValue, a:IntValue)
  extends SetInvariant(v.value.positionsOfValue(a.valueInt).foldLeft(SortedSet.empty[Int])((acc:SortedSet[Int],i:Int) => acc + i) , Domain(0 , DomainHelper.safeAdd(v.max,1)))
  with SeqNotificationTarget with IntNotificationTarget{

  setName(s"PositionOf(${a.name} in ${v.name})")

  registerStaticAndDynamicDependency(v)
  registerStaticAndDynamicDependency(a)

  finishInitialization()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    scheduleForPropagation()
  }

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit = {
    scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    //this is not incremental, but if we assume a small set of value,
    // like 0 or one; there is nothing better we can do
    this := v.value.positionsOfValue(a.valueInt).foldLeft(SortedSet.empty[Int])((acc:SortedSet[Int],i:Int) => acc + i)
  }

  override def checkInternals(c: Checker): Unit = {
    c.check(this.value equals v.value.positionsOfValue(a.valueInt).foldLeft(SortedSet.empty[Int])((acc:SortedSet[Int],i:Int) => acc + i))
  }
}

/**
 * the position of value a in sequence v; default if not in the sequence
 * @param v is a SeqValue
 * @param a is the value that is to locate in the sequence
 */
class PositionsOfConst(v: SeqValue, a:Int)
  extends SetInvariant(v.value.positionsOfValue(a).foldLeft(SortedSet.empty[Int])((acc:SortedSet[Int],i:Int) => acc + i), Domain(0 , DomainHelper.safeAdd(v.max,1)))
  with SeqNotificationTarget{

  setName(s"PositionOf($a in ${v.name})")

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := v.value.positionsOfValue(a).foldLeft(SortedSet.empty[Int])((acc:SortedSet[Int],i:Int) => acc + i)
  }

  override def checkInternals(c: Checker): Unit = {
    c.check(this.value equals v.value.positionsOfValue(a).foldLeft(SortedSet.empty[Int])((acc:SortedSet[Int],i:Int) => acc + i))
  }
}
