/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 * ****************************************************************************
 */
package oscar.cbls.lib.invariant.minmax

/**This package proposes a set of logic invariants, which are used to define the structure of the problem*/

import oscar.cbls._
import oscar.cbls.algo.heap._
import oscar.cbls.core.computation.{ChangingIntValue, Domain, IntInvariant, IntNotificationTarget, IntValue, SetValue}
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.logic.IntInt2Int

import scala.collection.immutable.SortedSet

abstract class MiaxLin(vars: SortedSet[IntValue])
  extends IntInvariant(initialValue = 0L)
  with IntNotificationTarget{

  require(vars.size > 0L, s"Invariant $this declared with zero vars to max")

  for (v <- vars) registerStaticAndDynamicDependency(v)
  finishInitialization()

  restrictDomain(vars.foldLeft((vars.head.min, vars.head.max))((acc, intvar) =>
    (if (better(intvar.min, acc._1)) intvar.min else acc._1,
      if (better(intvar.max, acc._2)) intvar.max else acc._2)))

  var MiaxCount: Long = 0L

  def better(a: Long, b: Long): Boolean //true if a is strictly more in the direction of the invariant that b

  private def LoadNewMiax(): Unit = {
    var CurrentMiax: Long = vars.head.value
    MiaxCount = 1L
    vars.foreach(v => {
      if (v.value == CurrentMiax) {
        MiaxCount += 1L
      } else if (better(v.value, CurrentMiax)) {
        MiaxCount = 1L
        CurrentMiax = v.value
      }
    })
    this := CurrentMiax
  }

  LoadNewMiax()

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit = {
    assert(vars.contains(v), s"$this notified for not interesting var")
    val MiaxVal = this.newValue
    if (OldVal == MiaxVal && better(MiaxVal, NewVal)) {
      MiaxCount -= 1L
      if (MiaxCount == 0L) LoadNewMiax() //this is where we pay the price.
    } else if (better(NewVal, MiaxVal)) {
      MiaxCount = 1L
      this := NewVal
    } else if (better(MiaxVal, OldVal) && NewVal == MiaxVal) {
      MiaxCount += 1L
    }
  }

  override def checkInternals(c: Checker): Unit = {
    vars.foreach(v => c.check(better(this.value, v.value) || this.value == v.value,
      Some(s"better(output.value (${this.value}), ${v.value}) || output.value == ${v.value}")))
  }
}

/**
 * maintains output = Max(vars)
 * where
 * * output is an IntVar
 * * on is a set of IntVar
 * update is O(n)
 * @author renaud.delandtsheer@cetic.be
 * */
case class MaxLin(vars: SortedSet[IntValue]) extends MiaxLin(vars) {

  override def better(a: Long, b: Long): Boolean = a > b
}

/**
 * maintains output = Min(vars)
 * where
 * * output is an IntVar
 * * on is a set of IntVar
 * update is O(n)
 * @author renaud.delandtsheer@cetic.be
 * */
case class MinLin(vars: SortedSet[IntValue]) extends MiaxLin(vars) {
  override def better(a: Long, b: Long): Boolean = a < b
}

abstract class Miax(vars: SortedSet[IntValue])
  extends IntInvariant
  with IntNotificationTarget{

  for (v <- vars) registerStaticAndDynamicDependency(v)
  finishInitialization()

  restrictDomain(vars.foldLeft((vars.head.min, vars.head.max))((acc, intvar) =>
    (if (better(intvar.min, acc._1)) intvar.min else acc._1,
      if (better(intvar.max, acc._2)) intvar.max else acc._2)))

  def ord(v: IntValue): Long
  def better(a: Long, b: Long): Boolean

  val h: BinomialHeapWithMove[IntValue] = new BinomialHeapWithMove[IntValue](ord, vars.size)

  for (v <- vars) { h.insert(v) }

  this := h.getFirst.value

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit = {
    assert(vars.contains(v), s"$name notified for not interesting var")
    h.notifyChange(v)
    this := h.getFirst.value
  }

  override def checkInternals(c: Checker): Unit = {
    vars.foreach(v => c.check(better(this.value, v.value)
      || this.value == v.value,
      Some(s"better(this.value (${this.value}), ${v.value}) || this.value == ${v.value}")))
  }
}

/**
 * maintains output = Min(vars)
 * deprecated: use MinArray
 * where
 * * output is an IntVar
 * update is O(n*n)
 * @author renaud.delandtsheer@cetic.be
 * */
@deprecated("use the MinArray instead", "always")
case class Min(vars: SortedSet[IntValue]) extends Miax(vars) {
  assert(vars.size > 0L, "Invariant Min declared with zero vars to min")

  override def ord(v: IntValue): Long = v.value

  override def better(a: Long, b: Long): Boolean = a < b
}

object Min{
  def apply(varss: Array[IntValue], ccond: SetValue = null, default: Long = Long.MaxValue) = MinArray(varss, ccond, default)
}

/**
 * maintains output = Max(vars)
 * deprecated use MaxArray
 * where
 * * output is an IntVar
 * update is O(n*n)
 * @author renaud.delandtsheer@cetic.be
 * */
@deprecated("use the MaxArray instead", "always")
case class Max(vars: SortedSet[IntValue]) extends Miax(vars) {
  assert(vars.size > 0L, "Invariant Max declared with zero vars to max")

  override def ord(v: IntValue): Long = -v.value

  override def better(a: Long, b: Long): Boolean = a > b
}

object Max{
  def apply(vars: Array[IntValue], cond: SetValue = null, default: Long = Long.MinValue) = MaxArray(vars, cond, default)
}

/**
 * maintains output = Max(a,b)
 * where output, a, and b are an IntVar
 * use this if you only have two variables to max, otherwise, refer to log implementations
 * @author renaud.delandtsheer@cetic.be
 * */
case class Max2(a: IntValue, b: IntValue)
  extends IntInt2Int(a, b, (x: Long, y: Long) => x.max(y), Domain(a.min.max(b.min),a.max.max(b.max)))

/**
 * maintains output = Min(a,b)
 * where output, a, and b are an IntVar
 * use this if you only have two variables to max, otherwise, refer to log implementations
 * @author renaud.delandtsheer@cetic.be
 * */
case class Min2(a: IntValue, b: IntValue)
  extends IntInt2Int(a, b, (x: Long, y: Long) => x.min(y), Domain(a.min.min(b.min) , a.max.min(b.max)))
