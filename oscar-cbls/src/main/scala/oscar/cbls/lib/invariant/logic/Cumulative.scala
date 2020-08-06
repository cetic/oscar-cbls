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
 ******************************************************************************/
package oscar.cbls.lib.invariant.logic

import oscar.cbls.core.computation.{CBLSIntVar, CBLSSetVar, ChangingIntValue, IntValue, Invariant, ShortIntNotificationTarget}

import scala.collection.immutable.SortedSet

/**
 * Maintains a resource usage profile.
 * @param indices the indices of tasks
 * @param start the start time of tasks
 * @param duration the duration of tasks
 * @param amount the amount that tasks use of this resource
 * @param profile the usage profile of the resource maintained to profile(time) <== sum(task.amount | task.start <= time <= t.start+t.duration)
 * @param active the tasks that are active maintained to active(time) <== (task.indices | task.start <= time <= t.start+t.duration)
 * @author renaud.delandtsheer@cetic.be
 */
case class Cumulative(indices: Array[Int],
                      start: Array[IntValue],
                      duration: Array[IntValue],
                      amount: Array[IntValue],
                      profile: Array[CBLSIntVar],
                      active: Array[CBLSSetVar])
  extends Invariant
  with ShortIntNotificationTarget{

  //horizon is the uppermost index of the profile, which is supposed to be the same as active
  val horizonPlus1 : Int = profile.length
  assert(active.length == horizonPlus1)

  //horizon is the uppermost index of the profile, which is supposed to be the same as active
  val horizon = profile.length-1
  assert(active.length == horizon +1)

  for (v <- start.indices) registerStaticAndDynamicDependency(start(v), v)
  for (v <- duration.indices) registerStaticAndDynamicDependency(duration(v), v)
  for (v <- amount.indices) registerStaticAndDynamicDependency(amount(v), v)

  finishInitialization()

  for (v <- profile) { v.setDefiningInvariant(this); v := 0L }
  for (v <- active) { v.setDefiningInvariant(this); v := SortedSet.empty }

  for (i <- start.indices) insert(start(i).valueInt, duration(i).valueInt, amount(i).value, i)

  def remove(start: Int, duration: Int, amount: Long, index: Int): Unit = {
    if (start < horizonPlus1) {
      for (t <- start until (horizonPlus1 min (start + duration))) {
        profile(t) :-= amount
        active(t).deleteValue(indices(index))
      }
    }
  }

  def insert(start: Int, duration: Int, amount: Long, index: Int): Unit = {
    if (start < horizonPlus1) {
      for (t <- start until (horizonPlus1 min (start + duration))) {
        //sprintln(s"insert($start, $duration, $amount, $index) t=$t")
        profile(t) :+= amount
        active(t).insertValue(indices(index))
      }
    }
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Int, NewVal: Int): Unit = {
    if (start(index) == v) {
      //start
      remove(OldVal, duration(index).valueInt, amount(index).value, index)
      insert(NewVal, duration(index).valueInt, amount(index).value, index)
    } else if (duration(index) == v) {
      //duration
      if (OldVal > NewVal) {
        remove(NewVal + start(index).valueInt, OldVal - NewVal, amount(index).value, index)
      } else {
        insert(OldVal + start(index).valueInt, NewVal - OldVal, amount(index).value, index)
      }
    } else {
      //amount
      if (start(index).value < horizonPlus1) {
        val Delta = NewVal - OldVal
        for (t <- start(index).valueInt until (horizonPlus1 min (start(index).valueInt + duration(index).valueInt))) {
          profile(t) :+= Delta
        }
      }
    }
  }
  //TODO: be more efficient if amount gets to zero.

  //TODO: checkInternals.
}

/**
 * Maintains a resource usage profile.
 * @param start the start time of tasks
 * @param duration the duration of tasks
 * @param amount the amount that tasks use of this resource
 * @param profile the usage profile of the resource maintained to profile(time) <== sum(task.amount | task.start <= time <= t.start+t.duration)
 * @author renaud.delandtsheer@cetic.be
 * @author Jean-Noel Monette
 */
case class CumulativeNoSet(start: Array[IntValue],
                           duration: Array[IntValue],
                           amount: Array[IntValue],
                           profile: Array[CBLSIntVar])
  extends Invariant
  with ShortIntNotificationTarget{

  //horizon is the uppermost indice of the profile, which is supposed to be the same as active
  val horizonPlus1 : Int = profile.length

  //horizon is the uppermost indice of the profile, which is supposed to be the same as active
  val horizon = profile.length-1

  for (v <- start.indices) registerStaticAndDynamicDependency(start(v), v)
  for (v <- duration.indices) registerStaticAndDynamicDependency(duration(v), v)
  for (v <- amount.indices) registerStaticAndDynamicDependency(amount(v), v)

  finishInitialization()

  for (v <- profile) { v.setDefiningInvariant(this); v := 0L }

  for (i <- start.indices) insert(start(i).valueInt, duration(i).valueInt, amount(i).value, i)

  def remove(start: Int, duration: Int, amount: Long, index: Int): Unit = {
    if (start < horizonPlus1) {
      for (t <- start until (horizonPlus1 min (start + duration))) {
        profile(t) :-= amount
      }
    }
  }

  def insert(start: Int, duration: Int, amount: Long, index: Int): Unit = {
    if (start < horizonPlus1) {
      for (t <- start until (horizonPlus1 min (start + duration))) {
        //sprintln(s"insert($start, $duration, $amount, $index) t=$t")
        profile(t) :+= amount
      }
    }
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Int, NewVal: Int): Unit = {
    if (start(index) == v) {
      //start
      remove(OldVal, duration(index).valueInt, amount(index).value, index)
      insert(NewVal, duration(index).valueInt, amount(index).value, index)
    } else if (duration(index) == v) {
      //duration
      if (OldVal > NewVal) {
        remove(NewVal + start(index).valueInt, OldVal - NewVal, amount(index).value, index)
      } else {
        insert(OldVal + start(index).valueInt, NewVal - OldVal, amount(index).value, index)
      }
    } else {
      //amount
      if (start(index).value < horizonPlus1) {
        val Delta = NewVal - OldVal
        for (t <- start(index).valueInt until (horizonPlus1 min (start(index).valueInt + duration(index).valueInt))) {
          profile(t) :+= Delta
        }
      }
    }
  }
  //TODO: be more efficient if amount gets to zero.

  //TODO: checkInternals.
}
