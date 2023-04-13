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
package oscar.cbls.lib.invariant.numeric

import oscar.cbls._
import oscar.cbls.core.computation.{CBLSIntVar, ChangingIntValue, Domain, IntInvariant, IntNotificationTarget, IntValue, Store}
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.logic.LazyIntInt2Int

import scala.annotation.tailrec

/**
 * Maintains output to the smallest value such that
 * output >= from
 * (output - shift) MOD period > zone
 * (output - shift + length) MOD period > zone
 * of course, it is required that length is < period - zone, and exception is thrown otherwise.
 *
 * For instance, suppose that some task can only happen during open day (Mon-Fri),
 * let 'from" being the lowest starting date, and 'length' its duration.
 * the invariant will check that the task can be finished by friday of the week, and if not,
 * will propose the next monday. 'shift' specifies says what is the starting day at zero.
 * zone is the forbidden zone. it starts at the beginning of the cycle.
 *
 * Suppose you represent days starting from zero, and zero is a monday,
 * and you want to round up to the next open day (sa and su are closed day, the correct declaration is:
 * RoundUpModulo(from,duration,7L,2L,5L)
 *
 * @param from the starting date of the task. it can start later.
 * @param duration the duration of the task.
 * @param period the period of the forbidden-allowed pattern
 * @param zone the size of the forbidden zone. it starts at the beginning of the period
 * @param shift the first period starts later than zero. it starts at shift. the duration before its start is allowed.
 * @author renaud.delandtsheer@cetic.be
 */
case class RoundUpModulo(from: IntValue, duration: IntValue, period: Long, zone: Long, shift: Long)
  extends LazyIntInt2Int(from, duration, (from: Long, duration: Long) => {
    require(duration <= period - zone, s"duration $duration<= period $period- zone $zone")
    require(period != 0L)
    val reducedfrom = (from + period - shift) % period
    if (reducedfrom < zone)
      from + (zone - reducedfrom) //to restore the modulo, we must compute this
    else if (reducedfrom + duration > period)
      from + (period + zone - reducedfrom)
    else
      from
  }, Domain(from.min , from.max + zone))

object TestRoundUpModulo extends App {

  def n2day(n: Long): String =
    n % 7L match {
      case 0L => "lu"
      case 1L => "ma"
      case 2L => "me"
      case 3L => "je"
      case 4L => "ve"
      case 5L => "sa"
      case 6L => "di"

    }
  val m = Store()

  val from = CBLSIntVar(m, 0L, fullRange, "from")
  val duration = CBLSIntVar(m, 2L, fullRange, "duration")

  val r = RoundUpModulo(from, duration, 7L, 2L, 0L)

  m.close()

  for (i <- 0 to 20) {
    from := i
    println(n2day(from.value) + " " + from.value + " " + duration + " " + n2day(r.value) + " " + r.value)
  }
}

/**
 * Maintains output to the smallest value such that
 * output >= from
 * the interval [output ; output + length] does not overlap with the intervals
 * given in ForbiddenZones.
 * A forbidden zone (start, end) defines the forbidden interval [start, end].
 *
 * Warning: the duration should never be zero.
 *
 * @param from
 * @param duration
 * @param forbiddenZones
 * @author renaud.delandtsheer@cetic.be
 */
case class RoundUpCustom(from: IntValue, duration: IntValue, forbiddenZones: List[(Long, Long)])
  extends IntInvariant(initialDomain = Domain(from.min , forbiddenZones.maxBy(_._2)._2 + 1L))
  with IntNotificationTarget{

  /**
   * These must be computed first.
   */
  private val sortedRegularizedZones = regularizeZones(forbiddenZones.sortBy(_._1))
  private val forbiddenStarts: Array[Long] = sortedRegularizedZones.map(_._1).toArray
  private val forbiddenEnds: Array[Long] = sortedRegularizedZones.map(_._2).toArray

  registerStaticAndDynamicDependenciesNoID(from, duration)
  finishInitialization()
  this := roundup()

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit = {
    scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := roundup()
  }

  /**
   * Recursively merges overlapping zoextends LazyIntVarIntVar2IntVarFun(from, duration, (from: Long, duration: Long) => {nes.
   */
  private def regularizeZones(zones: List[(Long, Long)]): List[(Long, Long)] = {
    zones match {
      case List(head) => zones
      case (a, b) :: tail => {
        assert(a <= b)
        absorb(a, b, regularizeZones(tail))
      }
      case Nil => zones
    }
  }

  /**
   * Recursively absorbs zones, as far as they overlap with the current one, that is:
   * (base case) if the list is empty or the current zone (a, b) does not overlap
   * with the next one (the first one in the sorted list), nothing to do
   * (inductive case) else merges the current zone with the next one
   */
  @tailrec
  private def absorb(a: Long, b: Long, list: List[(Long, Long)]): List[(Long, Long)] = {
    list match {
      case (c, d) :: tailTail if (b >= c) => absorb(a, (d max b), tailTail)
      case newTail => (a, b) :: newTail
    }
  }

  def roundup(): Long = {
    var newStart: Long = from.value
    var lastZoneBeforeNewStart = findLastStartBefore(from.value)
    while (true) {
      if (lastZoneBeforeNewStart != -1
        && forbiddenEnds(lastZoneBeforeNewStart) >= newStart) {
        //we cannot start here, we are in a forbidden zone
        //the zone before start does not change,
        //we just need to wait for the end of this zone
        newStart = forbiddenEnds(lastZoneBeforeNewStart) + 1L
      } else if ((lastZoneBeforeNewStart + 1 < forbiddenStarts.length)
        && (forbiddenStarts(lastZoneBeforeNewStart + 1) <= newStart + duration.value - 1L)) {
        //we can start here, but we end up in a forbidden zone
        //we have to start after the next zone
        newStart = forbiddenEnds(lastZoneBeforeNewStart + 1) + 1L
        lastZoneBeforeNewStart += 1
      } else {
        return newStart
      }
    }
    1L
  }

  private def findLastStartBefore(d: Long): Int = {
    var up = forbiddenStarts.length - 1
    var down = -1
    while (down + 1 < up) {
      val mid = (up + down) / 2
      if (forbiddenStarts(mid) == d) {
        return mid
      } else if (forbiddenStarts(mid) < d) {
        down = mid
      } else {
        up = mid
      }
    }
    if (forbiddenStarts(up) <= d) up
    else down
  }

  override def checkInternals(c: Checker): Unit = {
    c.check(from.value <= this.value)
    for ((a, b) <- forbiddenZones) {
      c.check((this.value > b) || (this.value + duration.value - 1L < a), Some("from.value = " + from.value + " (this.value " + this.value + " > zoneEnd " + b + ") || (this.value " + this.value + "+ duration.value " + duration.value + " -1L < zoneStart " + a + ")"))
    }

    for (i <- from.value until this.value) {
      c.check(forbiddenZones.exists(ab =>
        !((i + duration.value - 1L < ab._1) || (ab._2 < i))),
        Some(s"should be not suitable at position $i $duration exists:"
          + forbiddenZones.exists(ab =>
            !((i + duration.value - 1L < ab._1) || (ab._2 < i)))
          + "filter:" + forbiddenZones.filter(ab =>
            !((i + duration.value - 1L < ab._1) || (ab._2 < i)))))
    }
  }
}

object TestRoundUpCustom extends App {

  def n2day(n: Long): String =
    n % 7L match {
      case 0L => "lu"
      case 1L => "ma"
      case 2L => "me"
      case 3L => "je"
      case 4L => "ve"
      case 5L => "sa"
      case 6L => "di"

    }
  val m = Store()

  val from = CBLSIntVar(m, 0L, fullRange, "from")
  val duration = CBLSIntVar(m, 2L, fullRange, "duration")

  val r = RoundUpCustom(from, duration, List((3L, 4L), (9L, 12L)))

  m.close()

  for (i <- 0 to 20) {
    from := i
    println(n2day(from.value) + " " + from.value + " " + duration + " " + n2day(r.value) + " " + r.value)
  }
}

/**
 * Maintains output to the duration value of a task, with respect to a possible
 * interruption by a pre-emptive task.
 * The task is extended with the duration of the pre-emptive task, if this
 * pre-emptive task occurs during that task execution.
 * If necessary, the pre-empted task is resumed after the pre-emptive task execution.
 *
 * @param startTime the task start time
 * @param duration the task duration
 * @param preEmptStartTime the pre-emptive task start time
 * @param preEmptDuration the pre-emptive task duration
 * @param resume is true if the task must be resumed after the pre-emptive task
 * @author yoann.guyot@cetic.be
 */
case class PreEmption(startTime: IntValue, duration: IntValue,
                      preEmptStartTime: Long, preEmptDuration: Long, resume: Boolean)
  extends LazyIntInt2Int(startTime, duration,
    (startTime: Long, duration: Long) => {
      val endTime: Long = startTime + duration
      var newDuration: Long = duration

      /**
       * If the pre-emptive task begins during the execution of the current task,
       * the current task shall be shifted.
       */
      if ((startTime to endTime).contains(preEmptStartTime)) {
        if (resume) {
          newDuration = duration + preEmptDuration
        } else {
          newDuration = (preEmptStartTime - startTime) + preEmptDuration
        }
      }
      newDuration
    }, Domain(duration.min, if (resume) {
      duration.max + preEmptDuration
    } else {
      (preEmptStartTime - startTime.min) + preEmptDuration
    })) {
}

// replace with a so-called test
object TestPreEmption extends App {
  val m = Store()

  val from = CBLSIntVar(m, 0L, fullRange, "from")
  val duration = CBLSIntVar(m, 4L, fullRange, "duration")

  val preEmptionFrom = 2L
  val preEmptionDuration = 3L

  val durationResumed = PreEmption(from, duration,
    preEmptionFrom, preEmptionDuration, true)

  val durationNotResumed = PreEmption(from, duration,
    preEmptionFrom, preEmptionDuration, false)

  m.close()

  // should be 7L
  println(durationResumed.value)
  // should be 5L
  println(durationNotResumed.value)
}
