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
package oscar.cbls.lib.invariant.numeric

import oscar.cbls.core.computation.{IntValue, SetValue}

/**
 * modeling interface presenting the numeric invariants
 * @author renaud.delandtsheer@cetic.be
 */
trait NumericInvariants{
  /** sum(vars)
    * @param vars is an iterable of IntVars
    * */
  def sum(vars:Iterable[IntValue]) = Sum(vars:Iterable[IntValue])

  /** prod(vars)
    * @param vars is a set of IntVars
    * */
  def prod(vars:Iterable[IntValue]) = Prod(vars:Iterable[IntValue])

  /** left - right
    * where left, right, and output are IntVar*/
  def minus(left:IntValue, right:IntValue) = Minus(left:IntValue, right:IntValue)

  /** left + right
    * where left, right, and output are IntVar*/
  def sum2(left:IntValue, right:IntValue) = Sum2(left:IntValue, right:IntValue)

  /** left * right
    * where left, right, and output are IntVar*/
  def prod2(left:IntValue, right:IntValue) = Prod2(left:IntValue, right:IntValue)

  /**left / right
    * where left, right, and output are IntVar
    * do not set right to zero, as usual... */
  def div(left:IntValue, right:IntValue) = Div(left:IntValue, right:IntValue)

  /**left / right
    * where left, right, and output are IntVar
    * do not set right to zero, as usual... */
  def mod(left:IntValue, right:IntValue) = Mod(left:IntValue, right:IntValue)

  /**abs(v) (absolute value)
    * where output and v are IntVar*/
  def abs(v:IntValue) = Abs(v:IntValue)

  def pow(a:IntValue, b:IntValue) = Pow(a,b)

  /**
   * This invariant implements the identity function within the min-max range.
   * values lower tham min result to min
   * values higher tham max result to max
   * @author renaud.delandtsheer@cetic.be
   * @param x
   * @param min
   * @param max
   */
  def  bound(x: IntValue, min:Long, max:Long) = Bound(x, min, max)

  /**Maintains output to the smallest value such that
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
    * for instance, suppose you represent days starting from zero, and zero is a monday,
    * and you want to round up to the next open day (sa and su are closed day, the correct declaration is:
    * RoundUpModulo(from,duration,7L,2L,5L)
    *
    * @param from the starting date of the task. it can start later.
    * @param duration the duration of the task.
    * @param period the period of the forbidden-allowed pattern
    * @param zone the size of the forbidden zone. it starts at the beginning of the period
    * @param shift the first period starts later than zero. it starts at shift. the duration before its start is allowed.
    */

  def roundUpModulo(from: IntValue, duration: IntValue, period: Long, zone: Long, shift: Long) = RoundUpModulo(from: IntValue, duration: IntValue, period: Long, zone: Long, shift: Long)

  /**Maintains output to the smallest value such that
    * output >= from
    * the interval [output ; output + length] does not overlap with the intervals given in FobiddenZones
    *
    * @param from
    * @param duration
    * @param ForbiddenZones
    */
  def roundUpCustom(from: IntValue, duration: IntValue, ForbiddenZones: List[(Long, Long)]) = RoundUpCustom(from: IntValue, duration: IntValue, ForbiddenZones: List[(Long, Long)])

  /**
   * This invariant implements a step function. Values higher than pivot are mapped to ifval
   * values lower or equal to pivot are mapped to elseval
   * This invariant was suggested by Jean-Noël Monette
   *
   * @param x the IntVar parameter of the invariant
   * @param pivot the pivot value
   * @param thenval the value returned when x > pivot
   * @param elseval the value returned when x <= pivot
   */
  def step(x:IntValue,pivot:Long = 0L,thenval:Long = 1L,elseval:Long = 0L) = Step(x:IntValue,pivot:Long,thenval:Long ,elseval:Long)

  /** sum(i in cond) vars(i)
    * This invariant might modify vars array by cloning some variables to ensure that each variable only appears once.
    * @param vars is a set of IntVars
    * @param cond is the condition for selecting variables in the set of summed ones, cannot be null
    */
  def sumElements(vars: Array[IntValue], cond: SetValue) = SumElements(vars: Array[IntValue], cond: SetValue)

  /** prod(i in cond) vars(i)
    * This invariant might modify vars array by cloning some variables to ensure that each variable only appears once.
    * @param vars is a set of IntVars
    * @param cond is the condition for selecting variables in the set of summed ones.
    */
  def prodElements(vars: Array[IntValue], cond: SetValue) = ProdElements(vars: Array[IntValue], cond: SetValue)

}
