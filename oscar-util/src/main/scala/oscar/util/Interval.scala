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
package oscar.util

/** Represent the intervals used to define the domains of the functions.
  *
  * @constructor Create a new interval represented by its lower and upper bounds
  * @param minimum The lower bound of the interval
  * @param maximum the upper bound of the interval
  * @example To create the interval [-42.0; 42.0],
  *          one could write val a = Interval(-42.0, 42.0)
  * @throws IllegalArgumentException Launch an IllegalArgumentException if the parameter minimum
  *            is higher than the parameter maximum */
class Interval(minimum: Double, maximum: Double) {
  if (minimum > maximum)
    throw new IllegalArgumentException("The minimum of the interval should be lower or equal to the maximum.")

  /** The lower bound of the interval */
  val min = minimum
  /** The upper bound of the interval */
  val max = maximum

  /** Returns the width of the interval.
    * 
    * The width of the interval is defined as the upper bound minus the lower bound.
    * @return A Double that is the width of the interval
    * @example The interval [-42.0; 42.0] has a size = 42.0 - (-42.0) = 84.0 */
  def size: Double = {
    math.abs(max - min)
  }

  /** Returns the closest bound of the interval with respect to x.
    * 
    * The closest bound is chosen by choosing the highest absolute value of
    * the difference between the parameter and a bound.
    *
    * @param x The value we search the closest bound of the interval to
    * @return A Double that is the closest bound of the interval from the parameter
    * @example If we have the following interval
    *          val interval = Interval(-42.0, 42.0)
    *          the instruction interval.getClosestBound(-5.0) will return -42.0 */
  def getClosestBound(x: Double): Double = {
    if (math.abs(x - min) <= math.abs(x - max))
      min
    else
      max
  }

  //Returns true if x is in the interval.
  /** Returns true if the parameter is within the interval, false otherwise.
    * 
    * @param x The value we want to know if it is within the interval
    * @return A Boolean that is true if the parameter lies within the interval, false otherwise
    * @example If we have the following interval
    *          val interval = Interval(-42.0, 42.0)
    *          the istruction interval.isInInterval(25.0) will return true
    *          while the instruction interval.isInInterval(68.0) will return false */
  def isInInterval(x: Double): Boolean = {
    min <= x && x <= max
  }
}

/** Factory for Interval instances. */
object Interval {
  /** Create an interval according to the specified bounds
    *
    * @param min The lower bound of the interval
    * @param max The upper bound of the interval
    * @return An Interval with the specified bounds */
  def apply(min: Double, max: Double) = new Interval(min, max)
}
