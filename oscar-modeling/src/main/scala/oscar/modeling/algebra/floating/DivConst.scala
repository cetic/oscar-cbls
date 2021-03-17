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

package oscar.modeling.algebra.floating

import oscar.modeling.algebra.Expression
import scala.math.Ordering.Double.TotalOrdering

/**
 * Expression left/right
 *
 * @param left The numerator
 * @param right The denoinator
 */
case class DivConst(left: FloatExpression, right: Double) extends FloatExpression {
  override def evaluate(): Double = left.evaluate() / right
  override def min: Double = Array(left.min/right, left.max/right).min
  override def max: Double = Array(left.min/right, left.max/right).max

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[FloatExpression] = Array(left)

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (Expression) => Expression): FloatExpression = DivConst(func(left).asInstanceOf[FloatExpression], right)

  /**
    * Returns true if continuous (not an integer variable)
    */
  override def continuous: Boolean = left.continuous

  /**
    * Returns true if the expression is linear
    */
  override def linear: Boolean = left.linear

  /**
    * True if the variable is bound
    */
  override def isBound: Boolean = subexpressions().forall(_.isBound)
}
