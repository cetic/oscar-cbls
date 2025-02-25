// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.lib.invariant.numeric

import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable

import scala.math.{pow, round}

/** Companion object of [[Pow]] class. */
object Pow {

  /** Creates a Pow invariant, which maintains the power of an
    * [[oscar.cbls.core.computation.integer.IntVariable]] by another.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param a
    *   The first parameter of the function.
    * @param b
    *   The second parameter of the function.
    * @param output
    *   The IntVariable evaluating to `a^b`. If `b < 0`, the value is rounded to the closest
    *   integer.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(
    model: Store,
    a: IntVariable,
    b: IntVariable,
    output: IntVariable,
    name: Option[String] = None
  ): Pow = {
    new Pow(model, a, b, output, name)
  }

  /** Static method to obtain an IntVariable that is the result of `a^b`
    *
    * @param a
    *   Base of the exponentiation.
    * @param b
    *   Exponent of the exponentiation.
    */
  def result(a: IntVariable, b: IntVariable, name: Option[String] = None): IntVariable = {
    val output = IntVariable(a.model, round(pow(a.value().toDouble, a.value().toDouble)))
    Pow(a.model, a, b, output, name)
    output
  }
}

/** Invariant which maintains the power of an [[oscar.cbls.core.computation.integer.IntVariable]] by
  * another.
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param a
  *   The first parameter of the function.
  * @param b
  *   The second parameter of the function.
  * @param output
  *   The IntVariable evaluating to `a^b`. If `b < 0`, the value is rounded to the closest integer.
  * @param name
  *   The (optional) name of the Invariant.
  */
class Pow(
  model: Store,
  a: IntVariable,
  b: IntVariable,
  output: IntVariable,
  name: Option[String] = None
) extends IntInt2Int(
      model,
      a,
      b,
      output,
      (x: Long, y: Long) => round(pow(x.toDouble, y.toDouble)),
      name
    ) {}
