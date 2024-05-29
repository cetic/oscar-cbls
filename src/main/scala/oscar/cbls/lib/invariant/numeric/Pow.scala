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
object Pow{

  /** Create a [[Pow]] invariant.
   *
   * @param model
   *  The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
   * @param a
   *  The first parameter of the function.
   * @param b
   *  The second parameter of the function.
   * @param output
   *  The [[IntVariable]] which contains a&#94;b.
   *  If b < 0, the value is rounded to the closest integer.
   * @param name
   *   The name (optional) of your Invariant.
   */
  def apply(model: Store,
            a: IntVariable,
            b: IntVariable,
            output: IntVariable,
            name: Option[String] = None): Pow = {
    new Pow(model, a, b, output, name)
  }
}

/** [[oscar.cbls.core.computation.Invariant]] which maintains the power of an [[IntVariable]] by another.
 *
 * @param model
 *  The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
 * @param a
 *  The first parameter of the function.
 * @param b
 *  The second parameter of the function.
 * @param output
 *  The [[IntVariable]] which contains a&#94;b.
 *  If b < 0, the value is rounded to the closest integer.
 * @param name
 *   The name (optional) of your Invariant.
 */
class Pow(model: Store,
          a: IntVariable,
          b: IntVariable,
          output: IntVariable,
          name: Option[String] = None)
extends IntInt2Int(model, a, b, output, (x: Long, y: Long) => round(pow(x, y)), name) {

  override def checkInternals(): Unit = {
    require(output.value() == round(pow(a.value(), b.value())),
      s"output != a^b. output: $output - a: $a - b: $b")
  }
}
