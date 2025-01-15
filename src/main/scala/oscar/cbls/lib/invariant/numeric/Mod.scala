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

/** Companion object of [[Mod]]. */
object Mod {

  /** Creates a Mod invariant, which maintains the remainder of a division (the modulo) between two
    * [[oscar.cbls.core.computation.integer.IntVariable]]. Warning: the scala operator `%` is not
    * exactly a modulo operator. For example, `-1 % 3 == -1`, and `2 % 3 == 2`, but `-1 == 2 mod 3`.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param a
    *   The first parameter of the function.
    * @param b
    *   The second parameter of the function.
    * @param output
    *   The IntVariable evaluating to `a mod b`.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(
    model: Store,
    a: IntVariable,
    b: IntVariable,
    output: IntVariable,
    name: Option[String] = None
  ): Mod = {
    new Mod(model, a, b, output, name)
  }
}

/** Invariant which maintains the remainder of a division (the
  * modulo) between two [[oscar.cbls.core.computation.integer.IntVariable]]. Warning: the scala
  * operator `%` is not exactly a modulo operator. For example, `-1 % 3 == -1`, and `2 % 3 == 2`,
  * but `-1 mod 3 == 2`.
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param a
  *   The first parameter of the function.
  * @param b
  *   The second parameter of the function.
  * @param output
  *   The IntVariable evaluating to `a mod b`.
  * @param name
  *   The (optional) name of the Invariant.
  */
class Mod(
  model: Store,
  a: IntVariable,
  b: IntVariable,
  output: IntVariable,
  name: Option[String] = None
) extends IntInt2Int(model, a, b, output, (x: Long, y: Long) => ((x % y) + y) % y, name) {}
