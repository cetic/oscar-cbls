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

package oscar.cbls.lib.invariant.minmax

import oscar.cbls.core.computation.{IncredibleBulk, Invariant, Store}
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.SetVariable

/** The companion object of [[Max]] class. */
object Max {

  /** Creates a [[Max]] invariant.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this Invariant is linked.
    * @param input
    *   An [[Array]] of [[IntVariable]].
    * @param cond
    *   A [[SetVariable]] containing the indices of the input variables to be listened to calculate
    *   the maximum.
    * @param output
    *   The output [[IntVariable]] containing Max((input(i) | i in cond).
    * @param bulkIdentifier
    *   A [[IncredibleBulk]] is used when several [[Invariant]] listen to vars. Warning:
    *   [[IncredibleBulk]] are distinguished only by their identifier.Be sure to use the same one if
    *   you're referencing the same variables.
    * @param name
    *   The name (optional) of your Invariant.
    */
  def apply(
    model: Store,
    input: Array[IntVariable],
    cond: SetVariable,
    output: IntVariable,
    bulkIdentifier: String,
    name: Option[String] = None
  ): Max = {
    new Max(model, input, cond, output, bulkIdentifier, name)
  }

  /** Create a [[Max]] invariant that initially listen all the input variables.
   *
   * @param model
   *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
   * @param input
   *   An [[Array]] of [[IntVariable]].
   * @param output
   *   The output [[IntVariable]].
   * @param bulkIdentifier
   *   A [[IncredibleBulk]] is used when several [[Invariant]] listen to vars. Warning:
   *   [[IncredibleBulk]] are distinguished only by their identifier.Be sure to use the same one if
   *   you're referencing the same variables.
   * @param name
   *   The name (optional) of your Invariant.
   */
  def maxOnAllVariables(
                         model: Store,
                         input: Array[IntVariable],
                         output: IntVariable,
                         bulkIdentifier: String,
                         name: Option[String] = None
                       ): Max = {
    val cond: SetVariable = SetVariable(model, (for (i: Int <- input.indices) yield i).toSet)
    Max(model, input, cond, output, bulkIdentifier, name)
  }
}

/** [[Invariant]] that maintains Max((input(i) | i in cond). Update is in O(log(n))
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   An [[Array]] of [[IntVariable]].
  * @param cond
  *   A [[SetVariable]] containing the indices of the input variables to be listened to calculate
  *   the maximum.
  * @param output
  *   The output [[IntVariable]] containing Max((input(i) | i in cond).
  * @param bulkIdentifier
  *   A [[IncredibleBulk]] is used when several [[Invariant]] listen to vars. Warning:
  *   [[IncredibleBulk]] are distinguished only by their identifier.Be sure to use the same one if
  *   you're referencing the same variables.
  * @param name
  *   The name (optional) of your Invariant.
  */
class Max(
  model: Store,
  input: Array[IntVariable],
  cond: SetVariable,
  output: IntVariable,
  bulkIdentifier: String,
  name: Option[String] = None
) extends Extremum(model, input, cond, output, Long.MinValue, bulkIdentifier, name) {

  override def ord(v: IntVariable): Long =
    -v.value() // The biggest value must have the smallest priority in the heap
}
