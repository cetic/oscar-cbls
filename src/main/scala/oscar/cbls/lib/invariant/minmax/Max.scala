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

//TODO: manage condition on considered variables when SetVariable will be available
/** The invariant that maintains Max(vars(i) | i in cond
 *
 * @param model
 *    The [[oscar.cbls.core.propagation.PropagationStructure]] to which this Invariant is linked
 * @param vars
 *    An [[IndexedSeq]] of [[IntVariable]]
 * @param output
 *    The output [[IntVariable]]
 * @param bulkIdentifier
 * A [[IncredibleBulk]] can be use when several [[Invariant]] listen to vars.
 *    Warning: [[IncredibleBulk]] are distinguished only by their identifier.Be sure to use the same one if you're
 *    referencing the same variables.
 * @param name
 *   The name (optional) of your Invariant
 */
class Max (model: Store,
           vars: IndexedSeq[IntVariable],
           output: IntVariable,
           bulkIdentifier: String,
           name: Option[String] = None) extends Extremum(model, vars, output, Long.MinValue, bulkIdentifier, name){


  override def ord(v: IntVariable): Long = -v.value() //The biggest value must the smallest priority in the heap

  override def checkInternals(): Unit = {
    for (v <- vars){
      assert(output.value() >= v.value(), s"Value is bigger than Max. Max: ${output.value()} - Value: ${v.value()}")
    }

  }
}

object Max{
  def apply(model: Store,
            vars: IndexedSeq[IntVariable],
            output: IntVariable,
            bulkIdentifier: String): Max = {
    new Max(model, vars, output, bulkIdentifier)
  }

  def apply(model: Store,
            vars: IndexedSeq[IntVariable],
            output: IntVariable,bulkIdentifier: String,
            name: String): Max  = {
    new Max(model, vars, output, bulkIdentifier, Some(name))
  }
}
