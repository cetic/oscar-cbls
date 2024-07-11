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

package oscar.cbls.lib.invariant.logic

import oscar.cbls.core.computation.integer.{IntNotificationTarget, IntVariable}
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.core.computation.{IncredibleBulk, Invariant, Store}

/** Companion object of the [[Filter]] class. */
object Filter {

  /** Creates a Filter invariant, which maintains `{i in input.indices | predicate (input(i))}`.
    * Update depends of the predicate complexity. If predicate is in O(1), update is in O(1).
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   The elements we want to filter.
    * @param output
    *   A SetVariable evaluating to `{i in input.indices | predicate(input[i])}`
    * @param predicate
    *   The function that selects values such that their index must be included in the output set.
    *   This function cannot depend on any IntVariable, as updates to these IntVariables will not
    *   trigger propagation of this invariant. By default, predicate is "_ > 0".
    * @param bulkIdentifier
    *   An [[oscar.cbls.core.computation.IncredibleBulk]] is used when several
    *   [[oscar.cbls.core.computation.Invariant]] listen to vars. Warning:
    *   [[oscar.cbls.core.computation.IncredibleBulk]] are distinguished only by their identifier.
    *   Be sure to use the same one if you're referencing the same variables.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(
    model: Store,
    input: Array[IntVariable],
    output: SetVariable,
    predicate: Long => Boolean = _ > 0,
    bulkIdentifier: Option[String] = None,
    name: Option[String] = None
  ): Filter = {
    new Filter(model, input, output, predicate, bulkIdentifier, name)
  }
}

/** [[oscar.cbls.core.computation.Invariant]] which maintains `{i in input.indices | predicate`
  * `(input(i))}`. Update depends of the predicate complexity. If predicate is in O(1), update is in
  * O(1).
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   The elements we want to filter
  * @param output
  *   A SetVariable evaluating to `{i in input.indices | predicate(input[i])}`
  * @param predicate
  *   The function that selects values such that their index must be included in the output set.
  *   This function cannot depend on any IntVariable, as updates to these IntVariables will not
  *   trigger propagation of this invariant. By default, predicate is "_ > 0".
  * @param bulkIdentifier
  *   An [[oscar.cbls.core.computation.IncredibleBulk]] is used when several
  *   [[oscar.cbls.core.computation.Invariant]] listen to vars. Warning:
  *   [[oscar.cbls.core.computation.IncredibleBulk]] are distinguished only by their identifier. Be
  *   sure to use the same one if you're referencing the same variables.
  * @param name
  *   The (optional) name of the Invariant.
  */
class Filter(
  model: Store,
  input: Array[IntVariable],
  output: SetVariable,
  predicate: Long => Boolean = _ > 0,
  bulkIdentifier: Option[String] = None,
  name: Option[String] = None
) extends Invariant(model, name)
    with IntNotificationTarget {

  bulkIdentifier match {
    case None =>
      // No bulk is used
      for (vars <- input) this.registerStaticallyListenedElement(vars)
    case Some(bulkId) =>
      // Register static dependency via a bulk
      this.addIncredibleBulk(IncredibleBulk.bulkRegistering(input, bulkId, model))
  }

  output := Set.empty
  for (i <- input.indices) {
    val v: IntVariable = input(i)
    v.registerDynamicallyListeningElement(this, i)
    if (predicate(v.value())) output :+= i
  }

  output.setDefiningInvariant(this)

  override def notifyIntChanges(
    intVariable: IntVariable,
    contextualVarIndex: Int,
    oldVal: Long,
    newVal: Long
  ): Unit = {
    assert(intVariable == input(contextualVarIndex))

    val oldPredicate: Boolean = predicate(oldVal)
    val newPredicate: Boolean = predicate(newVal)
    if (oldPredicate && !newPredicate) output :-= contextualVarIndex
    else if (!oldPredicate && newPredicate) output :+= contextualVarIndex
  }

  override def checkInternals(): Unit = {
    val selectedIndices: Set[Int] = input.indices.filter(i => predicate(input(i).value())).toSet

    require(
      output.pendingValue == selectedIndices,
      s"checkInternals fails in invariant ${name()}. " +
        s"output != {i in input.indices | predicate(input[i]}. " +
        s"output: ${output.pendingValue} - selected  indices: $selectedIndices - input: ${input
            .mkString("", ", ", "")}"
    )
  }

}
