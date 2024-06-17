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

import oscar.cbls.core.computation.{IncredibleBulk, Invariant, KeyForRemoval, Store}
import oscar.cbls.core.computation.integer.{IntNotificationTarget, IntVariable}

/** Companion object of the [[Element]] class. */
object Element {

  /** Creates an [[Element]] invariant.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   An [[Array]] of [[IntVariable]].
    * @param index
    *   An [[IntVariable]] accessing one of the input values.
    * @param output
    *   The [[IntVariable]] which contains input(index).
    * @param bulkIdentifier
    *   A [[IncredibleBulk]] is used when several [[Invariant]] listen to vars. Warning:
    *   [[IncredibleBulk]] are distinguished only by their identifier. Be sure to use the same one
    *   if you're referencing the same variables.
    * @param name
    *   The name (optional) of your Invariant.
    */
  def apply(
    model: Store,
    input: Array[IntVariable],
    index: IntVariable,
    output: IntVariable,
    bulkIdentifier: Option[String] = None,
    name: Option[String] = None
  ): Element = {
    new Element(model, input, index, output, bulkIdentifier, name)
  }
}

/** [[Invariant]] that maintains input(index) where input is an array of [[IntVariable]]. Update is
  * in O(1).
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   An [[Array]] of [[IntVariable]].
  * @param index
  *   An [[IntVariable]] accessing one of the input values.
  * @param output
  *   The [[IntVariable]] which contains input(index).
  * @param bulkIdentifier
  *   A [[IncredibleBulk]] is used when several [[Invariant]] listen to vars. Warning:
  *   [[IncredibleBulk]] are distinguished only by their identifier. Be sure to use the same one if
  *   you're referencing the same variables.
  * @param name
  *   The name (optional) of your Invariant.
  */
class Element(
  model: Store,
  input: Array[IntVariable],
  index: IntVariable,
  output: IntVariable,
  bulkIdentifier: Option[String] = None,
  name: Option[String] = None
) extends Invariant(model, name)
    with IntNotificationTarget {

  private[this] var keyForCurrentVar: KeyForRemoval[_] = input(index.value().toInt)
    .registerDynamicallyListeningElement(this)

  bulkIdentifier match {
    case None =>
      // No bulk is used
      for (vars <- input) this.registerStaticallyListenedElement(vars)
    case Some(bulkId) =>
      // Register static dependency via a bulk
      this.addIncredibleBulk(IncredibleBulk.bulkRegistering(input, bulkId, model))
  }
  index.registerStaticallyAndDynamicallyListeningElement(this, 0)
  output.setDefiningInvariant(this)

  output := input(index.value().toInt).value()

  override def notifyIntChanges(
    intVariable: IntVariable,
    contextualVarIndex: Int,
    oldVal: Long,
    newVal: Long
  ): Unit = {
    if (contextualVarIndex == 0) {
      // The index is modified. We update the dependencies.
      assert(index == intVariable)
      keyForCurrentVar.delete()
      keyForCurrentVar = input(newVal.toInt).registerDynamicallyListeningElement(this)
      output := input(newVal.toInt).value()
    } else {
      // The listened variable is modified.
      output := newVal
    }
  }

  override def checkInternals(): Unit = {
    require(
      output.pendingValue == input(index.value().toInt).value(),
      s"checkInternals fails in invariant ${name()}. " +
        s"output != input(index). " +
        s"output: ${output.pendingValue} - index: $index - input: ${input.mkString("", ", ", "")}"
    )
  }
}
