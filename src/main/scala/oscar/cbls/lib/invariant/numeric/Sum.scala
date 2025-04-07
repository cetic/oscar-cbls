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

import oscar.cbls.core.computation.integer.{IntNotificationTarget, IntVariable}
import oscar.cbls.core.computation.set.{SetNotificationTarget, SetVariable}
import oscar.cbls.core.computation.{IncredibleBulk, Invariant, KeyForRemoval, Store}

/** Companion object of the [[Sum]] class. */
object Sum {

  /** Creates a Sum invariant, which maintains `Sum(input(i) | i in listenedVariablesIndices)`,
    * where `input` is an array of IntVariables.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   The array of IntVariables to sum.
    * @param listenedVariablesIndices
    *   A SetVariable containing the indices of the input variables to sum.
    * @param output
    *   The output variable evaluating to `Sum(input(i) | i in listenedVariablesIndices)`.
    * @param name
    *   The (optional) name of the Invariant.
    * @param bulkUsed
    *   Whether the input variables must be bulked (see
    *   [[oscar.cbls.core.computation.IncredibleBulk]]).
    */
  def apply(
    model: Store,
    input: Array[IntVariable],
    listenedVariablesIndices: SetVariable,
    output: IntVariable,
    name: Option[String] = None,
    bulkUsed: Boolean = false
  ): Sum = {
    new Sum(model, input, listenedVariablesIndices, output, name, bulkUsed)
  }
}

/** Invariant which maintains `Sum(input(i) | i in` `listenedVariablesIndices)`. Update is in O(1).
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   The array of IntVariables to sum.
  * @param listenedVariablesIndices
  *   A SetVariable containing the indices of the input variables to sum.
  * @param output
  *   The output variable containing `Sum(input(i) | i in listenedVariablesIndices)`.
  * @param name
  *   The (optional) name of the Invariant.
  * @param bulkUsed
  *   Whether the input variables must be bulked (see
  *   [[oscar.cbls.core.computation.IncredibleBulk]]).
  */
class Sum(
  model: Store,
  input: Array[IntVariable],
  listenedVariablesIndices: SetVariable,
  output: IntVariable,
  name: Option[String],
  bulkUsed: Boolean
) extends Invariant(model, name)
    with IntNotificationTarget
    with SetNotificationTarget {

  private[this] val keysForRemoval: Array[KeyForRemoval[_]] = new Array(input.length)

  if (bulkUsed) {
    // Registers static dependency via a bulk
    this.addIncredibleBulk(IncredibleBulk.bulkRegistering(input, model))
  } else {
    // No bulk is used
    for (vars <- input) this.registerStaticallyListenedElement(vars)
  }

  listenedVariablesIndices.registerStaticallyAndDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)

  output := 0
  for (i <- listenedVariablesIndices.value()) {
    keysForRemoval(i) = input(i).registerDynamicallyListeningElement(this, i)
    output :+= input(i).value()
  }

  override def notifyIntChanges(
    intVariable: IntVariable,
    contextualVarIndex: Int,
    oldVal: Long,
    newVal: Long
  ): Unit = {

    output :+= (newVal - oldVal)
  }
  override def notifySetChanges(
    setVariable: SetVariable,
    index: Int,
    addedElems: Iterable[Int],
    removedElems: Iterable[Int],
    oldValue: Set[Int],
    newValue: Set[Int]
  ): Unit = {
    for (added   <- addedElems) notifyInsertOn(setVariable, added)
    for (removed <- removedElems) notifyDeleteOn(setVariable, removed)
  }

  override def checkInternals(): Unit = {
    val listenedVariables: Set[IntVariable] =
      listenedVariablesIndices.value().map(i => input(i))
    val expectedSum = listenedVariables.foldLeft(0L)((acc: Long, x: IntVariable) => acc + x.value())

    require(
      output.pendingValue == expectedSum,
      s"checkInternals fails in invariant ${name()}. " +
        s"output != the sum of the listened variables. " +
        s"output: ${output.pendingValue} - expected sum: $expectedSum " +
        s"- listened variables: ${listenedVariables.mkString("", ", ", "")}"
    )
  }

  // updates sum when an additional IntVariable must be used
  private[this] def notifyInsertOn(set: SetVariable, index: Int): Unit = {
    assert(set == listenedVariablesIndices, "Input SetVariable is incorrect")

    keysForRemoval(index) = input(index).registerDynamicallyListeningElement(this, index)

    output :+= input(index).value()
  }

  // updates sum when an IntVariable is not used anymore
  private[this] def notifyDeleteOn(set: SetVariable, index: Int): Unit = {
    assert(set == listenedVariablesIndices, "Input SetVariable is incorrect")

    keysForRemoval(index).delete()

    output :-= input(index).value()
  }

}
