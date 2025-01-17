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

package oscar.cbls.lib.invariant.set

import oscar.cbls.core.computation.{Invariant, Store}
import oscar.cbls.core.computation.integer.{IntNotificationTarget, IntVariable}
import oscar.cbls.core.computation.set.{SetNotificationTarget, SetVariable}

/** The companion object of the [[BelongsTo]] class. */
object BelongsTo {

  /** Creates a BelongTo invariant, which contains if `inputValue in inputSet`.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param inputValue
    *   The variable we want to test if it belongs to `inputSet`.
    * @param inputSet
    *   The SetVariable which can contain `inputValue` or not.
    * @param output
    *   The IntVariable evaluating to:
    *   - 1 if `inputValue in inputSet`,
    *   - 0 if not.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(
    model: Store,
    inputValue: IntVariable,
    inputSet: SetVariable,
    output: IntVariable,
    name: Option[String] = None
  ): BelongsTo = new BelongsTo(model, inputValue, inputSet, output, name)
}

/** Invariant which maintains if `inputValue in inputSet`. Update is O(1).
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param inputValue
  *   The variable we want to test if it belongs to `inputSet`.
  * @param inputSet
  *   The SetVariable which can contain `inputValue` or not.
  * @param output
  *   The IntVariable evaluating to:
  *   - 1 if `inputValue in inputSet`,
  *   - 0 if not.
  * @param name
  *   The (optional) name of the Invariant.
  */
class BelongsTo(
  model: Store,
  inputValue: IntVariable,
  inputSet: SetVariable,
  output: IntVariable,
  name: Option[String]
) extends Invariant(model, name)
    with IntNotificationTarget
    with SetNotificationTarget {

  inputValue.registerStaticallyAndDynamicallyListeningElement(this)
  inputSet.registerStaticallyAndDynamicallyListeningElement(this)

  output.setDefiningInvariant(this)
  output := (if (inputSet.value().contains(inputValue.value().toInt)) 1L else 0L)

  override def notifyIntChanges(
    intVariable: IntVariable,
    contextualVarIndex: Int,
    oldVal: Long,
    newVal: Long
  ): Unit = output := (if (inputSet.value().contains(newVal.toInt)) 1L else 0L)

  override def notifySetChanges(
    setVariable: SetVariable,
    index: Int,
    addedElems: Iterable[Int],
    removedElems: Iterable[Int],
    oldValue: Set[Int],
    newValue: Set[Int]
  ): Unit = {
    if (
      (removedElems.nonEmpty && output.pendingValue == 1L)
      || (addedElems.nonEmpty && output.pendingValue == 0L)
    ) output := (if (inputSet.value().contains(inputValue.value().toInt)) 1L else 0L)
  }

  override def checkInternals(): Unit = {
    require(
      output.pendingValue == 0L || output.pendingValue == 1L,
      s"checkInternals fails in invariant ${name()}. " +
        s"output is neither 0 nor 1. output: ${output.pendingValue}"
    )

    require(
      output.pendingValue == (if (inputSet.value().contains(inputValue.value().toInt)) 1L else 0L),
      s"checkInternals fails in invariant ${name()}. " +
        s"output != inputValue in inputSet. " +
        s"output: ${output.pendingValue} - inputValue: $inputValue - inputSet: $inputSet"
    )
  }
}
