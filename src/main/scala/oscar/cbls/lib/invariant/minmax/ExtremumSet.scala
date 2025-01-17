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

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.{SetNotificationTarget, SetVariable}
import oscar.cbls.core.computation.{Invariant, Store}

/** An abstract Invariant which maintains `Extremum(input)`.
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   The set of integer on which to compute the extremum.
  * @param output
  *   An IntVariable containing the extremum of the input set.
  * @param default
  *   The default value of the extremum.
  * @param name
  *   The (optional) name of the Invariant.
  */
abstract class ExtremumSet(
  model: Store,
  input: SetVariable,
  output: IntVariable,
  default: Long,
  name: Option[String]
) extends Invariant(model, name)
    with SetNotificationTarget {

  private[this] var wasEmpty = input.value().isEmpty

  input.registerStaticallyAndDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)

  /** Checks if `a` is better than `b` */
  protected def better(a: Long, b: Long): Boolean

  performPropagation()

  override def notifySetChanges(
    setVariable: SetVariable,
    index: Int,
    addedElems: Iterable[Int],
    removedElems: Iterable[Int],
    oldValue: Set[Int],
    newValue: Set[Int]
  ): Unit = {
    for (added   <- addedElems) notifyInsertOn(added)
    for (removed <- removedElems) notifyDeleteOn(removed, newValue)
  }

  override def checkInternals(): Unit = {
    if (input.value().nonEmpty) {
      require(
        output.pendingValue == input
          .value()
          .foldLeft(default)((acc, v) =>
            if (better(v, acc)) v
            else acc
          ),
        s"checkInternals fails in invariant ${name()}. " +
          s"output != min/max of input SetVariable. " +
          s"output: ${output.pendingValue} - input: $input"
      )
    } else {
      require(
        output.pendingValue == default,
        s"checkInternals fails in invariant ${name()}. " +
          s"A problem occurs while maintaining the min/max of an empty set. " +
          s"output: ${output.pendingValue}"
      )
    }
  }

  // updates the extremum when an additional integer must be used
  private[this] def notifyInsertOn(value: Int): Unit = {
    if (wasEmpty || (!this.isScheduled && better(value, output.pendingValue))) output := value
    wasEmpty = false
  }

  // updates the extremum when an integer is removed from the input
  private[this] def notifyDeleteOn(value: Int, newValue: Set[Int]): Unit = {
    if (newValue.isEmpty) {
      wasEmpty = true
      output := default
    } else if (!this.isScheduled && value == output.pendingValue) {
      scheduleForPropagation()
    }

  }
}
