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

import oscar.cbls.algo.heap.BinaryHeapWithMoveIntItem
import oscar.cbls.core.computation.integer.{IntNotificationTarget, IntVariable}
import oscar.cbls.core.computation.set.{SetNotificationTarget, SetVariable}
import oscar.cbls.core.computation.{IncredibleBulk, Invariant, KeyForRemoval, Store}

/** Abstract Invariant which maintains `Extremum{input(i) | i in`
  * `listenedVariablesIndices}`. Exact ordering is specified by implementing abstract method of the
  * class. Update is in O(log(n)).
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   An array of variable on which to compute the extremum.
  * @param listenedVariablesIndices
  *   A SetVariable containing the indices of the input variables to be listened to calculate the
  *   extremum.
  * @param output
  *   The output IntVariable evaluating to `Extremum(input(i) | i in listenedVariablesIndices)`.
  * @param default
  *   The default value of the extremum.
  * @param bulkIdentifier
  *   A [[oscar.cbls.core.computation.IncredibleBulk]] is used when several
  *   Invariant listen to vars. Warning:
  *   [[oscar.cbls.core.computation.IncredibleBulk]] are distinguished only by their identifier. Be
  *   sure to use the same one if you're referencing the same variables.
  * @param name
  *   The (optional) name of the Invariant.
  */
abstract class Extremum(
  model: Store,
  input: Array[IntVariable],
  listenedVariablesIndices: SetVariable,
  output: IntVariable,
  default: Long,
  bulkIdentifier: Option[String] = None,
  name: Option[String] = None
) extends Invariant(model, name)
    with IntNotificationTarget
    with SetNotificationTarget {

  private[this] val keysForRemoval: Array[KeyForRemoval[_]] = new Array(input.length)

  // Use to stock the indices of the listened variables. All operations are in O(log(n))
  private[this] val h: BinaryHeapWithMoveIntItem =
    BinaryHeapWithMoveIntItem((i: Int) => ord(input(i)), input.length, input.length)

  bulkIdentifier match {
    case None =>
      // No bulk is used
      for (vars <- input) this.registerStaticallyListenedElement(vars)
    case Some(bulkId) =>
      // Register static dependency via a bulk
      this.addIncredibleBulk(IncredibleBulk.bulkRegistering(input, bulkId, model))
  }

  for (i <- listenedVariablesIndices.value()) {
    h.insert(i)
    keysForRemoval(i) = input(i).registerDynamicallyListeningElement(this, i)
  }

  listenedVariablesIndices.registerStaticallyAndDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)

  h.getFirst match {
    case None    => output := default
    case Some(i) => output := input(i).value()
  }

  protected def ord(v: IntVariable): Long

  override def notifyIntChanges(
    intVariable: IntVariable,
    index: Int,
    oldVal: Long,
    newVal: Long
  ): Unit = {
    h.notifyChange(index) // Update the heap
    output := input(h.getFirst.get).value() // The extremum has possibly change. We update it
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
    if (listenedVariablesIndices.value().nonEmpty) {
      // We get {input(i) | i in listenedVariablesIndices}
      val listenedVariables: Set[IntVariable] =
        listenedVariablesIndices.value().map(i => input(i))

      require(
        output.pendingValue == listenedVariables.minBy(ord).value(),
        s"checkInternals fails in invariant ${name()}. " +
          s"output != min/max of observed variables. " +
          s"output: ${output.pendingValue} - observed variables: ${listenedVariables.mkString("", ", ", "")}"
      )
    } else {
      require(h.isEmpty)
      require(
        output.pendingValue == default,
        s"checkInternals fails in invariant ${name()}. " +
          s"A problem occurs while observing an empty set of variables." +
          s"output: ${output.pendingValue}"
      )
    }
  }

  // updates the extremum when an additional IntVariable must be used
  private[this] def notifyInsertOn(set: SetVariable, index: Int): Unit = {
    assert(set == listenedVariablesIndices)

    keysForRemoval(index) = input(index).registerDynamicallyListeningElement(this, index)
    h.insert(index)
    output := input(h.getFirst.get).value()
  }

  // updates the extremum when an IntVariable is not used anymore
  private[this] def notifyDeleteOn(set: SetVariable, index: Int): Unit = {
    assert(set == listenedVariablesIndices)

    keysForRemoval(index).delete()
    keysForRemoval(index) = null

    h.removeElement(index)
    h.getFirst match {
      case None    => output := default // We removed the last element in the heap
      case Some(i) => output := input(i).value()
    }
  }
}
