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
import oscar.cbls.core.computation.{IncredibleBulk, Invariant, KeyForRemoval, Store}
import oscar.cbls.core.computation.integer.{IntNotificationTarget, IntVariable}
import oscar.cbls.core.computation.set.{SetNotificationTarget, SetVariable}

/** Abstract [[Invariant]] that maintains Extremum(input(i) | i in cond). Exact ordering is
  * specified by implementing abstract method of the class. Update is in O(log(n))
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   An [[Array]] of [[IntVariable]].
  * @param cond
  *   A [[SetVariable]] containing the indices of the input variables to be listened to calculate
  *   the extremum.
  * @param output
  *   The output [[IntVariable]].
  * @param default
  *   The default value of the extremum.
  * @param bulkIdentifier
  *   A [[IncredibleBulk]] is used when several [[Invariant]] listen to vars. Warning:
  *   [[IncredibleBulk]] are distinguished only by their identifier.Be sure to use the same one if
  *   you're referencing the same variables.
  * @param name
  *   The name (optional) of your Invariant.
  */
abstract class Extremum(
  model: Store,
  input: Array[IntVariable],
  cond: SetVariable,
  output: IntVariable,
  default: Long,
  bulkIdentifier: Option[String] = None,
  name: Option[String] = None
) extends Invariant(model, name)
    with IntNotificationTarget
    with SetNotificationTarget {

  private[this] val keysForRemoval: Array[KeyForRemoval[_]] = new Array(input.length)

  // Use to stock the indices of the listened variables. All operation are in O(log(n))
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

  for (i <- cond.value()) {
    h.insert(i)
    keysForRemoval(i) = input(i).registerDynamicallyListeningElement(this, i)
  }

  cond.registerStaticallyAndDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)

  h.getFirst match {
    case None    => output := default
    case Some(i) => output := input(i).value()
  }

  def ord(v: IntVariable): Long

  @inline
  override def notifyIntChanges(
    intVariable: IntVariable,
    index: Int,
    oldVal: Long,
    newVal: Long
  ): Unit = {
    h.notifyChange(index) // Update the heap
    output := input(h.getFirst.get).value() // The extremum has possibly change. We update it
  }

  @inline
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
    if (cond.value().nonEmpty) {
      // We get {input(i) | i in cond}
      var listenedVariables: Array[IntVariable] = new Array[IntVariable](0)
      for (i: Int <- cond.value()) listenedVariables = listenedVariables :+ input(i)

      require(
        output.value() == listenedVariables.minBy(ord).value(),
        s"checkInternals fails in invariant ${name()}. " +
          s"output != min/max of observed variables. " +
          s"output: $output - observed variables: ${listenedVariables.mkString("", ", ", "")}"
      )
    } else {
      require(h.isEmpty)
      require(
        output.value() == default,
        s"checkInternals fails in invariant ${name()}. " +
          s"A problem occurs while observing an empty set of variables." +
          s"output: $output"
      )
    }
  }

  @inline
  private[this] def notifyInsertOn(set: SetVariable, index: Int): Unit = {
    assert(set == cond)

    keysForRemoval(index) = input(index).registerDynamicallyListeningElement(this, index)

    h.insert(index)
    output := input(h.getFirst.get).value()
  }

  @inline
  private[this] def notifyDeleteOn(set: SetVariable, index: Int): Unit = {
    assert(set == cond)

    keysForRemoval(index).delete()
    keysForRemoval(index) = null

    h.removeElement(index)
    h.getFirst match {
      case None    => output := default // We removed the last element in the heap
      case Some(i) => output := input(i).value()
    }
  }
}
