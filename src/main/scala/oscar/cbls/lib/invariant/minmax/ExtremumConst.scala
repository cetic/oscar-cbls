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
import oscar.cbls.core.computation.integer.{IntConstant, IntVariable}
import oscar.cbls.core.computation.set.{SetNotificationTarget, SetVariable}
import oscar.cbls.core.computation.{Invariant, Store}

import scala.collection.mutable

/** Abstract [[oscar.cbls.core.computation.Invariant]] which maintains Extremum{input(i) | i in
  * listenedVariablesIndices}. Exact ordering is specified by implementing abstract method of the
  * class. This invariant is lazy and maintains a todo list of postponed updates. Update is in O
  * (log(n)) in worst case. If the update does not impact the output, it is postponed in O(1).
  * Otherwise, it is performed in O(log(n)). When a removed index is considered and does not impact
  * the extremum, it goes in the backlog as well, to be removed later. It is faster for neighborhood
  * exploration with moves and backtracks.
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   The constants on which to compute the extremum.
  * @param listenedValuesIndices
  *   A SetVariable containing the indices of the input variables to be observed to calculate the
  *   extremum.
  * @param output
  *   The output IntVariable evaluating to `Extremum(input(i) | i in listenedVariablesIndices)`.
  * @param default
  *   The default value of the extremum.
  * @param maxBacklog
  *   The maximum number of postponed updates that doesn't affect the extremum.
  * @param name
  *   The (optional) name of the Invariant.
  */
abstract class ExtremumConst(
  model: Store,
  input: Array[IntConstant],
  listenedValuesIndices: SetVariable,
  output: IntVariable,
  default: Long,
  maxBacklog: Int,
  name: Option[String] = None
) extends Invariant(model, name)
    with SetNotificationTarget {

  // Use to stock the indices of the considered variables. All operations are in O(log(n))
  private[this] val h: BinaryHeapWithMoveIntItem =
    BinaryHeapWithMoveIntItem((i: Int) => ord(input(i)), input.length, input.length)

  // Used to postpone not impacting updates
  private[this] var backlog: mutable.Queue[Int] = mutable.Queue()
  private[this] var backlogSize: Int            = 0
  // Tells if the postponed update have to be performed
  private[this] val isBacklogged: Array[Boolean] = Array.fill(input.length)(false)
  // Tells which value are used to compute the extremum
  private[this] val consideredValue: Array[Boolean] = Array.fill(input.length)(false)

  for (i <- listenedValuesIndices.value()) {
    h.insert(i)
    consideredValue(i) = true // A value is considered if and only if it is in h
  }

  listenedValuesIndices.registerStaticallyAndDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)

  updateFromHeap()

  protected def ord(v: IntVariable): Long

  protected def notImpactingExtremum(newValue: IntConstant): Boolean

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
    if (listenedValuesIndices.value().nonEmpty) {
      // We get {input(i) | i in listenedVariablesIndices}
      val listenedValues: Set[IntConstant] = listenedValuesIndices.value().map(i => input(i))
      require(
        output.pendingValue == listenedValues.minBy(ord).value(),
        s"checkInternals fails in invariant ${name()}. " +
          s"output != min/max of observed variables. " +
          s"output: ${output.pendingValue} - observed variables: ${listenedValues.mkString("", ", ", "")}"
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

  private[this] def updateFromHeap(): Unit = {
    h.getFirst match {
      case Some(i) =>
        output := input(i).value()
      case None => output := default
    }
  }

  // updates the extremum when an additional IntConstant must be used
  private[this] def notifyInsertOn(set: SetVariable, index: Int): Unit = {
    assert(set == listenedValuesIndices)
    if (consideredValue(index)) {
      // We wanted to remove first a considered value that didn't impact the extremum.
      // This value was put into the backlog and was always considered.
      // Now we want to reinsert this value. We have nothing to do except from unmark it as
      // backlogged. It is already in the heap.
      assert(isBacklogged(index))
      isBacklogged(index) = false
    } else if (notImpactingExtremum(input(index))) {
      // No impact, put in the backlog
      trimBacklog()
      putIntoBacklog(index)
    } else {
      // The added value is the new extremum
      output := input(index).value()
      // The added value is inserted in h and considered
      h.insert(index)
      consideredValue(index) = true
    }
  }

  // updates product when an IntConstant is not used anymore
  private[this] def notifyDeleteOn(set: SetVariable, index: Int): Unit = {
    assert(set == listenedValuesIndices)
    if (!consideredValue(index)) {
      // We wanted to insert first a value that didn't impact the extremum.
      // This value was put into the backlog and was not considered.
      // Now, we want to remove this value. We have nothing to do except from unmark it as
      // backlogged. It was never considered.
      assert(isBacklogged(index))
      isBacklogged(index) = false
    } else if (output.pendingValue == input(index).value() && consideredValue(index)) {
      // We are removing the current extremum. The new one can be in the heap or in the backlog.
      // We empty the backlog, update the heap and find the new extremum.
      processBacklog()
      h.removeElement(index)
      isBacklogged(index) = false
      consideredValue(index) = false
      updateFromHeap()
    } else {
      // No impacted, put in the backlog
      trimBacklog()
      putIntoBacklog(index)
    }
  }

  private[this] def putIntoBacklog(condValue: Int): Unit = {
    if (!isBacklogged(condValue)) {
      backlog = backlog :+ condValue
      isBacklogged(condValue) = true
      backlogSize += 1
    }
  }

  /** Remove from backlog all the heads that was processed and process values to reduce the size if
    * the backlog's length is bigger than maxBacklog
    */
  private[this] def trimBacklog(): Unit = {
    while (backlog.nonEmpty && (!isBacklogged(backlog.head) || backlogSize > maxBacklog)) {
      val condValue = backlog.dequeue()
      backlogSize -= 1
      if (isBacklogged(condValue)) processValueFromBacklog(condValue)
    }
  }

  /** Empty the backlog */
  private[this] def processBacklog(): Unit = {
    while (backlog.nonEmpty) {
      val condValue = backlog.dequeue()
      if (isBacklogged(condValue)) processValueFromBacklog(condValue)
    }
    backlogSize = 0
  }

  /** Used to add or remove a value in the heap.
    *
    * @param condValue
    *   Index put in the backlog previously
    */
  private[this] def processValueFromBacklog(condValue: Int): Unit = {
    if (consideredValue(condValue)) {
      // When this method is called, the only thing we can do with a considered value is remove it.
      h.removeElement(condValue)
      consideredValue(condValue) = false
    } else {
      // When this method is called, the only thing we can do with a not considered value is add it.
      h.insert(condValue)
      consideredValue(condValue) = true
    }
    isBacklogged(condValue) = false
  }

  /** Used only for testing
    *
    * @return
    *   all the private fields useful for testing
    */
  protected def currentBacklogStates()
    : (BinaryHeapWithMoveIntItem, mutable.Queue[Int], Array[Boolean], Array[Boolean]) =
    (h, backlog, isBacklogged, consideredValue)

}
