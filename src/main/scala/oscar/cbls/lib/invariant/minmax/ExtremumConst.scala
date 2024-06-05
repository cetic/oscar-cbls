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
import oscar.cbls.core.computation.{Invariant, Store}
import oscar.cbls.core.computation.integer.{IntConstant, IntVariable}
import oscar.cbls.core.computation.set.{SetNotificationTarget, SetVariable}

import scala.collection.mutable

/** Abstract [[Invariant]] that maintains Extremum(input(i) | i in cond). Exact ordering is
  * specified by implementing abstract method of the class. This invariant is lazy and maintains a
  * todo list of postponed updates. Update is in O (log(n)) in worst case. If the update does not
  * impact the output, it is postponed in O(1). Otherwise, it is performed in O(log(n)). It faster
  * for neighborhood exploration with moves and backtracks.
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   An array of [[IntConstant]]
  * @param cond
  *   A [[SetVariable]] containing the indices of the input variables to be observed to calculate
  *   the extremum.
  * @param output
  *   The output [[IntVariable]].
  * @param default
  *   The default value of the extremum.
  * @param maxBacklog
  *   The maximum number of postponed updates that doesn't affect the extremum.
  * @param name
  *   The name (optional) of your Invariant
  */
abstract class ExtremumConst(
  model: Store,
  input: Array[IntConstant],
  cond: SetVariable,
  output: IntVariable,
  default: Long,
  maxBacklog: Int,
  name: Option[String] = None
) extends Invariant(model, name)
    with SetNotificationTarget {

  // Use to stock the indices of the considered variables. All operation are in O(log(n))
  private[this] val h: BinaryHeapWithMoveIntItem =
    BinaryHeapWithMoveIntItem((i: Int) => ord(input(i)), input.length, input.length)

  // Use to postpone not impacting updates
  private[this] var backlog: mutable.Queue[Int] = mutable.Queue()
  private[this] var backlogSize: Int            = 0
  // Tell if the postponed update have to be performed
  private[this] val isBacklogged: Array[Boolean] = Array.fill(input.length)(false)
  // Tell which value are used to compute the extremum
  private[this] val consideredValue: Array[Boolean] = Array.fill(input.length)(false)

  for (i <- cond.value()) {
    h.insert(i)
    consideredValue(i) // A value is considered if and only if it is in h
  }

  cond.registerStaticallyAndDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)

  updateFromHeap()

  def ord(v: IntVariable): Long

  def notImpactingExtremum(newValue: IntConstant): Boolean

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
      var observedVariables: Array[IntVariable] = new Array[IntVariable](0)
      for (i: Int <- cond.value()) observedVariables = observedVariables :+ input(i)

      require(
        output.value() == observedVariables.minBy(ord).value(),
        s"checkInternals fails in invariant ${name()}. " +
          s"output != min/max of observed variables. " +
          s"output: $output - observed variables: ${observedVariables.mkString("", ", ", "")}"
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
  private[this] def updateFromHeap(): Unit = {
    h.getFirst match {
      case Some(i) => output := input(i).value()
      case None    => output := default
    }
  }

  @inline
  private[this] def notifyInsertOn(set: SetVariable, index: Int): Unit = {
    assert(set == cond)
    if (consideredValue(index)) {
      // We wanted to remove first a considered value that didn't impact the extremum.
      // This value was put into the backlog and was always considered.
      // Now we want to reinsert this value. We have nothing to do. It is already in the heap.
      assert(isBacklogged(index))
      isBacklogged(index) = false
      return
    }
    if (notImpactingExtremum(input(index))) {
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

  @inline
  private[this] def notifyDeleteOn(set: SetVariable, index: Int): Unit = {
    assert(set == cond)
    if (!consideredValue(index)) {
      // We wanted to insert first a value that didn't impact the extremum.
      // This value was put into the backlog and was not considered.
      // Now, we want to remove this value. We have nothing to do. It was never considered.
      assert(isBacklogged(index))
      isBacklogged(index) = false
      return
    }

    if (output.value() == input(index).value() && consideredValue(index)) {
      // We are removing the current extremum. The new one can be in the heap or in the backlog.
      // We empty the backlog, update the heap and find the new extremum
      processBacklog()
      h.removeElement(index)
      isBacklogged(index) = false
      updateFromHeap()
    } else {
      // No impacted, put in the backlog
      trimBacklog()
      putIntoBacklog(index)
    }
  }

  @inline
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
  @inline
  private[this] def trimBacklog(): Unit = {
    while ((!isBacklogged(backlog.head) || backlogSize > maxBacklog) && backlog.nonEmpty) {
      val condValue = backlog.dequeue()
      backlogSize -= 1
      if (isBacklogged(condValue)) processValueFromBacklog(condValue)
    }
  }

  /** Empty the backlog */
  @inline
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
  @inline
  private[this] def processValueFromBacklog(condValue: Int): Unit = {
    if (consideredValue(condValue)) {
      // When this method is called, the only thing we can do with a considered value is remove it.
      assert(cond.value().contains(condValue))
      h.removeElement(condValue)
      consideredValue(condValue) = false
    } else {
      // When this method is called, the only thing we can do with a not considered value is add it.
      assert(!cond.value().contains(condValue))
      h.insert(condValue)
      consideredValue(condValue) = true
    }
    isBacklogged(condValue) = false
  }

}
