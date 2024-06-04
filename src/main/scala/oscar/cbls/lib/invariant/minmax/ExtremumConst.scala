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

abstract class ExtremumConst(
  model: Store,
  input: Array[IntConstant],
  cond: SetVariable,
  output: IntVariable,
  default: Long,
  maxBackLog: Int,
  name: Option[String] = None
) extends Invariant(model, name)
    with SetNotificationTarget {

  // Use to stock the indices of the listened variables. All operation are in O(log(n))
  private[this] val h: BinaryHeapWithMoveIntItem =
    BinaryHeapWithMoveIntItem((i: Int) => ord(input(i)), input.length, input.length)

  private[this] var backLog: List[Int]              = List()
  private[this] var backLogSize: Int                = 0
  private[this] val isBacklogged: Array[Boolean]    = Array.fill(input.length)(false)
  private[this] val consideredValue: Array[Boolean] = Array.fill(input.length)(false)

  for (i <- cond.value()) {
    h.insert(i)
    consideredValue(i)
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

  override def checkInternals(): Unit = ???

  @inline
  private[this] def updateFromHeap(): Unit = {
    h.getFirst match {
      case Some(i) => output := input(i).value()
      case None    => output := default
    }
  }

  @inline
  private[this] def putIntoBacklog(condValue: Int): Unit = {
    if (!isBacklogged(condValue)) {
      backLog = condValue :: backLog
      isBacklogged(condValue) = true
      backLogSize += 1
    }
  }

  /** Used to add or remove a value in the heap.
    *
    * @param condValue
    *   Index put in the backlog previously
    */
  @inline
  private[this] def processValueFromBacklog(condValue: Int): Unit = {
    if (consideredValue(condValue)) { // should be removed
      assert(cond.value().contains(condValue))
      h.removeElement(condValue)
      consideredValue(condValue) = false
    } else { // should be added
      assert(!cond.value().contains(condValue))
      h.insert(condValue)
      consideredValue(condValue) = true
    }
    isBacklogged(condValue) = false
  }

  /** Remove from backlog all the heads that was processed and process values to reduce the size if
    * the backlog*
    */
  @inline
  private[this] def trimBacklog(): Unit = {
    while ((!isBacklogged(backLog.head) || backLogSize > maxBackLog) && backLog.nonEmpty) {
      val condValue = backLog.head
      if (isBacklogged(condValue)) processValueFromBacklog(condValue)
      backLogSize -= 1
      backLog = backLog.tail
    }
  }

  /** Empty the backlog */
  @inline
  private[this] def processBacklog(): Unit = {
    while (backLog.nonEmpty) {
      val condValue = backLog.head
      backLog = backLog.tail
      if (isBacklogged(condValue)) processValueFromBacklog(condValue)
    }
    backLogSize = 0
  }

  @inline
  private[this] def notifyInsertOn(set: SetVariable, index: Int): Unit = {
    assert(set == cond)

    if (notImpactingExtremum(input(index))) {
      // No impact, put in the backlog
      trimBacklog()
      putIntoBacklog(index)
    } else {
      // The added value is the new extremum
      output := input(index).value()
      h.insert(index)
      consideredValue(index) = true
    }

  }

  @inline
  private[this] def notifyDeleteOn(set: SetVariable, index: Int): Unit = {
    assert(set== cond)
    if (output.value() == input(index).value()) {
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

}
