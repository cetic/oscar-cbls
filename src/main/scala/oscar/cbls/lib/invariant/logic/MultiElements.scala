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
import oscar.cbls.core.computation.set.{SetNotificationTarget, SetVariable}

import scala.collection.mutable

/** Companion object of the [[MultiElements]] class. */
object MultiElements {

  /** Creates a [[MultiElements]] invariant.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   An [[Array]] of [[IntVariable]].
    * @param listenedVariablesIndices
    *   A [[SetVariable]] containing the indices of the value to return.
    * @param output
    *   A [[SetVariable]] containing {input(i) | i in listenedVariablesIndices}
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
    listenedVariablesIndices: SetVariable,
    output: SetVariable,
    bulkIdentifier: Option[String] = None,
    name: Option[String] = None
  ): MultiElements = {
    new MultiElements(model, input, listenedVariablesIndices, output, bulkIdentifier, name)
  }
}

/** [[Invariant]] that maintains {input(i) | i in listenedVariablesIndices} where input is an
  * [[Array]] of [[IntVariable]]. Update is O(1).
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   An [[Array]] of [[IntVariable]].
  * @param listenedVariablesIndices
  *   A [[SetVariable]] containing the indices of the value to return.
  * @param output
  *   A [[SetVariable]] containing {input(i) | i in listenedVariablesIndices}
  * @param bulkIdentifier
  *   A [[IncredibleBulk]] is used when several [[Invariant]] listen to vars. Warning:
  *   [[IncredibleBulk]] are distinguished only by their identifier. Be sure to use the same one if
  *   you're referencing the same variables.
  * @param name
  *   The name (optional) of your Invariant.
  */
class MultiElements(
  model: Store,
  input: Array[IntVariable],
  listenedVariablesIndices: SetVariable,
  output: SetVariable,
  bulkIdentifier: Option[String] = None,
  name: Option[String] = None
) extends Invariant(model, name)
    with IntNotificationTarget
    with SetNotificationTarget {

  private[this] val keysForRemoval: Array[KeyForRemoval[_]] = new Array(input.length)
  // valuesCount(x) ==  the number of listened variables with value x
  private[this] val valuesCount: mutable.HashMap[Int, Int] = new mutable.HashMap()

  bulkIdentifier match {
    case None =>
      // No bulk is used
      for (vars <- input) this.registerStaticallyListenedElement(vars)
    case Some(bulkId) =>
      // Register static dependency via a bulk
      this.addIncredibleBulk(IncredibleBulk.bulkRegistering(input, bulkId, model))
  }

  output := Set.empty
  listenedVariablesIndices.registerStaticallyAndDynamicallyListeningElement(this)
  for (i <- listenedVariablesIndices.value()) {
    keysForRemoval(i) = input(i).registerDynamicallyListeningElement(this, i)
    internalInsert(input(i).value().toInt)
  }

  output.setDefiningInvariant(this)

  @inline
  override def notifyIntChanges(
    intVariable: IntVariable,
    contextualVarIndex: Int,
    oldVal: Long,
    newVal: Long
  ): Unit = {
    internalRemove(oldVal.toInt)
    internalInsert(newVal.toInt)
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
    val listenedValues: Set[Int] = listenedVariablesIndices.value().map(i => input(i).value().toInt)

    require(
      output.pendingValue == listenedValues,
      s"checkInternals fails in invariant ${name()}. " +
        s"output != {input(i) | i in listenedVariablesIndices}. " +
        s"output: ${output.pendingValue} - indices: $listenedVariablesIndices - input: ${input
            .mkString("", ", ", "")}."
    )
  }

  @inline
  private[this] def internalInsert(value: Int): Unit = {
    valuesCount.get(value) match {
      case Some(_) => valuesCount(value) += 1
      case None =>
        valuesCount += (value -> 1)
        output :+= value
    }
  }

  @inline
  private[this] def internalRemove(value: Int): Unit = {
    assert(valuesCount.contains(value))
    if (valuesCount(value) == 1) {
      valuesCount -= value
      output :-= value
    } else {
      valuesCount(value) -= 1
    }
  }

  @inline
  private[this] def notifyInsertOn(set: SetVariable, index: Int): Unit = {
    assert(set == listenedVariablesIndices)
    keysForRemoval(index) = input(index).registerDynamicallyListeningElement(this, index)
    internalInsert(input(index).value().toInt)
  }

  @inline
  private[this] def notifyDeleteOn(set: SetVariable, index: Int): Unit = {
    assert(set == listenedVariablesIndices)

    keysForRemoval(index).delete()
    keysForRemoval(index) = null
    internalRemove(input(index).value().toInt)
  }
}
