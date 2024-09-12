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
import oscar.cbls.core.computation.set.{SetNotificationTarget, SetVariable}

import scala.collection.mutable

/** Companion object of the [[SetMap]] class. */
object SetMap {

  /** Creates a SetMap invariant, which maintains `{fun(x) | x in input}`.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   The set of integer on which apply the `fun` function.
    * @param fun
    *   The function defining the mapping.
    * @param output
    *   The SetVariable evaluating to `{fun(x) | x in input}`.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(
    model: Store,
    input: SetVariable,
    fun: Int => Int,
    output: SetVariable,
    name: Option[String] = None
  ): SetMap = new SetMap(model, input, fun, output, name)
}

/** Invariant which maintains `{fun(x) | x in input}`. If `fun` is in O(1), update is in O(1) too.
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   The set of integer on which apply the `fun` function.
  * @param fun
  *   The function defining the mapping.
  * @param output
  *   The SetVariable evaluating to `{fun(x) | x in input}`.
  * @param name
  *   The (optional) name of the Invariant.
  */
class SetMap(
  model: Store,
  input: SetVariable,
  fun: Int => Int,
  output: SetVariable,
  name: Option[String]
) extends Invariant(model, name)
    with SetNotificationTarget {

  // outputCount(x) == the number of input value v such that fun(v) == x
  private[this] val outputCount: mutable.HashMap[Int, Int] = mutable.HashMap.empty

  input.registerStaticallyAndDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)

  for (v <- input.value()) {
    val mappedV = fun(v)
    val count   = outputCount.getOrElse(mappedV, 0)
    if (count == 0) output :+= mappedV
    outputCount(mappedV) = count + 1
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
    val expected = input.value().map(fun)
    require(
      output.pendingValue == expected,
      s"checkInternals fails in invariant ${name()}. " +
        s"output != {fun(x) | x in input}. " +
        s"output: ${output.pendingValue} - expected set: $expected - input: $input"
    )
  }

  // Updates the output when a new value is inserted in the input.
  private[this] def notifyInsertOn(set: SetVariable, v: Int): Unit = {
    assert(input == set)
    val mappedV = fun(v)
    val count   = outputCount.getOrElse(mappedV, 0)
    if (count == 0) output :+= mappedV
    outputCount(mappedV) = count + 1
  }

  // Updates the output when a new value is removed from the input.
  private[this] def notifyDeleteOn(set: SetVariable, v: Int): Unit = {
    assert(input == set)
    val mappedV = fun(v)
    // We are not supposed to remove a non-inserted value. So outputCount(mappedV) is always defined.
    val count = outputCount(mappedV)
    assert(count != 0, s"Non input has $mappedV as mapped value.")
    if (count == 1) output :-= mappedV
    outputCount(mappedV) -= 1
  }
}
