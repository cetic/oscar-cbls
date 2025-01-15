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

/** Companion object of the [[FlatMap]] class. */
object FlatMap {

  /** Creates a FlapMap invariant, which maintains `{x | it exists v in input such that, x in`
    * `fun(v)}`.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   The set of integer on which apply the `fun` function.
    * @param fun
    *   The function defining the mapping.
    * @param output
    *   The SetVariable evaluating to `{x | it exists v in input such that, x in fun(v)}`.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(
    model: Store,
    input: SetVariable,
    fun: Int => Set[Int],
    output: SetVariable,
    name: Option[String] = None
  ): FlatMap = new FlatMap(model, input, fun, output, name)
}

/** Invariant which maintains `{x | it exists v in input such that, x in fun(v)}`.
  *
  * An update is in `O(n)` where `n` is the size of `fun(v)`.
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   The set of integer on which apply the `fun` function.
  * @param fun
  *   The function defining the mapping.
  * @param output
  *   The output of the invariant.
  * @param name
  *   The (optional) name of the Invariant.
  */
class FlatMap(
  model: Store,
  input: SetVariable,
  fun: Int => Set[Int],
  output: SetVariable,
  name: Option[String]
) extends Invariant(model, name)
    with SetNotificationTarget {

  private[this] val outputCount: mutable.HashMap[Int, Int] = mutable.HashMap.empty

  input.registerStaticallyAndDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)

  output := Set.empty
  for (v <- input.value()) notifyInsertOn(v)

  override def notifySetChanges(
    setVariable: SetVariable,
    index: Int,
    addedElems: Iterable[Int],
    removedElems: Iterable[Int],
    oldValue: Set[Int],
    newValue: Set[Int]
  ): Unit = {
    assert(input == setVariable)
    for (added   <- addedElems) notifyInsertOn(added)
    for (removed <- removedElems) notifyRemoveOn(removed)
  }

  override def checkInternals(): Unit = {
    val expected = input.pendingValue.flatMap(fun)
    require(
      output.pendingValue == expected,
      s"""checkInternals fails in invariant ${name()}.
         |output != {x | it exists v in input such that, x in fun(v)}
         |output: ${output.pendingValue}
         |expected: $expected
         |""".stripMargin
    )
  }

  // Updates the output when a new value is inserted in the input.
  private[this] def notifyInsertOn(v: Int): Unit = {
    val mappedV: Set[Int] = fun(v)
    for (m <- mappedV) {
      val count = outputCount.getOrElse(m, 0)
      if (count == 0) output :+= m
      outputCount(m) = count + 1
    }
  }

  // Updates the output when a new value is removed from the input.
  private[this] def notifyRemoveOn(v: Int): Unit = {
    val mappedV: Set[Int] = fun(v)
    for (m <- mappedV) {
      // We are not supposed to remove a non-inserted value. So outputCount(mappedV) is always defined.
      val count = outputCount(m)
      require(count != 0, s"No input has $m as mapped value.")
      if (count == 1) output :-= m
      outputCount(m) -= 1
    }
  }

}
