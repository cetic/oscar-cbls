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

package oscar.cbls.lib.invariant.numeric

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.{SetNotificationTarget, SetVariable}
import oscar.cbls.core.computation.{Invariant, Store}

/** Companion object of the [[SumConst]] class. */
object SumConst {

  /** Creates a SumConst invariant, , which maintains `Sum(input(i) | i in listenedValuesIndices)`,
    * * where `input` is an array of constant integers.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   The arrays of constant integers to sum.
    * @param listenedValuesIndices
    *   A SetVariable containing the indices of the input constants to sum.
    * @param output
    *   The output variable evaluating to `Sum(input(i) | i in listenedValuesIndices)`.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(
    model: Store,
    input: Array[Long],
    listenedValuesIndices: SetVariable,
    output: IntVariable,
    name: Option[String] = None
  ): SumConst = {
    new SumConst(model, input, listenedValuesIndices, output, name)
  }
}

/** Invariant that maintains `Sum(input(i) | i in` `listenedValuesIndices}`. , where `input` is an
  * array of constant integers. Update is in O(1).
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   The arrays of constant integers to sum.
  * @param listenedValuesIndices
  *   A SetVariable containing the indices of the input constants to sum.
  * @param output
  *   The output variable evaluating to `Sum(input(i) | i in listenedValuesIndices)`.
  * @param name
  *   The (optional) name of the Invariant.
  */
class SumConst(
  model: Store,
  input: Array[Long],
  listenedValuesIndices: SetVariable,
  output: IntVariable,
  name: Option[String] = None
) extends Invariant(model, name)
    with SetNotificationTarget {

  listenedValuesIndices.registerStaticallyAndDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)

  output := 0
  for (i <- listenedValuesIndices.value()) output :+= input(i)

  override def notifySetChanges(
    setVariable: SetVariable,
    index: Int,
    addedElems: Iterable[Int],
    removedElems: Iterable[Int],
    oldValue: Set[Int],
    newValue: Set[Int]
  ): Unit = {
    assert(setVariable == listenedValuesIndices, "Input SetVariable is incorrect")

    for (added   <- addedElems) output :+= input(added)
    for (removed <- removedElems) output :-= input(removed)
  }

  override def checkInternals(): Unit = {
    val listenedValues: List[Long] = listenedValuesIndices.pendingValue.toList.map(i => input(i))
    val expectedSum                = listenedValues.foldLeft(0L)((acc: Long, x: Long) => acc + x)

    require(
      output.pendingValue == expectedSum,
      s"checkInternals fails in invariant ${name()}. " +
        s"output != the sum of the listened values. " +
        s"output: ${output.pendingValue} - expected sum: $expectedSum " +
        s"- listened values: ${listenedValues.mkString("", ", ", "")} " +
        s"- listened index: ${listenedValuesIndices.pendingValue}"
    )
  }
}
