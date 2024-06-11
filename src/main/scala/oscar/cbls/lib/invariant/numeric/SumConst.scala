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

import oscar.cbls.core.computation.{Invariant, Store}
import oscar.cbls.core.computation.integer.{IntConstant, IntVariable}
import oscar.cbls.core.computation.set.{SetNotificationTarget, SetVariable}

/** Companion object of the [[SumConst]] class. */
object SumConst {

  /** Creates a [[SumConst]] invariant.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   An [[Array]] of [[IntConstant]].
    * @param listenedValuesIndices
    *   A [[SetVariable]] containing the indices of the input variables to be listened to calculate
    *   the sum.
    * @param output
    *   The output [[IntVariable]] containing Sum(input(i) | i in listenedValuesIndices}.
    * @param name
    *   The name (optional) of your Invariant.
    */
  def apply(
    model: Store,
    input: Array[IntConstant],
    listenedValuesIndices: SetVariable,
    output: IntVariable,
    name: Option[String] = None
  ): SumConst = {
    new SumConst(model, input, listenedValuesIndices, output, name)
  }
}

/** [[Invariant]] that maintains Sum(input(i) | i in listenedValuesIndices}
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   An [[Array]] of [[IntConstant]].
  * @param listenedValuesIndices
  *   A [[SetVariable]] containing the indices of the input variables to be listened to calculate
  *   the sum.
  * @param output
  *   The output [[IntVariable]] containing Sum(input(i) | i in listenedValuesIndices}.
  * @param name
  *   The name (optional) of your Invariant.
  */
class SumConst(
  model: Store,
  input: Array[IntConstant],
  listenedValuesIndices: SetVariable,
  output: IntVariable,
  name: Option[String] = None
) extends Invariant(model, name)
    with SetNotificationTarget {

  listenedValuesIndices.registerStaticallyAndDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)

  output := 0
  for (i <- listenedValuesIndices.value()) output :+= input(i).value()

  @inline
  override def notifySetChanges(
    setVariable: SetVariable,
    index: Int,
    addedElems: Iterable[Int],
    removedElems: Iterable[Int],
    oldValue: Set[Int],
    newValue: Set[Int]
  ): Unit = {
    assert(setVariable == listenedValuesIndices)

    for (added   <- addedElems) output :+= added
    for (removed <- removedElems) output :-= removed
  }

  override def checkInternals(): Unit = {
    val listenedValues: Set[IntConstant] = listenedValuesIndices.value().map(i => input(i))
    val expectedSum = listenedValues.foldLeft(0L)((acc: Long, x: IntConstant) => acc + x.value())

    require(
      output.pendingValue == expectedSum,
      s"checkInternals fails in invariant ${name()}. " +
        s"output != the sum of the listened values. " +
        s"output: ${output.pendingValue} - expected sum: $expectedSum " +
        s"- listened values: ${listenedValues.mkString("", ", ", "")}"
    )
  }
}
