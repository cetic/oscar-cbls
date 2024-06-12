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

/** Companion object of the [[ProdConst]] class. */
object ProdConst {

  /** Creates a [[ProdConst]] invariant
   *
   * @param model
   *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
   * @param input
   *   An [[Array]] of [[IntConstant]].
   * @param listenedValuesIndices
   *   A [[SetVariable]] containing the indices of the input variables to be listened to calculate
   *   the product.
   * @param output
   *   The output [[IntVariable]] containing Prod(input(i) | i in listenedValuesIndices).
   * @param name
   *   The name (optional) of your Invariant.
   */
  def apply(model: Store,
            input: Array[IntConstant],
            listenedValuesIndices: SetVariable,
            output: IntVariable,
            name: Option[String] = None): ProdConst = {
    new ProdConst(model, input, listenedValuesIndices, output, name)
  }
}

/** [[Invariant]] that maintains Prod(input(i) | i in listenedValuesIndices}. Update is in O(1).
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   An [[Array]] of [[IntConstant]].
  * @param listenedValuesIndices
  *   A [[SetVariable]] containing the indices of the input variables to be listened to calculate
  *   the product.
  * @param output
  *   The output [[IntVariable]] containing Prod(input(i) | i in listenedValuesIndices).
  * @param name
  *   The name (optional) of your Invariant.
  */
class ProdConst(
  model: Store,
  input: Array[IntConstant],
  listenedValuesIndices: SetVariable,
  output: IntVariable,
  name: Option[String] = None
) extends Invariant(model, name)
    with SetNotificationTarget {

  private[this] var numberOfZeroFactors: Int = 0
  private[this] var nonZeroProduct: Long     = 1

  for (i <- listenedValuesIndices.value()) {
    if (input(i).value() == 0) numberOfZeroFactors += 1
    else nonZeroProduct *= input(i).value()
  }

  listenedValuesIndices.registerStaticallyAndDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)

  updateOutput()

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
    val listenedValues: Set[IntConstant] = listenedValuesIndices.value().map(i => input(i))
    val expectedProd = listenedValues.foldLeft(1L)((acc: Long, x: IntConstant) => acc * x.value())

    require(
      output.pendingValue == expectedProd,
      s"checkInternals fails in invariant ${name()}. " +
        s"output != the product of the listened values. " +
        s"output: ${output.pendingValue} - expected sum: $expectedProd " +
        s"- listened values: ${listenedValues.mkString("", ", ", "")}"
    )
  }

  @inline
  private[this] def updateOutput(): Unit = {
    if (numberOfZeroFactors == 0) output := nonZeroProduct
    else output                          := 0
  }

  @inline
  private[this] def notifyInsertOn(set: SetVariable, index: Int): Unit = {
    assert(set == listenedValuesIndices)

    if (input(index).value() == 0) numberOfZeroFactors += 1
    else nonZeroProduct *= input(index).value()

    updateOutput()
  }

  @inline
  private[this] def notifyDeleteOn(set: SetVariable, index: Int): Unit = {
    assert(set == listenedValuesIndices)

    if (input(index).value() == 0) numberOfZeroFactors -= 1
    else nonZeroProduct /= input(index).value()

    updateOutput()
  }

}
