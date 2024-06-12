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

import oscar.cbls.core.computation.{IncredibleBulk, Invariant, KeyForRemoval, Store}
import oscar.cbls.core.computation.integer.{IntNotificationTarget, IntVariable}
import oscar.cbls.core.computation.set.{SetNotificationTarget, SetVariable}

/** Companion object of the [[Prod]] class. */
object Prod {

  /** Creates a [[Prod]] invariant.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   An [[Array]] of [[IntVariable]].
    * @param listenedVariablesIndices
    *   A [[SetVariable]] containing the indices of the input variables to be listened to calculate
    *   the product.
    * @param output
    *   The output [[IntVariable]] containing Prod(input(i) | i in listenedVariablesIndices).
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
    output: IntVariable,
    bulkIdentifier: Option[String] = None,
    name: Option[String] = None
  ): Prod = {
    new Prod(model, input, listenedVariablesIndices, output, bulkIdentifier, name)
  }
}

/** [[Invariant]] that maintains Prod(input(i) | i in listenedVariablesIndices). Update is in O(1).
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   An [[Array]] of [[IntVariable]].
  * @param listenedVariablesIndices
  *   A [[SetVariable]] containing the indices of the input variables to be listened to calculate
  *   the product.
  * @param output
  *   The output [[IntVariable]] containing Prod(input(i) | i in listenedVariablesIndices).
  * @param bulkIdentifier
  *   A [[IncredibleBulk]] is used when several [[Invariant]] listen to vars. Warning:
  *   [[IncredibleBulk]] are distinguished only by their identifier. Be sure to use the same one if
  *   you're referencing the same variables.
  * @param name
  *   The name (optional) of your Invariant.
  */
class Prod(
  model: Store,
  input: Array[IntVariable],
  listenedVariablesIndices: SetVariable,
  output: IntVariable,
  bulkIdentifier: Option[String] = None,
  name: Option[String] = None
) extends Invariant(model, name)
    with IntNotificationTarget
    with SetNotificationTarget {

  private[this] val keysForRemoval: Array[KeyForRemoval[_]] = new Array(input.length)
  private[this] var numberOfZeroFactors: Int                = 0
  private[this] var nonZeroProduct: Long                    = 1

  bulkIdentifier match {
    case None =>
      // No bulk is used
      for (vars <- input) this.registerStaticallyListenedElement(vars)
    case Some(bulkId) =>
      // Register static dependencies via a bulk
      this.addIncredibleBulk(IncredibleBulk.bulkRegistering(input, bulkId, model))
  }

  for (i <- listenedVariablesIndices.value()) {
    keysForRemoval(i) = input(i).registerDynamicallyListeningElement(this, i)
    if (input(i).value() == 0) numberOfZeroFactors += 1
    else nonZeroProduct *= input(i).value()
  }

  listenedVariablesIndices.registerStaticallyAndDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)

  updateOutput()

  @inline
  override def notifyIntChanges(
    intVariable: IntVariable,
    contextualVarIndex: Int,
    oldVal: Long,
    newVal: Long
  ): Unit = {
    // The update mechanism of an IntVariable is such that oldVal is never equal to newVal.
    // So the case oldVal == 0 && newVal == 0 never happens.
    if (oldVal == 0 && newVal != 0) {
      numberOfZeroFactors -= 1
      nonZeroProduct *= newVal
    } else if (oldVal != 0 && newVal == 0) {
      numberOfZeroFactors += 1
      nonZeroProduct /= oldVal
    } else {
      nonZeroProduct /= oldVal
      nonZeroProduct *= newVal
    }
    updateOutput()
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
    val listenedVariables: Set[IntVariable] =
      listenedVariablesIndices.value().map(i => input(i))
    val expectedProd =
      listenedVariables.foldLeft(1L)((acc: Long, x: IntVariable) => acc * x.value())

    require(
      output.pendingValue == expectedProd,
      s"checkInternals fails in invariant ${name()}. " +
        s"output != the product of the listened variables. " +
        s"output: ${output.pendingValue} - expected product: $expectedProd " +
        s"- listened variables: ${listenedVariables.mkString("", ", ", "")}"
    )

  }

  @inline
  private[this] def updateOutput(): Unit = {
    if (numberOfZeroFactors == 0) output := nonZeroProduct
    else output                          := 0
  }

  @inline
  private[this] def notifyInsertOn(set: SetVariable, index: Int): Unit = {
    assert(set == listenedVariablesIndices)

    keysForRemoval(index) = input(index).registerDynamicallyListeningElement(this, index)

    if (input(index).value() == 0) numberOfZeroFactors += 1
    else nonZeroProduct *= input(index).value()

    updateOutput()
  }

  @inline
  private[this] def notifyDeleteOn(set: SetVariable, index: Int): Unit = {
    assert(set == listenedVariablesIndices)

    keysForRemoval(index).delete()
    keysForRemoval(index) = null

    if (input(index).value() == 0) numberOfZeroFactors -= 1
    else nonZeroProduct /= input(index).value()

    updateOutput()
  }
}
