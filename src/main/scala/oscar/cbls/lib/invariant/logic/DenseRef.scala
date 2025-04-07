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

import oscar.cbls.core.computation.set.{SetNotificationTarget, SetVariable}
import oscar.cbls.core.computation.{IncredibleBulk, Invariant, Store}

/** Companion object of [[DenseRef]] class. */
object DenseRef {

  /** Creates a DenseRef invariant.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   The sets containing the references values.
    * @param output
    *   An array of SetVariable such that output(i) = {j | i in input(j)}.
    * @param name
    *   The (optional) name of the Invariant.
    * @param bulkUsed
    *   Whether the input variables must be bulked (see
    *   [[oscar.cbls.core.computation.IncredibleBulk]]).
    */
  def apply(
    model: Store,
    input: Array[SetVariable],
    output: Array[SetVariable],
    name: Option[String] = None,
    bulkUsed: Boolean = false
  ): DenseRef = {
    new DenseRef(model, input, output, name, bulkUsed)
  }

  /** Creates a [[DenseRef]] invariant and instantiate the output [[scala.Array]] of
    * [[oscar.cbls.core.computation.set.SetVariable]].
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   The sets containing the references values.
    * @param upperBound
    *   The integer such that the input values are in [0, upperBound[.
    * @param name
    *   The (optional) name of the Invariant.
    * @param bulkUsed
    *   Whether the input variables must be bulked (see
    *   [[oscar.cbls.core.computation.IncredibleBulk]]).
    */
  def makeDenseRef(
    model: Store,
    input: Array[SetVariable],
    upperBound: Int = Int.MaxValue,
    name: Option[String] = None,
    bulkUsed: Boolean = false
  ): DenseRef = {
    require(upperBound > 0)
    val output: Array[SetVariable] = Array.fill(upperBound)(SetVariable(model, Set.empty))
    new DenseRef(model, input, output, name, bulkUsed)
  }
}

/** Invariant such that `output(i) = {j | i in input(j)}`. It is considered as a dense ref because
  * output is an array and cover all possible values that can contains the input
  * [[oscar.cbls.core.computation.set.SetVariable]]. Update is in O(1).
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   The sets containing the references values.
  * @param output
  *   An array of SetVariable such that `output(i) = {j | i in input(j)}`.
  * @param name
  *   The (optional) name of the Invariant.
  * @param bulkUsed
  *   Whether the input variables must be bulked (see
  *   [[oscar.cbls.core.computation.IncredibleBulk]]).
  */
class DenseRef(
  model: Store,
  input: Array[SetVariable],
  output: Array[SetVariable],
  name: Option[String],
  bulkUsed: Boolean
) extends Invariant(model, name)
    with SetNotificationTarget {

  if (bulkUsed) {
    // Registers static dependency via a bulk
    this.addIncredibleBulk(IncredibleBulk.bulkRegistering(input, model))
  } else {
    // No bulk is used
    for (vars <- input) this.registerStaticallyListenedElement(vars)
  }

  for (ref <- output) {
    ref.setDefiningInvariant(this)
    ref := Set.empty
  }

  for (j <- input.indices) {
    input(j).registerDynamicallyListeningElement(this, j)
    for (ref <- input(j).value()) {
      require(
        ref < output.length,
        s"An input value is bigger than the upper bound of admissible value (output length). " +
          s"Set containing the value: ${input(j)} - Value: $ref - Upper bound: ${output.length}"
      )
      output(ref) :+= j
    }
  }

  /** Returns the output variables. */
  def apply(): Array[SetVariable] = output

  override def notifySetChanges(
    setVariable: SetVariable,
    index: Int,
    addedElems: Iterable[Int],
    removedElems: Iterable[Int],
    oldValue: Set[Int],
    newValue: Set[Int]
  ): Unit = {
    for (added <- addedElems) {
      require(
        added < output.length,
        s"Try to add an value bigger An input value is bigger than " +
          s"the upper bound of admissible value (output length) in Set $setVariable. " +
          s"Value: $added - Exclusive upper bound: ${output.length}"
      )
      output(added) :+= index
    }
    for (removed <- removedElems) output(removed) :-= index
  }

  override def checkInternals(): Unit = {
    // output(i) == {j | i in input(j)}
    for (i <- output.indices) {
      for (j <- output(i).pendingValue) {
        require(
          input(j).value().contains(i),
          s"checkInternals fails in invariant ${name()}. " +
            s"output is not referencing to the good input set. " +
            s"ref: $i - output(i): ${output(i).pendingValue} - input: ${input.mkString("", ", ", "")}"
        )
      }
    }
  }

}
