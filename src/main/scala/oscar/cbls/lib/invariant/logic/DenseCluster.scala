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

import oscar.cbls.core.computation.integer.{IntNotificationTarget, IntVariable}
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.core.computation.{IncredibleBulk, Invariant, Store}

/** Companion object of the [[DenseCluster]] class. */
object DenseCluster {

  /** Creates a [[DenseCluster]] invariant, which maintains clusters of the indices of an array:
    * `output(j) = {i in input.indices | input(i) == j}`. It is considered as a dense cluster
    * because `output` is an [[scala.Array]] and covers all the possible values of the input
    * variables. Update is O(1).
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   An array of variable to cluster.
    * @param output
    *   The clustered elements such that `output(j) = {i in input.indices | input(i) == j}`.
    * @param bulkIdentifier
    *   An [[oscar.cbls.core.computation.IncredibleBulk]] is used when several Invariant listen to
    *   vars. Warning: [[oscar.cbls.core.computation.IncredibleBulk]] are distinguished only by
    *   their identifier. Be sure to use the same one if you're referencing the same variables.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(
    model: Store,
    input: Array[IntVariable],
    output: Array[SetVariable],
    bulkIdentifier: Option[String],
    name: Option[String]
  ): DenseCluster = new DenseCluster(model, input, output, bulkIdentifier, name)
}

/** Invariant which maintains clusters of the indices of an array: `output(j) = {i in input.indices`
  * `| input(i) == j}`. It is considered as a dense cluster because `output` is an [[scala.Array]]
  * and covers all the possible values of the input variables. Update is O(1).
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   An array of variable to cluster.
  * @param output
  *   The clustered elements such that `output(j) = {i in input.indices | input(i) == j}`.
  * @param bulkIdentifier
  *   An [[oscar.cbls.core.computation.IncredibleBulk]] is used when several Invariant listen to
  *   vars. Warning: [[oscar.cbls.core.computation.IncredibleBulk]] are distinguished only by their
  *   identifier. Be sure to use the same one if you're referencing the same variables.
  * @param name
  *   The (optional) name of the Invariant.
  */
class DenseCluster(
  model: Store,
  input: Array[IntVariable],
  output: Array[SetVariable],
  // by the Cluster object.
  bulkIdentifier: Option[String] = None,
  name: Option[String] = None
) extends Invariant(model, name)
    with IntNotificationTarget {

  bulkIdentifier match {
    case None =>
      // No bulk is used
      for (vars <- input) this.registerStaticallyListenedElement(vars)
    case Some(bulkId) =>
      // Register static dependency via a bulk
      this.addIncredibleBulk(IncredibleBulk.bulkRegistering(input, bulkId, model))
  }

  for (cluster <- output) {
    cluster.setDefiningInvariant(this)
    cluster := Set.empty
  }

  for (i <- input.indices) {
    require(
      input(i).value().toInt < output.length,
      s"An input value is bigger than the upper bound of admissible value (output length). " +
        s"Input value: ${input(i).value()} - Exclusive upper bound: ${output.length}"
    )
    input(i).registerDynamicallyListeningElement(this, i)
    output(input(i).value().toInt) :+= i
  }

  /** Returns the output variables. */
  def apply(): Array[SetVariable] = output

  override def notifyIntChanges(
    intVariable: IntVariable,
    contextualVarIndex: Int,
    oldVal: Long,
    newVal: Long
  ): Unit = {
    require(
      newVal.toInt < output.length,
      s"Variable is updated with a value bigger than the upper bound of admissible value (output " +
        s"length). Variable: $intVariable - New Value: $newVal - " +
        s"Exclusive upper bound: ${output.length}"
    )
    assert(input(contextualVarIndex) == intVariable)

    output(oldVal.toInt) :-= contextualVarIndex
    output(newVal.toInt) :+= contextualVarIndex
  }

  override def checkInternals(): Unit = {
    for (i <- input.indices) {
      val v = input(i).value().toInt
      require(
        output(v).pendingValue.contains(i),
        s"checkInternal fails in invariant ${name()}. " +
          s"Found a variable that is not in expected cluster. " +
          s"variable: ${input(i)} - index: $i.\n ${internalDescription()}"
      )
    }

    for (value <- output.indices) {
      for (index <- output(value).pendingValue) {
        require(
          input(index).value() == value,
          s"checkInternals fail in invariant ${name()}. " +
            s"A variable has not the same value than its cluster's key. " +
            s"variable: ${input(index)} - cluster's key: $value.\n ${internalDescription()}"
        )
      }
    }
  }

  private[this] def internalDescription(): String = {
    var toReturn: String =
      s"\n\tinput: ${input.mkString("", ", ", "")}\n\tClusters pending values: |"

    for ((cluster, index) <- output.zipWithIndex) {
      if (cluster.pendingValue.nonEmpty) toReturn += s" $index -> ${cluster.pendingValue} |"
    }
    toReturn
  }
}
