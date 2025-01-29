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

import scala.collection.immutable.HashMap

/** Companion object of [[SparseCluster]] class. */
object SparseCluster {

  /** Creates a SparseCluster invariant, which maintains clusters of the indices of an array:
    * `output(j) = {i in input .indices | input(i) == j}`. It is considered as a sparse cluster
    * because output is an [[scala.collection.immutable.HashMap]] and covers only some preselected
    * possible values of the input variables. Update is in O(1).
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   The elements we want to cluster.
    * @param output
    *   A Hashmap such that `output(j) = {i in input.indices | input(i) == j}`.
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
    output: HashMap[Long, SetVariable],
    bulkIdentifier: Option[String],
    name: Option[String]
  ): SparseCluster = {
    new SparseCluster(model, input, output, bulkIdentifier, name)
  }
}

/** Invariant which maintains clusters of the indices of an array: `output(j) = {i in input`
  * `.indices| input(i) == j}`. It is considered as a sparse cluster because output is an
  * [[scala.collection.immutable.HashMap]] and covers only some preselected possible values of the
  * input variables. Update is in O(1).
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   The elements we want to cluster
  * @param output
  *   A Hashmap such that `output(j) = {i in input.indices | input(i) == j}`.
  * @param bulkIdentifier
  *   An [[oscar.cbls.core.computation.IncredibleBulk]] is used when several Invariant listen to
  *   vars. Warning: [[oscar.cbls.core.computation.IncredibleBulk]] are distinguished only by their
  *   identifier. Be sure to use the same one if you're referencing the same variables.
  * @param name
  *   The (optional) name of the Invariant.
  */
class SparseCluster(
  model: Store,
  input: Array[IntVariable],
  output: HashMap[Long, SetVariable],
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

  for (cluster <- output.values) {
    cluster.setDefiningInvariant(this)
    cluster := Set.empty
  }

  for (i <- input.indices) {
    input(i).registerDynamicallyListeningElement(this, i)
    val cluster = output.get(input(i).value())
    if (cluster.nonEmpty) cluster.get :+= i
  }

  /** Returns the output variables. */
  def apply(): HashMap[Long, SetVariable] = output

  override def notifyIntChanges(
    intVariable: IntVariable,
    contextualVarIndex: Int,
    oldVal: Long,
    newVal: Long
  ): Unit = {
    assert(input(contextualVarIndex) == intVariable)

    val oldCluster = output.get(oldVal)
    val newCluster = output.get(newVal)

    if (oldCluster.nonEmpty) oldCluster.get :-= contextualVarIndex
    if (newCluster.nonEmpty) newCluster.get :+= contextualVarIndex
  }

  override def checkInternals(): Unit = {
    for (i <- input.indices) {
      val v = input(i).value()
      if (output.isDefinedAt(v)) {
        require(
          output(v).pendingValue.contains(i),
          s"checkInternal fails in invariant ${name()}. " +
            s"Found a variable that is not in expected cluster. " +
            s"variable: ${input(i)} - index: $i.\n ${internalDescription()}"
        )
      }
    }
    for (value <- output.keys) {
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
      super.toString + s"\n\tinput: ${input.mkString("", ", ", "")}\n\tClusters pending values: |"
    for ((k, cluster) <- output) toReturn += s" $k -> ${cluster.pendingValue} |"

    toReturn
  }
}
