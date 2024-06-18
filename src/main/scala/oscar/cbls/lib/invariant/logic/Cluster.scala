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

import oscar.cbls.core.computation.{IncredibleBulk, Store}
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.SetVariable

import scala.collection.immutable.HashMap

/** Helper object for [[SparseCluster]] and [[DenseCluster]] invariants */
object Cluster {

  /** Creates a [[SparseCluster]] and instantiates the output [[HashMap]] from a list of values
    * defining the cluster's keys.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   An [[Array]] of [[IntVariable]]
    * @param clusters
    *   The list of keys defining the input values to cluster.
    * @param bulkIdentifier
    *   A [[IncredibleBulk]] is used when several [[oscar.cbls.core.computation.Invariant]] listen
    *   to vars. Warning: [[IncredibleBulk]] are distinguished only by their identifier. Be sure to
    *   use the same one if you're referencing the same variables.
    * @param name
    *   The name (optional) of your Invariant
    */
  def makeSparse(
    model: Store,
    input: Array[IntVariable],
    clusters: Iterable[Long],
    bulkIdentifier: Option[String] = None,
    name: Option[String] = None
  ): SparseCluster = {

    val output: HashMap[Long, SetVariable] =
      HashMap.from(clusters.map(x => (x, SetVariable(model, Set.empty))))

    SparseCluster(model, input, output, bulkIdentifier, name)
  }

  /** Creates a [[DenseCluster]] and instantiates the output [[Array]].
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   An [[Array]] of [[IntVariable]]
    * @param upperBound
    *   The [[Int]] such that the input variables' domain is [0, upperBound[.
    * @param bulkIdentifier
    *   A [[IncredibleBulk]] is used when several [[oscar.cbls.core.computation.Invariant]] listen
    *   to vars. Warning: [[IncredibleBulk]] are distinguished only by their identifier. Be sure to
    *   use the same one if you're referencing the same variables.
    * @param name
    *   The name (optional) of your Invariant
    */
  def makeDense(
    model: Store,
    input: Array[IntVariable],
    upperBound: Int = Int.MaxValue,
    bulkIdentifier: Option[String] = None,
    name: Option[String] = None
  ): DenseCluster = {
    require(upperBound > 0, "upperBound should be > 0.")

    val output: Array[SetVariable] = Array.fill(upperBound)(SetVariable(model, Set.empty))

    DenseCluster(model, input, output, bulkIdentifier, name)
  }

}
