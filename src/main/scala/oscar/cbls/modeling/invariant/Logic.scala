package oscar.cbls.modeling.invariant

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.{SetConstant, SetVariable}
import oscar.cbls.lib.invariant.logic._
import oscar.cbls.modeling.Model

import scala.collection.immutable.HashMap

trait Logic {

  /** Returns a variable representing the subset of the indices of the input variable array that
    * satisfy the given predicate, i.e., `{i in input.indices | predicate(input(i))}`. The default
    * predicate is whether the given variable is strictly larger than zero.
    *
    * Uses the [[oscar.cbls.lib.invariant.logic.Filter]] invariant.
    *
    * @param input
    *   An array of integer variables.
    * @param predicate
    *   The predicate that elements in the output variables must satisfy.
    * @param name
    *   Optional name of the returned variable.
    * @param bulkUsed
    *   Whether the input variables must be bulked (see
    *   [[oscar.cbls.core.computation.IncredibleBulk]]).
    */
  def filter(
    input: Array[IntVariable],
    predicate: Long => Boolean = _ > 0,
    name: String = "",
    bulkUsed: Boolean = false
  )(implicit m: Model): SetVariable = {
    val store = m.store
    if (input.length == 0) {
      SetConstant(store, Set.empty)
    } else {
      val out = SetVariable(store, Set.empty)
      val optName = name match {
        case "" => None
        case x  => Some(x)
      }
      Filter(store, input, out, predicate, optName, bulkUsed)
      out
    }
  }

  /** Returns an array of variables representing clusters based on the indices of the input
    * variables, i.e., `output(j) = {i in input.indices | input(i) == j}`.<br>
    *
    * Uses the [[oscar.cbls.lib.invariant.logic.DenseCluster]] invariant.
    *
    * @param input
    *   An array of variable to cluster.
    * @param upperBound
    *   The integer such that the input variables' domain is `[0, upperBound[`. By default, it is
    *   set to `Ìnt.MaxValue`.
    * @param name
    *   Optional name of the returned variable.
    * @param bulkUsed
    *   Whether the input variables must be bulked (see
    *   [[oscar.cbls.core.computation.IncredibleBulk]]).
    */
  def denseCluster(
    input: Array[IntVariable],
    upperBound: Int = Int.MaxValue,
    name: String = "",
    bulkUsed: Boolean = false
  )(implicit m: Model): Array[SetVariable] = {

    val optName = name match {
      case "" => None
      case x  => Some(x)
    }

    val inv = Cluster.makeDense(m.store, input, upperBound, optName, bulkUsed)
    inv()
  }

  /** Returns an array of variables representing clusters based on the indices of the input
    * variables, i.e., `output(j) = {i in input.indices | input(i) == j}`.<br>
    *
    * Uses the [[oscar.cbls.lib.invariant.logic.SparseCluster]] invariant.
    *
    * @param input
    *   An array of variable to cluster.
    * @param clusters
    *   The list of keys defining the input values to cluster.
    * @param name
    *   Optional name of the returned variable.
    * @param bulkUsed
    *   Whether the input variables must be bulked (see
    *   [[oscar.cbls.core.computation.IncredibleBulk]]).
    */
  def sparseCluster(
    input: Array[IntVariable],
    clusters: Iterable[Long],
    name: String = "",
    bulkUsed: Boolean = false
  )(implicit m: Model): HashMap[Long, SetVariable] = {

    val optName = name match {
      case "" => None
      case x  => Some(x)
    }

    val inv = Cluster.makeSparse(m.store, input, clusters, optName, bulkUsed)
    inv()
  }

  /** Returns an array of variable maintaining the set of variables containing some values, i.e.,
    * `output(i) = {j | i in input(j)}`.<br>
    *
    * Uses the [[oscar.cbls.lib.invariant.logic.DenseRef]] invariant.
    *
    * @param input
    *   The sets containing the references values.
    * @param upperBound
    *   The integer such that the input values are in `[0, upperBound[`. By default, it is * set to
    *   `Ìnt.MaxValue`.
    * @param name
    *   Optional name of the returned variable.
    * @param bulkUsed
    *   Whether the input variables must be bulked (see
    *   [[oscar.cbls.core.computation.IncredibleBulk]]).
    */
  def denseRef(
    input: Array[SetVariable],
    upperBound: Int = Int.MaxValue,
    name: String = "",
    bulkUsed: Boolean = false
  )(implicit m: Model): Array[SetVariable] = {

    val optName = name match {
      case "" => None
      case x  => Some(x)
    }

    val inv = DenseRef.makeDenseRef(m.store, input, upperBound, optName, bulkUsed)
    inv()
  }

  /** Returns a variable representing the listened element, i.e., `input(index)`, where `input` is
    * an array of integer variables.<br>
    *
    * Uses the [[oscar.cbls.lib.invariant.logic.Element]] invariant.
    *
    * @param input
    *   The elements that can be chosen.
    * @param index
    *   An IntVariable pointing to one of the input values.
    * @param name
    *   Optional name of the returned variable.
    * @param bulkUsed
    *   Whether the input variables must be bulked (see
    *   [[oscar.cbls.core.computation.IncredibleBulk]]).
    */
  def element(
    input: Array[IntVariable],
    index: IntVariable,
    name: String = "",
    bulkUsed: Boolean = false
  )(implicit m: Model): IntVariable = {

    require(
      index.value() < input.length,
      s"Index $index out of bounds of input array of length ${input.length}"
    )

    val optName = name match {
      case "" => None
      case x  => Some(x)
    }
    val output = IntVariable(m.store, 0)
    Element(m.store, input, index, output, optName, bulkUsed)
    output
  }

  /** Returns a variable representing the listened element, i.e., `input(index)`, where `input` is
    * an array of constant integers.<br>
    *
    * Uses the [[oscar.cbls.lib.invariant.logic.Element]] invariant.
    *
    * @param input
    *   The elements that can be chosen.
    * @param index
    *   An IntVariable pointing to one of the input values.
    * @param name
    *   Optional name of the returned variable.
    */
  def elementConst(input: Array[Long], index: IntVariable, name: String = "")(implicit
    m: Model
  ): IntVariable = {

    require(
      index.value() < input.length,
      s"Index $index out of bounds of input array of length ${input.length}"
    )

    val optName = name match {
      case "" => None
      case x  => Some(x)
    }
    val output = IntVariable(m.store, 0)
    ElementConst(m.store, input, index, output, optName)
    output
  }

  /** Returns a variable representing the listened element, i.e., `input(index)`, where `input` is
    * an array of set variable.<br>
    *
    * Uses the [[oscar.cbls.lib.invariant.logic.Element]] invariant.
    *
    * @param input
    *   The elements that can be chosen.
    * @param index
    *   An IntVariable pointing to one of the input values.
    * @param name
    *   Optional name of the returned variable.
    * @param bulkUsed
    *   Whether the input variables must be bulked (see
    *   [[oscar.cbls.core.computation.IncredibleBulk]]).
    */
  def setsElement(
    input: Array[SetVariable],
    index: IntVariable,
    name: String = "",
    bulkUsed: Boolean = false
  )(implicit m: Model): SetVariable = {
    require(
      index.value() < input.length,
      s"Index $index out of bounds of input array of length ${input.length}"
    )

    val optName = name match {
      case "" => None
      case x  => Some(x)
    }
    val output = SetVariable(m.store, Set.empty)
    SetElement(m.store, input, index, output, optName, bulkUsed)
    output
  }

  /** Returns a variable containing the values corresponding to selected indices, i.e.,<br>
    * `{input(i) | i in indices}`
    *
    * @param input
    *   The elements that can be chosen.
    * @param indices
    *   A SetVariable containing the indices of the values to return.
    * @param name
    *   Optional name of the returned variable.
    * @param bulkUsed
    *   Whether the input variables must be bulked (see
    *   [[oscar.cbls.core.computation.IncredibleBulk]]).
    */
  def multiElements(
    input: Array[IntVariable],
    indices: SetVariable,
    name: String = "",
    bulkUsed: Boolean = false
  )(implicit m: Model): SetVariable = {
    val store = m.store
    if (input.isEmpty) SetConstant(store, Set.empty)
    else {

      val optName = name match {
        case "" => None
        case x  => Some(x)
      }

      val output = SetVariable(store, Set.empty)
      MultiElements(store, input, indices, output, optName, bulkUsed)
      output
    }
  }

}
