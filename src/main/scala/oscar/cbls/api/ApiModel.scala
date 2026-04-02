package oscar.cbls.api

import oscar.cbls.Neighborhood
import oscar.cbls.core.computation.Solution
import oscar.cbls.lib.neighborhoods.combinator.PopulationBasedSearch
import oscar.cbls.modeling.search.{Combinator, Neighborhoods}
import oscar.cbls.modeling.{Model, Invariants => Inv}

import scala.collection.immutable.HashMap

/** Companion object of the [[ApiModel]] class. */
object ApiModel {

  /** Declares a [[ApiModel]] with the given name and debug level. */
  def apply(name: String = "<unnamed model>", debugLevel: Int = 0) = new ApiModel(name, debugLevel)
}

/** This class provides the main functionality necessary to express an optimization model: it allows
  * the instantiation of variables, constraints, and objective function. Furthermore, it allows
  * expressing derived quantities (such as the sum over an array of variables) through objects that
  * collect the associated methods, with each object representing a category.
  *
  * @param name
  *   the name of the model
  * @param debugLevel
  *   the debug level of the associated [[oscar.cbls.core.computation.Store]]
  */
class ApiModel(override val name: String, debugLevel: Int = 0) extends Model(name, debugLevel) {

  private val am = this

  object search extends Neighborhoods with Combinator {

    /** This combinator implements a population-based meta-heuristics that maintains a population of
      * solutions and repeatedly (1) diversifies and improves the solutions by applying a set of
      * neighborhoods on each of them (2) selects the best solutions in this population. Upon
      * termination, the best solution in this population is returned, only if it improves over the
      * initial solution.
      *
      * This if a form of restart meta-heuristics that allows devoting little time to low quality
      * solutions while maintaining some diversity throughout the search.
      *
      * This combinator is a long-lasting combinator; ie: it will perform and manage it business and
      * return a loadSolutionMove. It cannot be used on the right hand side of a dynAndThen for
      * instance.
      *
      * This meta-heuristics is described in:
      *
      * [[https://doi.org/10.1016/j.cor.2020.105166 Florian Arnold and Kenneth Sörensen, A progressive filtering heuristic for the location-routing problem and variants, Computers & Operations Research, vol 129, 2021,]]
      *
      * The difference with this reference paper is that this implementation does not feature a
      * crossover operator that would combine two solutions into a new one.
      *
      * Since this combinator is long-lasting, it will perform many local searches in a row and has
      * different verbose modes than the general verbosity defined for combinators. The search
      * procedure used by this meta heuristics are allocated a lower verbosity mode. The verbosity
      * are as follows:
      *   - verbose = 1 will show the evolution of the population and the base search procedure has
      *     verbose = 0
      *   - verbose = 2 will show mode insight on the algorithm: how many identical individuals were
      *     filtered and the base search procedure has verbose = 0
      *   - verbose = 3 also shows the start and end information the search procedure used in the
      *     search. and the base search procedure has verbose = 1
      *
      * @tparam D
      *   the type of the data associated to each individual
      *
      * @param initData
      *   generates data for the initial individual
      * @param step
      *   the definition of the diversification and filtering operations to apply on the population.
      *   <p>
      *
      * It receives:
      *   - the iteration number,
      *   - the data associated to each individual.
      *
      * It returns optionally :
      *   - a function used to generate children of an individual. This function receives the
      *     [[oscar.cbls.core.computation.Solution]] and the data associated to the individual. It
      *     returns whether the individual must be keep in the next generation, a list of
      *     [[Neighborhood]] used to generate children the data associated to each child ;
      *   - how many individual must be kept in the next generation ;
      *     - an optional [[Objective]] that can be used to generate and select children.
      *
      * If it returns None, the meta-heuristics stops and the best current solution is returned if
      * accepted by the objective.
      * @param maxIt
      *   The maximal number of iterations. This is somehow redundant with the previous parameter,
      *   but if you need it, it is there.
      * @param saveAnytimeBest
      *   - If set to true, the meta-heuristics will permanently ensure that the best solution
      *     encountered during the meta-heuristics is saved and reloaded into the population. This
      *     can happen if the neighborhood performs a restart or any form of randomization. This
      *     parameter only makes sense if keepOld is set to false.
      *   - If set to false, this additional mechanism is deactivated.
      * @param filterRedundantElements
      *   - If set to true, the meta-heuristics will identify and filter identical solutions in the
      *     population within each iteration.
      *   - If set to false, this additional mechanism is deactivated. It can then spare time. Logs
      *     are provided in the console about the number of elements in the population that are
      *     filtered out by this mechanism.
      * @param dropIfNoMoveFound
      *   - If set to true, a solution will be dropped if no move is found by a neighborhood.
      *   - If set to false, a solution is kept in the population even if no move is found by the
      *     neighborhood.
      * @param name
      *   a name for this combinator that will be used in logs
      */
    def populationBased[D](
      initData: () => D,
      step: (
        Int,
        List[D]
      ) => Option[((Solution, D) => (Boolean, List[(Neighborhood, D)]), Int, Option[Objective])],
      maxIt: Int = Int.MaxValue,
      saveAnytimeBest: Boolean = false,
      filterRedundantElements: Boolean = true,
      dropIfNoMoveFound: Boolean = false,
      name: String = "PopulationBasedSearch"
    ): PopulationBasedSearch[D] = super.populationBasedSearch(
      initData,
      step,
      maxIt,
      saveAnytimeBest,
      filterRedundantElements,
      dropIfNoMoveFound,
      name
    )(am)

  }

  // ***** Constraints and invariants *****//

  // *** Logic *** //

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
  ): SetVariable = Inv.logic.filter(input, predicate, name, bulkUsed)(this)

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
  ): Array[SetVariable] = Inv.logic.denseCluster(input, upperBound, name, bulkUsed)(this)

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
  ): HashMap[Long, SetVariable] = Inv.logic.sparseCluster(input, clusters, name, bulkUsed)(this)

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
  ): Array[SetVariable] = Inv.logic.denseRef(input, upperBound, name, bulkUsed)(this)

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
  ): IntVariable = Inv.logic.element(input, index, name, bulkUsed)(this)

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
  def elementConst(input: Array[Long], index: IntVariable, name: String = ""): IntVariable =
    Inv.logic.elementConst(input, index, name)(this)

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
  ): SetVariable = Inv.logic.setsElement(input, index, name, bulkUsed)(this)

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
  ): SetVariable = Inv.logic.multiElements(input, indices, name, bulkUsed)(this)

  // *** MinMax *** //

  /** Returns a variable representing the minimum value between the two input values.
    *
    * @param a
    *   The first value on which compute the minimum.
    * @param b
    *   The second value on which compute the minimum.
    */
  def min(a: IntVariable, b: IntVariable): IntVariable = Inv.minMax.min(a, b)(this)

  /** Returns a variable representing the maximum value between the two input values.
    *
    * @param a
    *   The first value on which compute the maximum.
    * @param b
    *   The second value on which compute the maximum.
    */
  def max(a: IntVariable, b: IntVariable): IntVariable = Inv.minMax.max(a, b)(this)

  /** Returns a variable representing the minimum of all the input values.
    *
    * @param input
    *   Array of variables on which to compute the minimum.
    * @param default
    *   The default value of the minimum. By default, it is set to `Int.MaxValue`.
    * @param name
    *   Optional name of the resulting variable.
    * @param bulkUsed
    *   Whether the input variables must be bulked (see
    *   [[oscar.cbls.core.computation.IncredibleBulk]]).
    */
  def min(
    input: Array[IntVariable],
    default: Long = Int.MaxValue,
    name: String = "",
    bulkUsed: Boolean = false
  ): IntVariable = Inv.minMax.min(input, default, name, bulkUsed)(this)

  /** Returns a variable representing the maximum of all the input values.
    *
    * @param input
    *   Array of variables on which to compute the maximum.
    * @param default
    *   The default value of the maximum. By default, it is set to `Int.MinValue`.
    * @param name
    *   Optional name of the resulting variable.
    * @param bulkUsed
    *   Whether the input variables must be bulked (see
    *   [[oscar.cbls.core.computation.IncredibleBulk]]).
    */
  def max(
    input: Array[IntVariable],
    default: Long = Int.MinValue,
    name: String = "",
    bulkUsed: Boolean = false
  ): IntVariable = Inv.minMax.max(input, default, name, bulkUsed)(this)

  /** Returns a variable representing the minimum of the input values such that their indices belong
    * to a given set, i.e., `Min(input(i) | i in indices)` where `input` is an array of integer
    * variables.
    *
    * @param input
    *   Array of variables on which to compute the minimum.
    * @param indices
    *   Variable maintaining the indices of the input array to take into account to calculate the
    *   minimum.
    * @param default
    *   The default value of the minimum. By default, it is set to `Int.MaxValue`.
    * @param name
    *   Optional name of the resulting variable.
    * @param bulkUsed
    *   Whether the input variables must be bulked (see
    *   [[oscar.cbls.core.computation.IncredibleBulk]]).
    */
  def partialMin(
    input: Array[IntVariable],
    indices: SetVariable,
    default: Long = Int.MaxValue,
    name: String = "",
    bulkUsed: Boolean = false
  ): IntVariable = Inv.minMax.partialMin(input, indices, default, name, bulkUsed)(this)

  /** Returns a variable representing the maximum of the input values such that their indices belong
    * to a given set, i.e., `Max(input(i) | i in indices)` where `input` is an array of integer
    * variables.
    *
    * @param input
    *   Array of variables on which to compute the maximum.
    * @param indices
    *   Variable maintaining the indices of the input array to take into account to calculate the
    *   maximum.
    * @param default
    *   The default value of the maximum. By default, it is set to `Int.MinValue`.
    * @param name
    *   Optional name of the resulting variable.
    * @param bulkUsed
    *   Whether the input variables must be bulked (see
    *   [[oscar.cbls.core.computation.IncredibleBulk]]).
    */
  def partialMax(
    input: Array[IntVariable],
    indices: SetVariable,
    default: Long = Int.MinValue,
    name: String = "",
    bulkUsed: Boolean = false
  ): IntVariable = Inv.minMax.partialMax(input, indices, default, name, bulkUsed)(this)

  /** Returns a variable representing the minimum value in a subset of a given array of constant
    * integers, i.e., `Min{input(i) | i in indices}`.
    *
    * @param input
    *   Array of constants on which to compute the minimum.
    * @param indices
    *   Variable maintaining the indices of the input array to take into account to calculate the
    *   minimum.
    * @param default
    *   The default value of the minimum. By default, it is set to `Int.MaxValue`.
    * @param name
    *   Optional name of the resulting variable.
    */
  def minOfConstants(
    input: Array[Long],
    indices: SetVariable,
    default: Long = Int.MaxValue,
    name: String = ""
  ): IntVariable = Inv.minMax.minOfConstants(input, indices, default, name)(this)

  /** Returns a variable representing the maximum value in a subset of a given array of constant
    * integers, i.e., `Max{input(i) | i in indices}`.
    *
    * @param input
    *   Array of constants on which to compute the maximum.
    * @param indices
    *   Variable maintaining the indices of the input array to take into account to calculate the
    *   maximum.
    * @param default
    *   The default value of the maximum. By default, it is set to `Int.MinValue`.
    * @param name
    *   Optional name of the resulting variable.
    */
  def maxOfConstants(
    input: Array[Long],
    indices: SetVariable,
    default: Long = Int.MinValue,
    name: String = ""
  ): IntVariable = Inv.minMax.maxOfConstants(input, indices, default, name)(this)

  /** Returns a variable representing the minimum of the input set.
    *
    * @param input
    *   Set variable on which to compute the minimum.
    * @param default
    *   The default value of the minimum. By default, it is set to `Int.MaxValue`.
    * @param name
    *   Optional name of the resulting variable.
    */
  def minSet(input: SetVariable, default: Long = Int.MaxValue, name: String = ""): IntVariable =
    Inv.minMax.minSet(input, default, name)(this)

  /** Returns a variable that maintain the maximum of the input set.
    *
    * @param input
    *   Set variable on which to compute the maximum.
    * @param default
    *   The default value of the maximum. By default, it is set to `Int.MinValue`.
    * @param name
    *   Optional name of the resulting variable.
    */
  def maxSet(input: SetVariable, default: Long = Int.MinValue, name: String = ""): IntVariable =
    Inv.minMax.maxSet(input, default, name)(this)

  // *** Numeric *** //

  /** Returns a variable that maintains the sum of all variables in the input array.
    *
    * Uses the [[oscar.cbls.lib.invariant.numeric.Sum]] invariant.
    *
    * @param input
    *   Array of variables to sum.
    * @param name
    *   optional name of the resulting variable.
    * @param bulkUsed
    *   Whether the input variables must be bulked (see
    *   [[oscar.cbls.core.computation.IncredibleBulk]]).
    */
  def sum(input: Array[IntVariable], name: String = "", bulkUsed: Boolean = false): IntVariable =
    Inv.numeric.sum(input, name, bulkUsed)(this)

  /** Returns a variable maintaining the sum over the elements of an array such that their indices
    * belong to a given set, i.e., `Sum(input(i) | i in indices)`, where `input` is an array of
    * [[oscar.cbls.core.computation.integer.IntVariable]].
    *
    * Uses the [[oscar.cbls.lib.invariant.numeric.Sum]] invariant.
    *
    * @param input
    *   Array variables to sum.
    * @param indices
    *   Variable maintaining the set of indices of the integers to sum.
    * @param name
    *   Optional name of the resulting variable.
    * @param bulkUsed
    *   Whether the input variables must be bulked (see
    *   [[oscar.cbls.core.computation.IncredibleBulk]]).
    */
  def partialSum(
    input: Array[IntVariable],
    indices: SetVariable,
    name: String = "",
    bulkUsed: Boolean = false
  ): IntVariable = Inv.numeric.partialSum(input, indices, name, bulkUsed)(this)

  /** Returns a variable maintaining the sum over the elements of an array such that their indices
    * belong to a given set, i.e., `Sum(input(i) | i in indices)`, where `input` is an array of
    * constant integers.
    *
    * Uses the [[oscar.cbls.lib.invariant.numeric.SumConst]] invariant.
    *
    * @param input
    *   Array of constant integers to sum.
    * @param indices
    *   Variable maintaining the set of indices of the integers to sum.
    * @param name
    *   Optional name of the resulting variable.
    */
  def partialSumOfConstants(
    input: Array[Long],
    indices: SetVariable,
    name: String = ""
  ): IntVariable = Inv.numeric.partialSumOfConstants(input, indices, name)(this)

  /** Returns a variable that maintains the product of all variables in the input array.
    *
    * Uses the [[oscar.cbls.lib.invariant.numeric.Prod]] invariant.
    *
    * @param input
    *   Array of variables to multiply.
    * @param name
    *   optional name of the resulting variable.
    * @param bulkUsed
    *   Whether the input variables must be bulked (see
    *   [[oscar.cbls.core.computation.IncredibleBulk]]).
    */
  def prod(input: Array[IntVariable], name: String = "", bulkUsed: Boolean = false): IntVariable =
    Inv.numeric.prod(input, name, bulkUsed)(this)

  /** Returns a variable maintaining the product over the elements of an array such that their
    * indices belong to a given set, i.e., `Prod(input(i) | i in indices)`, where `input` is an
    * array of [[oscar.cbls.core.computation.integer.IntVariable]].
    *
    * Uses the [[oscar.cbls.lib.invariant.numeric.Prod]] invariant.
    *
    * @param input
    *   Array variables to sum.
    * @param indices
    *   Variable maintaining the set of indices of the integers to sum.
    * @param name
    *   Optional name of the resulting variable.
    * @param bulkUsed
    *   Whether the input variables must be bulked (see
    *   [[oscar.cbls.core.computation.IncredibleBulk]]).
    */
  def partialProd(
    input: Array[IntVariable],
    indices: SetVariable,
    name: String = "",
    bulkUsed: Boolean = false
  ): IntVariable = Inv.numeric.partialProd(input, indices, name, bulkUsed)(this)

  /** Returns a variable maintaining the product over the elements of an array such that their
    * indices belong to a given set, i.e., `Sum(input(i) | i in indices)`, where `input` is an array
    * of constant integers.
    *
    * Uses the [[oscar.cbls.lib.invariant.numeric.ProdConst]] invariant.
    *
    * @param input
    *   Array of constant integers to sum.
    * @param indices
    *   Variable maintaining the set of indices of the integers to sum.
    * @param name
    *   Optional name of the resulting variable.
    */
  def partialProdOfConstants(
    input: Array[Long],
    indices: SetVariable,
    name: String = ""
  ): IntVariable = Inv.numeric.partialProdOfConstants(input, indices, name)(this)

  /** Returns the absolute value of the input variable. */
  def abs(a: IntVariable, name: String = ""): IntVariable = Inv.numeric.abs(a, name)

  /** Returns the square of this variable. */
  def square(a: IntVariable, name: String = ""): IntVariable = Inv.numeric.square(a, name)

  /** Returns an IntVariable that is the square root of the input variable. */
  def sqrt(a: IntVariable, name: String = ""): IntVariable = Inv.numeric.sqrt(a, name)

  /** Returns a variable maintaining `fun(a, b)`. The used invariant is not incremental. So this
    * method should be used with very simple function.
    *
    * @param a
    *   The first parameter of the function.
    * @param b
    *   The second parameter of the function.
    * @param fun
    *   The function to maintain. It is supposed not to listen to any variable of the model.
    * @param name
    *   Optional name of the resulting variable.
    */
  def intInt2Int(
    a: IntVariable,
    b: IntVariable,
    fun: (Long, Long) => Long,
    name: String = ""
  ): IntVariable = Inv.numeric.intInt2Int(a, b, fun, name)(this)

  /** Returns a variable maintaining `fun(input)`. The used invariant is not incremental. So this
    * method should be used with very simple function.
    *
    * @param input
    *   The listened IntVariable.
    * @param fun
    *   The function to maintain. It is supposed not to listen to any variable in the model.
    * @param cached
    *   Set to true to have a cache of size 1. Set to false to have no cache. A cache can provide
    *   speedup if fun is time-consuming.
    * @param name
    *   Optional name of the resulting variable.
    */
  def int2Int(
    input: IntVariable,
    fun: Long => Long,
    cached: Boolean = false,
    name: String = ""
  ): IntVariable = Inv.numeric.int2Int(input, fun, cached, name)(this)

  // *** Seq *** //

  /** Returns a variable that maintains the content of the input SeqVariable.
    *
    * @param input
    *   The SeqVariable whose content is maintained.
    */
  def content(input: SeqVariable): SetVariable = Inv.seq.content(input)(this)

}
