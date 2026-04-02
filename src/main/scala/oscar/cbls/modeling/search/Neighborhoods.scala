package oscar.cbls.modeling.search

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.lib.neighborhoods.{Assign, DoIt, Swap}
import oscar.cbls.modeling.Neighborhoods.combinator.{acceptAll, maxMoves}
import oscar.cbls.{Model, Neighborhood}

/** This trait collects methods used to instantiate basic neighborhoods to construct a search
  * procedure.
  */
trait Neighborhoods {

  /** Instantiates an [[oscar.cbls.lib.neighborhoods.Assign]] that consists in assigning a new value
    * to one of its input variables with respect of its domain.
    *
    * @note
    *   WARNING: by default, the domain specified at variable initialization is used for the
    *   `varsDomain` parameter, and ALL integers in this domain are explored, which may be an issue
    *   when using very large domains. When encountering performance issues, consider defining a
    *   custom search domain.
    *
    * @param vars
    *   The variables defining the search space.
    * @param varsDomain
    *   Attributes to each variable a collection of possible values depending on its value and its
    *   index.
    * @param name
    *   The name of the neighborhood.
    * @param selectVariableBehavior
    *   How to iterate over the variables.
    * @param selectValueBehavior
    *   How to iterate over the variables' domain.
    * @param searchZone
    *   A subset of indices of the variables to consider. If None is provided, are considered
    * @param symmetryClassOfVariable
    *   An optional function that inputs the variables' indices and returns a symmetry class. Only
    *   one of the variables in each class will be considered, making the search faster. Elements in
    *   the class denoted by Int.Minvalue are always considered.
    * @param symmetryClassOfValue
    *   An optional function that inputs the variables' indices and returns a symmetry class on
    *   their domains' values. Only one value of each class will be tested. This must be used only
    *   if the model is very expensive to evaluate.
    * @param hotRestart
    *   Set the use of a [[oscar.cbls.algo.search.HotRestart]] mechanism.
    * @return
    *   an instance of a [[oscar.cbls.lib.neighborhoods.Assign]]
    */
  def assign(
    vars: Array[IntVariable],
    varsDomain: (IntVariable, Int) => Iterable[Long] = (x, _) => x.iterableDomain,
    name: String = "Assign",
    selectVariableBehavior: LoopBehavior = LoopBehavior.first(),
    selectValueBehavior: LoopBehavior = LoopBehavior.first(),
    searchZone: Option[() => Iterable[Int]] = None,
    symmetryClassOfVariable: Option[Int => Int] = None,
    symmetryClassOfValue: Option[Int => Long => Long] = None,
    hotRestart: Boolean = true
  ): Assign =
    Assign(
      vars,
      varsDomain,
      name,
      selectVariableBehavior,
      selectValueBehavior,
      searchZone,
      symmetryClassOfVariable,
      symmetryClassOfValue,
      hotRestart
    )

  /** Instantiates a DoIt that returns a move that performs a given unit function.
    *
    * @param model
    *   Model attached to the search.
    * @param doIt
    *   The method to performs when commiting a move.
    * @param name
    *   The name of the Neighborhood.
    */
  def doIt(doIt: () => Unit, name: String = "DoIt")(implicit model: Model): DoIt =
    DoIt(model.store, doIt, neighborhoodName = name)

  /** Instantiate a neighborhood that does not change the solution. Can be used in
    * [[oscar.cbls.lib.neighborhoods.combinator.PopulationBasedSearch]] to allows an individual to
    * skip a generation.
    *
    * @param name
    *   the name of the neighborhood
    * @param model
    *   model attached to the search
    * @return
    *   a neighborhood that does nothing
    */
  def doNothing(name: String = "DoNothing")(implicit model: Model): Neighborhood =
    maxMoves(acceptAll(doIt(() => {}, name)(model)), 1)

  /** Instantiates a [[oscar.cbls.lib.neighborhoods.Swap]] that exchanges the values of two
    * [[oscar.cbls.core.computation.integer.IntVariable]].
    *
    * @note
    *   WARNING: This neighborhood does not check whether the provided variables have all the same
    *   domain.
    * @param vars
    *   The variables defining the search space.
    * @param name
    *   The name of the neighborhood.
    * @param selectFirstVariableBehavior
    *   How to iterate to select the first variable to swap.
    * @param selectSecondVariableBehavior
    *   How to iterate to select the second variable to swap.
    * @param firstSearchZone
    *   A subset of indices of `vars` to consider for the first variable of the Swap operation. If
    *   `None` is provided, all the variables are considered.
    * @param secondSearchZone
    *   A subset of indices of `vars` to consider for the second variable of the Swap operation.
    *   These indices are computed from the index of the first variable and its pending value. If
    *   `None` is provided, all the variables are considered.
    * @param symmetryCanBeBrokenOnIndices
    *   If `false`, ensure that the first variable of a swap has always a smaller index than the
    *   second one. For `i < j`, the exploration only tries to swap `i` with `j` and avoids to swap
    *   `j` with `i`. Must always be `false` except if two different search zones are used.
    * @param symmetryCanBeBrokenOnValues
    *   If `false`, ensure that the first variable of a swap has always a smaller value than the
    *   second one. `symmetryCannotBeBrokenOnValues` and `symmetryCannotBeBrokenOnIndices` cannot be
    *   `false` at the same time.
    * @param symmetryClassOfFirstVariable
    *   An optional function that inputs the variables' indices and returns a symmetry class. Only
    *   one of the variables in each class will be considered, making the search faster.
    *   Int.Minvalue is considered different to itself. Used for the first variable of the swap.
    * @param symmetryClassOsSecondVariable
    *   An optional function that inputs the variables' indices and returns a symmetry class. Only
    *   one of the variables in each class will be considered, making the search faster.
    *   Int.Minvalue is considered different to itself. Used for the second variable of the swap.
    * @param hotRestart
    *   Sets the use of a [[oscar.cbls.algo.search.HotRestart]] mechanism.
    */
  def swap(
    vars: Array[IntVariable],
    name: String = "Swap",
    selectFirstVariableBehavior: LoopBehavior = LoopBehavior.first(),
    selectSecondVariableBehavior: LoopBehavior = LoopBehavior.first(),
    firstSearchZone: Option[() => Iterable[Int]] = None,
    secondSearchZone: Option[(Int, Long) => Iterable[Int]] = None,
    symmetryCanBeBrokenOnIndices: Boolean = false,
    symmetryCanBeBrokenOnValues: Boolean = true,
    symmetryClassOfFirstVariable: Option[Int => Int] = None,
    symmetryClassOsSecondVariable: Option[Int => Int] = None,
    hotRestart: Boolean = true
  ): Swap =
    Swap(
      vars,
      name,
      selectFirstVariableBehavior,
      selectSecondVariableBehavior,
      firstSearchZone,
      secondSearchZone,
      symmetryCanBeBrokenOnIndices,
      symmetryCanBeBrokenOnValues,
      symmetryClassOfFirstVariable,
      symmetryClassOsSecondVariable,
      hotRestart
    )
}
