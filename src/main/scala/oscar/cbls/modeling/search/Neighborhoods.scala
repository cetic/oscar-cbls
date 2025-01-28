package oscar.cbls.modeling.search

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.lib.neighborhoods.AssignNeighborhood

/** This trait collects methods used to instantiate basic neighborhoods to construct a search
  * procedure.
  */
trait Neighborhoods {

  /** Instantiates a neighborhood where the move consists in changing the value of one of its input
    * variables within the given variable domain in order to improve the objective.
    *
    * @param vars
    *   The variables defining the search space.
    * @param varsDomain
    *   Attributes to each variable a list of possible values depending on its value and its index.
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
    *   an instance of a [[oscar.cbls.lib.neighborhoods.AssignNeighborhood]]
    */
  def assign(
    vars: Array[IntVariable],
    varsDomain: (IntVariable, Int) => Iterable[Long],
    name: String = "AssignNeighborhood",
    selectVariableBehavior: LoopBehavior = LoopBehavior.first(),
    selectValueBehavior: LoopBehavior = LoopBehavior.first(),
    searchZone: Option[() => Iterable[Int]] = None,
    symmetryClassOfVariable: Option[Int => Int] = None,
    symmetryClassOfValue: Option[Int => Long => Long] = None,
    hotRestart: Boolean = true
  ): AssignNeighborhood =
    AssignNeighborhood(
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
}
