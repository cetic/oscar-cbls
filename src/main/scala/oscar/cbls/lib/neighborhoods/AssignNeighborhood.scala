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

package oscar.cbls.lib.neighborhoods

import oscar.cbls.algo.search.{HotRestart, IdenticalAggregator}
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.Exploration
import oscar.cbls.core.search.loop.{BoundedIterator, LoopBehavior}
import oscar.cbls.core.search.{Move, NoMoveFound, SimpleNeighborhood}

/** Companion object of the [[AssignNeighborhood]] class. */
object AssignNeighborhood {

  /** Creates an AssignNeighborhood that find an [[oscar.cbls.core.computation.integer.IntVariable]]
    * from the input array and a from the variable's domain such that the objective function is
    * improved.
    *
    * @param vars
    *   The variable defining the search space.
    * @param varsDomain
    *   Attribute to each variable a list of possible values.
    * @param name
    *   The name of the neighborhood.
    * @param selectVariableBehavior
    *   How to iterate over the variables.
    * @param selectValueBehavior
    *   How to iterate over the variables' domain.
    * @param searchZone
    *   A subset of indices of `vars` to consider. If `None` is provided, all the variables are
    *   considered
    * @param symmetryClassOfVariable
    *   An optional function that inputs the variables' indices and returns a symmetry class. Only
    *   one of the variables in each class will be considered, making the search faster.
    *   Int.Minvalue is considered different to itself.
    * @param symmetryClassOfValue
    *   An optional function that inputs the variables' indices and returns a symmetry class on
    *   their domains' values. Only one value of each class will be tested. This must be used only
    *   if the model is very expensive to evaluate.
    * @param hotRestart
    *   Set the use of a [[oscar.cbls.algo.search.HotRestart]] mechanism.
    */
  def apply(
    vars: Array[IntVariable],
    varsDomain: IntVariable => Iterable[Long],
    name: String = "AssignNeighborhood",
    selectVariableBehavior: LoopBehavior = LoopBehavior.first(),
    selectValueBehavior: LoopBehavior = LoopBehavior.first(),
    searchZone: Option[Iterable[Int]] = None,
    symmetryClassOfVariable: Option[Int => Int] = None,
    symmetryClassOfValue: Option[Int => Long => Long] = None,
    hotRestart: Boolean = true
  ): AssignNeighborhood = {
    new AssignNeighborhood(
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
}

/** [[oscar.cbls.core.search.Neighborhood]] that find an
  * [[oscar.cbls.core.computation.integer.IntVariable]] from the input array and a from the
  * variable's domain such that the objective function is improved.
  *
  * @param vars
  *   The variable defining the search space.
  * @param varsDomain
  *   Attribute to each variable a list of possible values.
  * @param name
  *   The name of the neighborhood.
  * @param selectVariableBehavior
  *   How to iterate over the variables.
  * @param selectValueBehavior
  *   How to iterate over the variables' domain.
  * @param searchZone
  *   A subset of indices of `vars` to consider. If `None` is provided, all the variables are
  *   considered
  * @param symmetryClassOfVariable
  *   An optional function that inputs the variables' indices and returns a symmetry class. Only one
  *   of the variables in each class will be considered, making the search faster. Int.Minvalue is
  *   considered different to itself.
  * @param symmetryClassOfValue
  *   An optional function that inputs the variables' indices and returns a symmetry class on their
  *   domains' values. Only one value of each class will be tested. This must be used only if the
  *   model is very expensive to evaluate.
  * @param hotRestart
  *   Set the use of a [[oscar.cbls.algo.search.HotRestart]] mechanism.
  */
class AssignNeighborhood(
  vars: Array[IntVariable],
  varsDomain: IntVariable => Iterable[Long],
  name: String = "AssignNeighborhood",
  selectVariableBehavior: LoopBehavior = LoopBehavior.first(),
  selectValueBehavior: LoopBehavior = LoopBehavior.first(),
  searchZone: Option[Iterable[Int]] = None,
  symmetryClassOfVariable: Option[Int => Int] = None,
  symmetryClassOfValue: Option[Int => Long => Long] = None,
  hotRestart: Boolean = true
) extends SimpleNeighborhood[AssignMove](name) {

  private var startIndex: Int = 0

  override protected def exploreNeighborhood(exploration: Exploration[AssignMove]): Unit = {

    // Which indices must be considered
    val iterationZone = searchZone match {
      case None     => vars.indices
      case Some(sz) => sz
    }

    // Activate hot restart if needed
    val iterationSchemeOnZone =
      if (hotRestart) HotRestart(iterationZone, startIndex)
      else iterationZone

    // Remove symmetries from considered variables
    val iterationSchemeOnSymmetryFreeZone = symmetryClassOfVariable match {
      case None => iterationSchemeOnZone
      case Some(s) =>
        IdenticalAggregator.removeIdenticalClassesLazily(
          iterationSchemeOnZone,
          (index: Int) => (s(index), vars(index).value())
        )
    }

    // How to iterate over selected variables
    val (variablesIndicesIterator, stopIndices) =
      selectVariableBehavior.toIterator(iterationSchemeOnSymmetryFreeZone)

    // Iterate on the selected variables
    var currentIndex: Int = 0
    while (variablesIndicesIterator.hasNext) {
      currentIndex = variablesIndicesIterator.next()
      val currentVar: IntVariable = vars(currentIndex)

      // Remove symmetries from currentVar's domain
      val domainIterationScheme = symmetryClassOfValue match {
        case None => varsDomain(currentVar)
        case Some(s) =>
          IdenticalAggregator.removeIdenticalClassesLazily(varsDomain(currentVar), s(currentIndex))
      }

      // How to iterate over the domain
      val (domainIterator, stopDomain) = selectValueBehavior.toIterator(domainIterationScheme)

      // Iterate over the possible values of the current variable
      while (domainIterator.hasNext) {
        val initVal: Long = currentVar.value()
        val newVal: Long  = domainIterator.next()
        currentVar := newVal
        searchProfiler().foreach(x => x.neighborSelected())

        // Check if assigning newVal to current val improves the objective
        exploration.checkNeighborWP(objValue =>
          new AssignMove(currentVar, newVal, objValue, this.name)
        )
        currentVar := initVal
        if (exploration.toReturn != NoMoveFound) {
          stopIndices()
          stopDomain()
        }
      }

    }
    startIndex = currentIndex + 1

  }

  override def doMove(move: AssignMove): Unit = {
    move match {
      case m: AssignMove => m.commit()
      case _ => require(requirement = false, s"Move should be an AssignMove, but get $move")
    }

  }

  override def reset(): Unit = startIndex = 0
}

/** Standard move that assign a [[scala.Long]] value to an
  * [[oscar.cbls.core.computation.integer.IntVariable]].
  *
  * @param variable
  *   The variable to change.
  * @param newValue
  *   The value to assign to `variable`.
  * @param objValueAfter
  *   The objective value of the neighbor. Used for comparison and validation.
  * @param neighborhoodName
  *   The name of the [[oscar.cbls.core.search.Neighborhood]] that generated this Move.
  */
class AssignMove(
  variable: IntVariable,
  newValue: Long,
  objValueAfter: Long,
  override val neighborhoodName: String
) extends Move(objValueAfter, neighborhoodName) {

  override def commit(): Unit = variable := newValue

  override def toString: String = s"AssignMove: ${variable.name()} set to $newValue" +
    s". " + super
    .toString
}
