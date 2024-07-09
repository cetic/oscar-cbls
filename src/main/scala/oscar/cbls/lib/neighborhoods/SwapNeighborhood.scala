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
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.core.search.{Move, NoMoveFound, SimpleNeighborhood}

/** Companion object of the [[SwapNeighborhood]] class. */
object SwapNeighborhood {

  /** Creates a SwapNeighborhood that exchanges the values of two
    * [[oscar.cbls.core.computation.integer.IntVariable]].
    *
    * @param vars
    *   The variables defining the search space.
    * @param name
    *   The name of the neighborhood.
    * @param selectFirstVariableBehavior
    *   How to iterate to select the first variable to swap.
    * @param selectSecondVariableBehavior
    *   How to iterate to select the second variable to swap.
    * @param firstSearchZone
    *   A subset of indices of `vars` to consider as the first variable of the Swap operation. If
    *   `None` is provided, all the variables are considered.
    * @param secondSearchZone
    *   A subset of indices of `vars` to consider as the second variable of the Swap operation. This
    *   indices are computed from the index of the first variable and its old value. If `None` is
    *   provided, all the variables are considered.
    * @param symmetryCanBeBrokenOnIndices
    *   If `false`, ensure that the first variable of a swap has always a smaller index than the
    *   second one. Must always be `false` except if two different search zones are used.
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
    *   Set the use of a [[oscar.cbls.algo.search.HotRestart]] mechanism.
    */
  def apply(
    vars: Array[IntVariable],
    name: String = "SwapNeighborhood",
    selectFirstVariableBehavior: LoopBehavior = LoopBehavior.first(),
    selectSecondVariableBehavior: LoopBehavior = LoopBehavior.first(),
    firstSearchZone: Option[() => Iterable[Int]] = None,
    secondSearchZone: Option[(Int, Long) => Iterable[Int]] = None,
    symmetryCanBeBrokenOnIndices: Boolean = false,
    symmetryCanBeBrokenOnValues: Boolean = true,
    symmetryClassOfFirstVariable: Option[Int => Int] = None,
    symmetryClassOsSecondVariable: Option[Int => Int] = None,
    hotRestart: Boolean = true
  ): SwapNeighborhood = {
    new SwapNeighborhood(
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
}

/** [[oscar.cbls.core.search.Neighborhood]] that find two
  * [[oscar.cbls.core.computation.integer.IntVariable]] from an input value and swap their values
  *
  * @param vars
  *   The variables defining the search space.
  * @param name
  *   The name of the neighborhood.
  * @param selectFirstVariableBehavior
  *   How to iterate to select the first variable to swap.
  * @param selectSecondVariableBehavior
  *   How to iterate to select the second variable to swap.
  * @param firstSearchZone
  *   A subset of indices of `vars` to consider as the first variable of the Swap operation. If
  *   `None` is provided, all the variables are considered.
  * @param secondSearchZone
  *   A subset of indices of `vars` to consider as the second variable of the Swap operation. This
  *   indices are computed from the index of the first variable and its old value. If `None` is
  *   provided, all the variables are considered.
  * @param symmetryCanBeBrokenOnIndices
  *   If `false`, ensure that the first variable of a swap has always a smaller index than the
  *   second one. Must always be `false` except if two different search zones are used.
  * @param symmetryCanBeBrokenOnValues
  *   If `false`, ensure that the first variable of a swap has always a smaller value than the
  *   second one. `symmetryCannotBeBrokenOnValues` and `symmetryCannotBeBrokenOnIndices` cannot be
  *   `false` at the same time.
  * @param symmetryClassOfFirstVariable
  *   An optional function that inputs the variables' indices and returns a symmetry class. Only one
  *   of the variables in each class will be considered, making the search faster. Int.Minvalue is
  *   considered different to itself. Used for the first variable of the swap.
  * @param symmetryClassOsSecondVariable
  *   An optional function that inputs the variables' indices and returns a symmetry class. Only one
  *   of the variables in each class will be considered, making the search faster. Int.Minvalue is
  *   considered different to itself. Used for the second variable of the swap.
  * @param hotRestart
  *   Set the use of a [[oscar.cbls.algo.search.HotRestart]] mechanism.
  */
class SwapNeighborhood(
  vars: Array[IntVariable],
  name: String = "SwapNeighborhood",
  selectFirstVariableBehavior: LoopBehavior = LoopBehavior.first(),
  selectSecondVariableBehavior: LoopBehavior = LoopBehavior.first(),
  firstSearchZone: Option[() => Iterable[Int]] = None,
  secondSearchZone: Option[(Int, Long) => Iterable[Int]] = None,
  symmetryCanBeBrokenOnIndices: Boolean = false,
  symmetryCanBeBrokenOnValues: Boolean = true,
  symmetryClassOfFirstVariable: Option[Int => Int] = None,
  symmetryClassOsSecondVariable: Option[Int => Int] = None,
  hotRestart: Boolean = true
) extends SimpleNeighborhood[SwapMove](name) {

  require(
    symmetryCanBeBrokenOnIndices || symmetryCanBeBrokenOnValues,
    "symmetryCannotBeBrokenOnValues and symmetryCannotBeBrokenOnIndices cannot be false at the " +
      "same time."
  )

  private[this] var indexOfFirstVariable = 0

  override protected def exploreNeighborhood(exploration: Exploration[SwapMove]): Unit = {

    // Which indices must be considered for the first variable
    val firstIterationZone = firstSearchZone match {
      case None     => vars.indices
      case Some(sz) => sz()
    }

    // Activate hot restart if needed
    val firstIterationSchemeOnZone =
      if (hotRestart) HotRestart(firstIterationZone, indexOfFirstVariable)
      else firstIterationZone

    // Remove symmetries from considered variables
    val firstIterationSchemeOnSymmetryFreeZone = symmetryClassOfFirstVariable match {
      case None => firstIterationSchemeOnZone
      case Some(s) =>
        IdenticalAggregator.removeIdenticalClassesLazily(firstIterationSchemeOnZone, s)
    }

    // How to iterate over the first variable
    val (firstIterator, stopFirst) =
      selectFirstVariableBehavior.toIterator(firstIterationSchemeOnSymmetryFreeZone)

    while (firstIterator.hasNext) {
      indexOfFirstVariable = firstIterator.next()
      val firstVar: IntVariable = vars(indexOfFirstVariable)
      val firstOldVal: Long     = firstVar.value()

      // Which indices must be considered for the second variable
      val secondIterationZone = secondSearchZone match {
        case None     => vars.indices
        case Some(sz) => sz(indexOfFirstVariable, firstOldVal)
      }

      // Remove symmetries from considered variables
      val secondIterationSchemeOnSymmetryFreeZone = symmetryClassOsSecondVariable match {
        case None    => secondIterationZone
        case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(secondIterationZone, s)
      }

      // How to iterate over the second variable
      val (secondIterator, stopSecond) =
        selectSecondVariableBehavior.toIterator(secondIterationSchemeOnSymmetryFreeZone)

      while (secondIterator.hasNext) {
        val secondIndex: Int       = secondIterator.next()
        val secondVar: IntVariable = vars(secondIndex)
        val secondOldVal: Long     = secondVar.value()

        if (
          // Can we make a swap with a previous variable?
          (symmetryCanBeBrokenOnIndices || indexOfFirstVariable < secondIndex)
          // The swapped variables must be different
          && indexOfFirstVariable != secondIndex
          // Do we have to keep the order of the values to perform the swap?
          && (symmetryCanBeBrokenOnValues || firstOldVal < secondOldVal)
          // The two variable must have different values
          && firstOldVal != secondOldVal
        ) {
          firstVar  := secondOldVal
          secondVar := firstOldVal
          searchProfiler().foreach(x => x.neighborSelected())

          exploration.checkNeighborWP(objValue =>
            new SwapMove(firstVar, secondVar, objValue, this.name)
          )
          firstVar  := firstOldVal
          secondVar := secondOldVal

          if (exploration.toReturn != NoMoveFound) {
            stopFirst()
            stopSecond()
          }
        }
      }
    }
    indexOfFirstVariable += 1
  }

  override def doMove(move: SwapMove): Unit = move.commit()

  override def reset(): Unit = indexOfFirstVariable = 0

}

/** [[oscar.cbls.core.search.Move]] that exchanges the values of two
  * [[oscar.cbls.core.computation.integer.IntVariable]].
  *
  * @param first
  *   The first variable to swap.
  * @param second
  *   The second variable to swap.
  * @param objValueAfter
  *   The objective value of the neighbor. Used for comparison and validation.
  * @param neighborhoodName
  *   The name of the [[oscar.cbls.core.search.Neighborhood]] that generated this Move.
  */
class SwapMove(
  first: IntVariable,
  second: IntVariable,
  objValueAfter: Long,
  override val
  neighborhoodName: String
) extends Move(objValueAfter, neighborhoodName) {

  override def commit(): Unit = {
    val firstOldVal: Long = first.value()
    first  := second.value()
    second := firstOldVal
  }

  override def toString: String =
    s"SwapMove: ${first.name()} swapped with ${second.name()}. " + super.toString
}
