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

package oscar.cbls.lib.neighborhoods.combinator

import oscar.cbls.core.computation.objective.Objective
import oscar.cbls.core.search.{Move, Neighborhood}

/** Object that instantiates [[EjectionChains]] with a first layer of exploration used for
  * backtracking.
  *
  * @note
  *   The ejections chains are not designed to be used as a left neighborhood of a [[DynAndThen]]
  *   combinator. However, you can achieve the same combined neighborhood by putting you right
  *   neighborhood as the last neighborhood of your ejection chain.
  */
object BackTrackedEjectionChains {

  /** Returns a neighborhood that performs a first exploration of neighborhood and then, based on
    * the result, explore an [[EjectionChains]]. The neighborhood is instantiated as a
    * [[DynAndThen]] between a first neighborhood and an ejection chain.
    *
    * @param firstNeighborhood
    *   The neighborhood defining the first layer of the exploration.
    * @param nextNeighborhood
    *   Function that, given the move selected from the first layer, returns a function used by the
    *   ejection chain to generate sub-neighborhood to explore.
    * @param relaxedObjective
    *   A relaxed objective that can be used to ease the exploration of the sub-neighborhoods.
    * @param relaxedAcceptanceCriterion
    *   A function that, given the initial objective value and a new objective value, returns if the
    *   move can be accepted.
    * @param stopAtFirstImprovement
    *   Whether the exploration can stop after the first improvement of the objective value.
    * @param name
    *   The name of the neighborhood.
    */
  def apply(
    firstNeighborhood: Neighborhood,
    nextNeighborhood: Move => List[Move] => Option[Neighborhood],
    relaxedObjective: Option[Objective] = None,
    relaxedAcceptanceCriterion: Option[(Long, Long) => Boolean] = None,
    stopAtFirstImprovement: Boolean = false,
    name: String = "Ejection Chains"
  ): Neighborhood = {
    DynAndThen(
      firstNeighborhood,
      (initMove: Move) => {
        EjectionChains(
          nextNeighborhood(initMove),
          relaxedObjective,
          relaxedAcceptanceCriterion,
          stopAtFirstImprovement,
          name
        )
      }
    )
  }

  /** Returns a neighborhood that performs a first exploration of neighborhood and then, based on
    * the result, explore an [[EjectionChains]]. The ejection uses an accumulator to generates its
    * sub-neighborhoods.
    *
    * @param initAcc
    *   Initial value of the accumulator used to generate the next neighborhood in the ejection
    *   chain.
    * @param nextNeighborhood
    *   Function that, given the previous committed moves and an accumulator, returns the next
    *   neighborhood to explore.
    * @param relaxedObjective
    *   A relaxed objective that can be used to ease the exploration of the sub-neighborhoods.
    * @param relaxedAcceptanceCriterion
    *   A function that, given the initial objective value and a new objective value, returns if the
    *   move can be accepted.
    * @param stopAtFirstImprovement
    *   Whether the exploration can stop after the first improvement of the objective value.
    * @param name
    *   The name of the neighborhood combinator.
    * @tparam T
    *   The type of the accumulator.
    */
  def fold[T](initAcc: T)(
    firstNeighborhood: Neighborhood,
    nextNeighborhood: Move => (T, List[Move]) => Option[(T, Neighborhood)],
    relaxedObjective: Option[Objective] = None,
    relaxedAcceptanceCriterion: Option[(Long, Long) => Boolean] = None,
    stopAtFirstImprovement: Boolean = false,
    name: String = "Ejection Chains"
  ): Neighborhood = {
    DynAndThen(
      firstNeighborhood,
      (initMove: Move) => {
        EjectionChains.fold(initAcc)(
          nextNeighborhood(initMove),
          relaxedObjective,
          relaxedAcceptanceCriterion,
          stopAtFirstImprovement,
          name
        )
      }
    )
  }

}
