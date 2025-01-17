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

import oscar.cbls.core.search.Neighborhood

/** Companion object of the [[BestSlopeFirst]] class. */
object BestSlopeFirst {

  /** Creates a BestSlopFirst combinator.
    *
    * @param subNeighborhoods
    *   The Neighborhood that this combinator handles. Those can be
    *   [[oscar.cbls.core.search.SimpleNeighborhood]] or other NeighborhoodCombinator.
    * @param tabuLength
    *   The number of iterations a neighborhood has to wait before being explored again.
    * @param overrideTabuOnFullExhaust
    *   If all the neighborhoods had been exhausted, this combinator tries the older tabu
    *   neighborhoods. This parameter must be lesser than `tabuLength` and is used to determine
    *   which tabu neighborhood can ben explored again.
    * @param refreshRate
    *   Each time the number of iteration is a multiple of `refreshRate`, the profiled statistics
    *   are reset.
    * @param neighborhoodCombinatorName
    *   The name of the neighborhood combinator.
    */
  def apply(
    subNeighborhoods: List[Neighborhood],
    tabuLength: Int = 10,
    overrideTabuOnFullExhaust: Int = 9,
    refreshRate: Int = 100,
    neighborhoodCombinatorName: String = "BestSlopFirst"
  ): BestSlopeFirst = new BestSlopeFirst(
    subNeighborhoods,
    tabuLength,
    overrideTabuOnFullExhaust,
    refreshRate,
    neighborhoodCombinatorName
  )
}

/** At each invocation, this combinator explores one of the neighborhoods in `subNeighborhood` (and
  * repeat if it is exhausted). <br>
  *
  * Neighborhoods are selected based on their efficiency slope. The efficiency slope is the total
  * gain in objective function performed by the neighborhood, divided by the total amount of time
  * spend exploring the neighborhood.<br>
  *
  * A tabu is added: in case a neighborhood is exhausted, it is not explored for a number of
  * exploration of this combinator. The tabu can be overridden if all neighborhoods explored are
  * exhausted. Tabu neighborhood can be explored anyway if they are still tabu, but for less than
  * `overrideTabuOnFullExhaust` invocations of this combinator. <br>
  *
  * The `refresh` parameter forces the combinator to try all neighborhoods every `refresh`
  * invocation. It is useful because some neighborhood can perform poorly at the beginning of search
  * and much better later on, and we do not want the combinator to just "stick to its first
  * impression".
  *
  * @param subNeighborhoods
  *   The Neighborhood that this combinator handles. Those can be
  *   [[oscar.cbls.core.search.SimpleNeighborhood]] or other NeighborhoodCombinator.
  * @param tabuLength
  *   The number of iterations a neighborhood has to wait before being explored again.
  * @param overrideTabuOnFullExhaust
  *   If all the neighborhoods had been exhausted, this combinator tries the older tabu
  *   neighborhoods. This parameter must be lesser than `tabuLength` and is used to determine which
  *   tabu neighborhood can ben explored again.
  * @param refreshRate
  *   Each time the number of iteration is a multiple of `refreshRate`, the profiled statistics are
  *   reset.
  * @param neighborhoodCombinatorName
  *   The name of the neighborhood combinator.
  */
class BestSlopeFirst(
  subNeighborhoods: List[Neighborhood],
  tabuLength: Int,
  overrideTabuOnFullExhaust: Int,
  refreshRate: Int,
  neighborhoodCombinatorName: String
) extends BestNeighborhoodFirst(
      subNeighborhoods,
      tabuLength,
      overrideTabuOnFullExhaust,
      refreshRate,
      neighborhoodCombinatorName
    ) {

  override protected def bestKey(neighborhoodId: Int): Long =
    -_selectionProfilerOpt.get.efficiencySlope(neighborhoodId)
}
