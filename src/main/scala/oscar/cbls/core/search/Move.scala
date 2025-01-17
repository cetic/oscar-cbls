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

package oscar.cbls.core.search

/** A Move represents the modifications made to the model to reach a specific new neighbor.
  *
  * Each [[oscar.cbls.core.search.Neighborhood]] has to implement it's own Move by extending this
  * abstract class.
  *
  * When a Neighborhood selects a new neighbor to explore, it generates a Move and gives it to a
  * concrete [[oscar.cbls.core.computation.objective.Objective]] for validation. Then the
  * Neighborhood roll-back it's modification of the model. If the selected neighbor is accepted,
  * OscaR.cbls uses the Move to re-apply the modifications to the model and commit them.
  * @param objValueAfter
  *   The objective value of the neighbor. Used for comparison and validation.
  * @param neighborhoodName
  *   The name of the [[oscar.cbls.core.search.Neighborhood]] that generated this Move.
  */
abstract class Move(val objValueAfter: Long, val neighborhoodName: String) {

  /** Applies the modifications of this Move. */
  def commit(): Unit

  /** Returns the new objective value when applying this Move. */
  def objAfter(): Long = objValueAfter

  override def toString: String = s" objValue after $objValueAfter"

  /** Method that can be overridden when some of the variable of the Move has to be regularized when
    * using composite moves (namely moves working with
    * [[oscar.cbls.algo.sequence.IntSequenceExplorer]]).
    */
  def regularize(): Move = this

}
