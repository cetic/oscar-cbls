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

package oscar.cbls.lib.neighborhoods.routing

import oscar.cbls.algo.sequence.IntSequenceExplorer
import oscar.cbls.core.computation.seq.SeqVariable
import oscar.cbls.core.search.Move

/** Move that removes a value from a [[oscar.cbls.core.computation.seq.SeqVariable]].
  *
  * @param seq
  *   The sequence from which values can be removed
  * @param removedPointExplorer
  *   Explorer associated to the node to remove
  * @param objValueAfter
  *   The objective value of the neighbor. Used for comparison and validation
  * @param neighborhoodName
  *   The name of the [[oscar.cbls.core.search.Neighborhood]] that generated this Move
  */
case class RemovePointMove(
  seq: SeqVariable,
  removedPointExplorer: IntSequenceExplorer,
  override val objValueAfter: Long,
  override val neighborhoodName: String
) extends Move(objValueAfter, neighborhoodName) {

  override def commit(): Unit = seq.remove(removedPointExplorer)

  override def toString: String = s"RemovePointMove: remove node ${removedPointExplorer.value}. "

  override def regularize(): Move = {
    val reguRemovedPointExplorer =
      seq.pendingValue.explorerAtAnyOccurrence(removedPointExplorer.value).get

    RemovePointMove(seq, reguRemovedPointExplorer, objValueAfter, neighborhoodName)
  }
}
