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

/** Move which moves a value from a [[oscar.cbls.core.computation.seq.SeqVariable]] to another point
  * in this sequence.
  *
  * @param seq
  *   The sequence into which the new value.
  * @param nodeToMoveExplorer
  *   The value to move.
  * @param afterPointExplorer
  *   The value or the position after which move the value.
  * @param positionIndependentMove
  *   If set to `true`, `insertAfterPoint` is supposed to be a '''value contained by the'''
  *   '''sequence'''. Else, `insertAfterPoint` is supposed to be a '''position''' in the sequence.
  * @param objValueAfter
  *   The objective value of the neighbor. Used for comparison and validation.
  * @param neighborhoodName
  *   The name of the [[oscar.cbls.core.search.Neighborhood]] that generated this Move.
  */
class OnePointMoveMove(
  seq: SeqVariable,
  nodeToMoveExplorer: IntSequenceExplorer,
  afterPointExplorer: IntSequenceExplorer,
  positionIndependentMove: Boolean,
  objValueAfter: Long,
  override val neighborhoodName: String
) extends Move(objValueAfter, neighborhoodName) {

  override def commit(): Unit = {
    seq.move(nodeToMoveExplorer, nodeToMoveExplorer, afterPointExplorer, flip = false)
  }

  override def toString: String = {
    s"OnePointMoveMove: Move node ${nodeToMoveExplorer.value} after " +
      s"${if (positionIndependentMove) s"node ${afterPointExplorer.value}"
        else s"position ${afterPointExplorer.position}"} in sequence\n $seq" + super.toString
  }
}
