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

/** Move that changes the position a segment within a sequence represented by a
  * [[oscar.cbls.core.computation.seq.SeqVariable]].
  *
  * @param seq
  *   The sequence from which segments can be moved.
  * @param startExplorer
  *   Explorer associated to the start of the segment to move.
  * @param endExplorer
  *   Explorer associated to the end of the segment to move.
  * @param afterPointExplorer
  *   Explorer associated to the insertion point of the move.
  * @param flip
  *   If the moved segment must be flipped or not.
  * @param objValueAfter
  *   The objective value of the neighbor. Used for comparison and validation.
  * @param neighborhoodName
  *   The name of the [[oscar.cbls.core.search.Neighborhood]] that generated this Move.
  */
case class ThreeOptMove(
  seq: SeqVariable,
  startExplorer: IntSequenceExplorer,
  endExplorer: IntSequenceExplorer,
  afterPointExplorer: IntSequenceExplorer,
  flip: Boolean,
  override val objValueAfter: Long,
  override val neighborhoodName: String
) extends Move(objValueAfter, neighborhoodName) {

  override def commit(): Unit = seq.move(startExplorer, endExplorer, afterPointExplorer, flip)

  override def toString: String = {
    super.toString + s"\n Move segment ${startExplorer.value} -> ... -> ${endExplorer.value} " +
      s"after node ${afterPointExplorer.value}${if (flip) "," else ", not"} reversed: \n $seq"
  }
}
