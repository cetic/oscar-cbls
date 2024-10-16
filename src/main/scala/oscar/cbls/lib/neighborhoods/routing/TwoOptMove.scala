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

/** Move that flips a subsequence in a [[oscar.cbls.core.computation.seq.SeqVariable]].
  *
  * @param seq
  *   The sequence on which perform a flip.
  * @param fromExplorer
  *   Explorer associated to the start of the segment to flip.
  * @param toExplorer
  *   Explorer associated to the end of the segment to flip.
  * @param objValueAfter
  *   The objective value of the neighbor. Used for comparison and validation.
  * @param neighborhoodName
  *   The name of the [[oscar.cbls.core.search.Neighborhood]] that generated this Move.
  */
class TwoOptMove(
  seq: SeqVariable,
  fromExplorer: IntSequenceExplorer,
  toExplorer: IntSequenceExplorer,
  objValueAfter: Long,
  override val neighborhoodName: String
) extends Move(objValueAfter, neighborhoodName) {

  override def commit(): Unit = seq.flip(fromExplorer, toExplorer)

  override def toString: String =
    super.toString + s" flips segment ${fromExplorer.value} -> ... -> ${toExplorer.value} " +
      s"in sequence $seq"
}
