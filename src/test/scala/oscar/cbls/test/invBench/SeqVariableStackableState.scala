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

package oscar.cbls.test.invBench

import oscar.cbls.algo.sequence.IntSequence

import scala.collection.immutable.HashSet

/** Stackable state of a SeqVariable
  *
  * '''Notes:'''
  *   - The fact that this class is stackable eases the checkpoint management.
  *   - `previousStackableUnrouted` and `previousStackableRouted` are only used to manage checkpoint
  *     with routing problems.
  */
case class SeqVariableStackableState(
  seqSize: Int,
  seqOperationSinceLastCheckpoint: Int,
  previousStackableState: Option[SeqVariableStackableState],
  previousStackableUnrouted: Option[HashSet[Int]] = None,
  previousStackableRouted: Option[IntSequence] = None
) {

  /** Returns the checkpoint level of the [[oscar.cbls.core.computation.seq.SeqVariable]]
    *
    * @return
    * -1 if no checkpoint define. >= 0 otherwise
    */
  def seqCheckpointLevel: Int = {
    previousStackableState match {
      case None                         => -1
      case Some(previousStackableState) => previousStackableState.seqCheckpointLevel + 1
    }
  }

  /** Returns a copy of this SeqVariableStackableState with a new move and eventually a new size */
  def pushOp(newSeqSize: Option[Int] = None): SeqVariableStackableState = {
    SeqVariableStackableState(
      newSeqSize.getOrElse(seqSize),
      if (seqCheckpointLevel == -1) 0 else seqOperationSinceLastCheckpoint + 1,
      previousStackableState,
      previousStackableUnrouted,
      previousStackableRouted
    )
  }
}
