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

package oscar.cbls.core.computation.seq

import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.SavedValue
import oscar.cbls.core.distributed.computation.{StoreIndependentSavedValue, StoreIndependentSeqSavedValue}

/** A saved state of a [[SeqVariable]].
  *
  * @param seqVariable
  *   The SeqVariable whose state is saved.
  * @param savedValue
  *   the saved value
  */
case class SeqSavedValue(seqVariable: SeqVariable, savedValue: IntSequence)
    extends SavedValue(seqVariable) {

  override def compare(that: SavedValue): Int = {
    that match {
      case i: SeqSavedValue if this.seqVariable == i.seqVariable =>
        savedValue compare i.savedValue
      case _ => throw new Error("cannot compare saved values from different variables")
    }
  }

  /** Restores the variable current value to the saved one */
  override def restoreValue(): Unit = {
    if (!seqVariable.isConstant) {
      while (seqVariable.topCheckpointLevel > -1) {
        seqVariable.rollbackToTopCheckpoint()
        seqVariable.releaseTopCheckpoint()
      }

      seqVariable.setValue(savedValue)
    }
  }

  override def makeStoreIndependent: StoreIndependentSavedValue =
    StoreIndependentSeqSavedValue(seqVariable.id, savedValue.toList.toArray)
}
