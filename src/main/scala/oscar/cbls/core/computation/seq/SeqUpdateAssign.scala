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

/** Companion object of SeqUpdateAssign.
  */
object SeqUpdateAssign {

  /** Creates a SeqUpdateAssign.
    *
    * @param sequence
    *   The new value assigned to the SeqVariable.
    * @return
    *   A SeqUpdateAssign.
    */
  def apply(sequence: IntSequence): SeqUpdateAssign = new SeqUpdateAssign(sequence)

  /** Extracts the parameter of the given SeqUpdateAssign.
    *
    * @param seqUpdateAssign
    *   The SeqUpdateAssign we want to know the parameters of.
    * @return
    *   The value of the SeqUpdateAssign.
    */
  def unapply(seqUpdateAssign: SeqUpdateAssign): Option[IntSequence] =
    Some(seqUpdateAssign.newSequence)
}

/** A SeqUpdate that assigns a new value to the [[oscar.cbls.algo.sequence.IntSequence]].
  *
  * '''This update is not incremental'''.
  *
  * @param newSequence
  *   The new value assigned to the SeqVariable.
  */
class SeqUpdateAssign(val newSequence: IntSequence) extends SeqUpdate(newSequence) {

  override protected[computation] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = this

  override protected[computation] def regularize(maxPivot: Int): SeqUpdate =
    SeqUpdateAssign(newSequence.regularizeToMaxPivot(maxPivot))

  override def toString: String =
    s"SeqUpdateAssign(new value : $newSequence)"
}
