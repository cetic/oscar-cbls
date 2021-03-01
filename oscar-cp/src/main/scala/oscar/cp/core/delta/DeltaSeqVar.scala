/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 *******************************************************************************/

package oscar.cp.core.delta

import oscar.algo.reversible.TrailEntry
import oscar.cp.core._
import oscar.cp.core.variables._

class DeltaSeqVarTrailEntry(delta: DeltaSeqVar, oldSizeAppended: Int, oldSizePossible: Int, oldSizeRequired: Int) extends TrailEntry {
  final override def restore(): Unit = delta.restore(oldSizeAppended, oldSizePossible, oldSizeRequired)
}

abstract class DeltaSeqVar(seq: CPSeqVar, idx: Int) extends Delta {

  def id: Int = idx

  protected val store: CPStore = seq.store
  protected var _oldSizeAppended: Int = seq.length

  // Used to trail changes in the delta
  protected var lastMagic: Long = -1L

  def oldSizeMember: Int = _oldSizeAppended

  def oldSizeRequired: Int

  def oldSizePossible: Int

  protected def trail(): Unit

  def restore(oldSizeAppended: Int, oldSizePossible: Int, oldSizeRequired: Int): Unit

  def changed(): Boolean

  def membersChanged(): Boolean = seq.membersChanged(this)

  def possibleChanged(): Boolean

  def requiredChanged(): Boolean

  def deltaMembersSize(): Int = seq.deltaMembersSize(this)

  def deltaPossibleSize(): Int

  def deltaRequiredSize(): Int

  def deltaMembers(): Iterator[Int] = seq.deltaMembers(this)

  def deltaPossible(): Iterator[Int]

  def deltaRequired(): Iterator[Int]
}

class DeltaSetBasedSeqVar(seq: CPSeqVar, val deltaSet: DeltaSetVar, idx: Int) extends DeltaSeqVar(seq, idx) {

  def oldSizeRequired: Int = deltaSet.oldSizeRequired

  def oldSizePossible: Int = deltaSet.oldSizePossible

  final def trail(): Unit = {
    val contextMagic = store.magic
    if (lastMagic != contextMagic) {
      lastMagic = contextMagic
      store.trail(new DeltaSeqVarTrailEntry(this, oldSizeMember, deltaSet.oldSizePossible, deltaSet.oldSizeRequired))
    }
  }

  final def restore(oldSizeAppended: Int, oldSizePossible: Int, oldSizeRequired: Int): Unit = {
    _oldSizeAppended = oldSizeAppended
    deltaSet.restore(oldSizePossible, oldSizeRequired)
  }


  final override def update(): Unit = {
    val seqvs = seq.length
    if (seqvs != _oldSizeAppended) trail()
    _oldSizeAppended = seq.length
    deltaSet.update()
  }

  def changed(): Boolean = seq.changed(this) || deltaSet.changed()

  def possibleChanged(): Boolean = deltaSet.possibleChanged()

  def requiredChanged(): Boolean = deltaSet.requiredChanged()

  def deltaPossibleSize(): Int = deltaSet.deltaPossibleSize()

  def deltaRequiredSize(): Int = deltaSet.deltaRequiredSize()

  def deltaPossible(): Iterator[Int] = deltaSet.deltaPossible()

  def deltaRequired(): Iterator[Int] = deltaSet.deltaRequired()
}

object DeltaSeqVar{
  def apply(seq: CPHeadSeqVar, deltaSet: DeltaSetVar, idx: Int): DeltaSeqVar = new DeltaSetBasedSeqVar(seq, deltaSet, idx)

  def apply(seq: CPInsertSeqVar, deltaSet: DeltaSetVar, idx: Int): DeltaSeqVar = new DeltaSetBasedSeqVar(seq, deltaSet, idx)
}
