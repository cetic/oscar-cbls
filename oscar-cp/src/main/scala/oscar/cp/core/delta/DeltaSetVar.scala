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
import oscar.cp.core.variables.CPSetVar

/**
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class DeltaSetVarTrailEntry(delta: DeltaSetVar, oldSizePossible: Int, oldSizeRequired: Int) extends TrailEntry {
  final override def restore(): Unit = delta.restore(oldSizePossible, oldSizeRequired)
}

final class DeltaSetVar(x: CPSetVar, idx: Int) extends Delta {

  def id: Int = idx

  private[this] val store = x.store
  private[this] var _oldSizePossible: Int = x.possibleSize
  private[this] var _oldSizeRequired: Int = x.requiredSize

  // Used to trail changes in the delta
  private[this] var lastMagic: Long = -1L

  @inline final def oldSizePossible: Int = _oldSizePossible
  @inline final def oldSizeRequired: Int = _oldSizeRequired

  @inline private def trail(): Unit = {
    val contextMagic = store.magic
    if (lastMagic != contextMagic) {
      lastMagic = contextMagic
      store.trail(new DeltaSetVarTrailEntry(this, oldSizePossible,oldSizeRequired))
    }
  }

  @inline final def restore(oldSizePossible: Int, oldSizeRequired: Int): Unit = {
    _oldSizePossible = oldSizePossible
    _oldSizeRequired = oldSizeRequired
  }

  
  final override def update(): Unit = {
    val xps = x.possibleSize
    val xrs = x.requiredSize
    if (xps != _oldSizePossible || xrs != _oldSizeRequired) trail()
    _oldSizePossible = x.possibleSize
    _oldSizeRequired = x.requiredSize
  }

  def changed(): Boolean = x.changed(this)

  def possibleChanged(): Boolean = x.possibleChanged(this)

  def requiredChanged(): Boolean = x.requiredChanged(this)

  def deltaPossibleSize(): Int = x.deltaPossibleSize(this)

  def deltaRequiredSize(): Int = x.deltaRequiredSize(this)

  def deltaPossible(): Iterator[Int] = x.deltaPossible(this)

  def deltaRequired(): Iterator[Int] = x.deltaRequired(this)


  // TODO: add the fillArray methods to avoid creation of iterators


}