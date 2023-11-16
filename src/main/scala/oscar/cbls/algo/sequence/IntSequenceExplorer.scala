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

package oscar.cbls.algo.sequence

/** Abstract class used to explore an [[IntSequence]]
  *
  * Each instance represents the state of the sequence at a specific position.
  */
abstract class IntSequenceExplorer {
  // The value at the current position
  val value: Int
  // Returns the position of the value
  def position: Int
  // Returns the next explorer in the sequence or None if end of sequence
  def next: Option[IntSequenceExplorer]
  // Returns the previous explorer in the sequence or None if start of sequence
  def prev: Option[IntSequenceExplorer]

  def untilPosition(position: Int): Option[IntSequenceExplorer] = {
    require(position >= 0)
    if (this.position == position) Some(this)
    else {
      val explorer =
        if (this.position > position) prev
        else next
      if (explorer.nonEmpty) explorer.get.untilPosition(position)
      else None
    }
  }

  def backwardUntilValue(value: Int): Option[IntSequenceExplorer] = {
    if (this.value == value) Some(this)
    else {
      val previousExplorer = prev
      if (previousExplorer.nonEmpty) previousExplorer.get.backwardUntilValue(value)
      else None
    }
  }

  def forwardUntilValue(value: Int): Option[IntSequenceExplorer] = {
    if (this.value == value) Some(this)
    else {
      val nextExplorer = next
      if (nextExplorer.nonEmpty) nextExplorer.get.forwardUntilValue(value)
      else None
    }
  }

  def backwardUntil(f: IntSequenceExplorer => Boolean): Option[IntSequenceExplorer] = {
    if (f(this)) Some(this)
    else {
      val previousExplorer = prev
      if (previousExplorer.nonEmpty) previousExplorer.get.backwardUntil(f)
      else None
    }
  }

  def forwardUntil(f: IntSequenceExplorer => Boolean): Option[IntSequenceExplorer] = {
    if (f(this)) Some(this)
    else {
      val nextExplorer = next
      if (nextExplorer.nonEmpty) nextExplorer.get.forwardUntil(f)
      else None
    }
  }

}
