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

  /** Returns the [[IntSequenceExplorer]] at the given position.
    *
    * Based on this.position goes backward or forward
    * @param position
    *   The position to reach
    * @return
    *   The [[IntSequenceExplorer]] as an [[scala.Option]] or None
    */
  def toPosition(position: Int): Option[IntSequenceExplorer] = {
    require(position >= -1)
    if (this.position == position) Some(this)
    else {
      val explorer =
        if (this.position > position) prev
        else next
      if (explorer.nonEmpty) explorer.get.toPosition(position)
      else None
    }
  }

  /** Returns the [[IntSequenceExplorer]] containing the given value, going backward.
    *
    * @param value
    *   The value to reach
    * @return
    *   The [[IntSequenceExplorer]] as an [[scala.Option]] or None
    */
  def prevUntilValue(value: Int): Option[IntSequenceExplorer] = {
    if (this.value == value) Some(this)
    else {
      val previousExplorer = prev
      if (previousExplorer.nonEmpty) previousExplorer.get.prevUntilValue(value)
      else None
    }
  }

  /** Returns the [[IntSequenceExplorer]] containing the given value, going forward.
    *
    * @param value
    *   The value to reach
    * @return
    *   The [[IntSequenceExplorer]] as an [[scala.Option]] or None
    */
  def nextUntilValue(value: Int): Option[IntSequenceExplorer] = {
    if (this.value == value) Some(this)
    else {
      val nextExplorer = next
      if (nextExplorer.nonEmpty) nextExplorer.get.nextUntilValue(value)
      else None
    }
  }

  /** Returns the [[IntSequenceExplorer]] satisfying the given function, going backward.
    *
    * @param f
    *   The function to satisfy
    * @return
    *   The [[IntSequenceExplorer]] as an [[scala.Option]] or None
    */
  def prevUntil(f: IntSequenceExplorer => Boolean): Option[IntSequenceExplorer] = {
    if (f(this)) Some(this)
    else {
      val previousExplorer = prev
      if (previousExplorer.nonEmpty) previousExplorer.get.prevUntil(f)
      else None
    }
  }

  /** Returns the [[IntSequenceExplorer]] satisfying the given function, going forward.
    *
    * @param f
    *   The function to satisfy
    * @return
    *   The [[IntSequenceExplorer]] as an [[scala.Option]] or None
    */
  def nextUntil(f: IntSequenceExplorer => Boolean): Option[IntSequenceExplorer] = {
    if (f(this)) Some(this)
    else {
      val nextExplorer = next
      if (nextExplorer.nonEmpty) nextExplorer.get.nextUntil(f)
      else None
    }
  }

  /** Applies the given function on each [[IntSequenceExplorer]]
    *
    * @param f
    *   The function to apply as [[IntSequenceExplorer]] to [[scala.Unit]]
    */
  def foreach(f: IntSequenceExplorer => Unit): Unit = {
    f(this)
    next match {
      case None           =>
      case Some(explorer) => explorer.foreach(f)
    }
  }

}
