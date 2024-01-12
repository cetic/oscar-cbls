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
abstract class IntSequenceExplorer(intSequence: IntSequence) {

  /** The value at the current position */
  def value: Int

  /** Returns the position of the value */
  def position: Int

  /** Returns the next explorer in the sequence. RootIntSequenceExplorer if end of sequence */
  def next: IntSequenceExplorer

  /** Returns the previous explorer in the sequence. RootIntSequenceExplorer if start of sequence */
  def prev: IntSequenceExplorer

  /** Returns the [[IntSequenceExplorer]] at the given position.
    *
    * Based on this.position goes backward or forward
    * @param position
    *   The position to reach
    * @return
    *   The IntSequenceExplorer if it exists
    */
  def goToPosition(position: Int): Option[IntSequenceExplorer] = {
    if (position < -1 || position > intSequence.size) None
    else if (this.position == position) Some(this)
    else {
      val explorer =
        if (this.position > position) prev
        else next
      explorer.goToPosition(position)
    }
  }

  /** Returns the [[IntSequenceExplorer]] containing the given value, going backward.
    *
    * @param value
    *   The value to reach
    * @return
    *   The IntSequenceExplorer if it exists
    */
  def prevUntilValue(value: Int): Option[IntSequenceExplorer] = {
    if (this.value == value) Some(this)
    else {
      val previousExplorer = prev
      previousExplorer.prevUntilValue(value)
    }
  }

  /** Returns the [[IntSequenceExplorer]] containing the given value, going forward.
    *
    * @param value
    *   The value to reach
    * @return
    *   The IntSequenceExplorer if it exists
    */
  def nextUntilValue(value: Int): Option[IntSequenceExplorer] = {
    if (this.value == value) Some(this)
    else {
      val nextExplorer = next
      nextExplorer.nextUntilValue(value)
    }
  }

  /** Returns the [[IntSequenceExplorer]] satisfying the given function, going backward.
    *
    * @param f
    *   The function to satisfy
    * @return
    *   The IntSequenceExplorer if it exists
    */
  def prevUntil(f: IntSequenceExplorer => Boolean): Option[IntSequenceExplorer] = {
    if (f(this)) Some(this)
    else {
      val previousExplorer = prev
      previousExplorer.prevUntil(f)
    }
  }

  /** Returns the [[IntSequenceExplorer]] satisfying the given function, going forward.
    *
    * @param f
    *   The function to satisfy
    * @return
    *   The IntSequenceExplorer if it exists
    */
  def nextUntil(f: IntSequenceExplorer => Boolean): Option[IntSequenceExplorer] = {
    if (f(this)) Some(this)
    else {
      val nextExplorer = next
      nextExplorer.nextUntil(f)
    }
  }

  /** Applies the given function on each [[IntSequenceExplorer]]
    *
    * @param f
    *   The function to apply
    */
  def foreach(f: IntSequenceExplorer => Unit): Unit = {
    if (!this.isInstanceOf[RootIntSequenceExplorer]) f(this)
    next match {
      case _: RootIntSequenceExplorer    =>
      case explorer: IntSequenceExplorer => explorer.foreach(f)
    }
  }

}
