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
abstract class IntSequenceExplorer(val intSequence: IntSequence) {

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
  def exploreToPosition(position: Int): Option[IntSequenceExplorer] = {
    if (position < -1 || position > intSequence.size) None
    else if (this.position == position) Some(this)
    else {
      val explorer =
        if (this.position > position) prev
        else next
      explorer.exploreToPosition(position)
    }
  }

  /** Returns the first [[IntSequenceExplorer]] containing the given value, going backward.
    *
    * @param value
    *   The value to reach
    * @return
    *   The IntSequenceExplorer if it exists
    */
  def exploreBackwardUntilValue(value: Int): Option[IntSequenceExplorer] = {
    if (this.value == value) Some(this)
    else {
      val previousExplorer = prev
      previousExplorer.exploreBackwardUntilValue(value)
    }
  }

  /** Returns the first [[IntSequenceExplorer]] containing the given value, going forward.
    *
    * @param value
    *   The value to reach
    * @return
    *   The IntSequenceExplorer if it exists
    */
  def exploreForwardUntilValue(value: Int): Option[IntSequenceExplorer] = {
    if (this.value == value) Some(this)
    else {
      val nextExplorer = next
      nextExplorer.exploreForwardUntilValue(value)
    }
  }

  /** Returns the first [[IntSequenceExplorer]] satisfying the given function, going backward.
    *
    * @param f
    *   The function to satisfy
    * @return
    *   The IntSequenceExplorer if it exists
    */
  def exploreBackwardUntil(f: IntSequenceExplorer => Boolean): Option[IntSequenceExplorer] = {
    if (f(this)) Some(this)
    else {
      val previousExplorer = prev
      previousExplorer.exploreBackwardUntil(f)
    }
  }

  /** Returns the first [[IntSequenceExplorer]] satisfying the given function, going forward.
    *
    * @param f
    *   The function to satisfy
    * @return
    *   The IntSequenceExplorer if it exists
    */
  def exploreForwardUntil(f: IntSequenceExplorer => Boolean): Option[IntSequenceExplorer] = {
    if (f(this)) Some(this)
    else {
      val nextExplorer = next
      nextExplorer.exploreForwardUntil(f)
    }
  }

  /** Returns an IntSequenceExplorer Iterator going backward. Use it for for/while loop */
  def backward: IntSequenceExplorerToIterator = IntSequenceExplorerToIterator(this, forward = false)

  /** Returns an IntSequenceExplorer Iterator going forward. Use it for for/while loop */
  def forward: IntSequenceExplorerToIterator = IntSequenceExplorerToIterator(this, forward = true)

}
