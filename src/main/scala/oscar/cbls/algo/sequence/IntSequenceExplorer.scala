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
}
