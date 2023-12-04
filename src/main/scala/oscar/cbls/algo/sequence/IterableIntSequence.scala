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

/** [[IntSequence]] as an [[scala.Iterable]] of [[scala.Int]]
  *
  * @param sequence
  *   The IntSequence to convert
  */
class IterableIntSequence(sequence: IntSequence) extends Iterable[Int] {
  override def iterator: Iterator[Int] = sequence.iterator

  override def head: Int = sequence.valueAtPosition(0).get

  override def headOption: Option[Int] = sequence.valueAtPosition(0)

  override def last: Int = sequence.valueAtPosition(sequence.size - 1).get

  override def lastOption: Option[Int] = sequence.valueAtPosition(sequence.size - 1)
}
