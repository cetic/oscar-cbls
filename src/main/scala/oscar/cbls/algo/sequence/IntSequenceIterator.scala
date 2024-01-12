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

/** [[IntSequence]] as an [[scala.Iterator]] of [[scala.Int]]
  *
  * It uses the logic of [[IntSequenceExplorer]] to perform its tasks
  * @param intSequenceExplorer
  *   The optional [[IntSequenceExplorer]]
  */
class IntSequenceIterator(var intSequenceExplorer: IntSequenceExplorer) extends Iterator[Int] {

  override def hasNext: Boolean =
    intSequenceExplorer match {
      case _: RootIntSequenceExplorer => false
      case _                          => true
    }

  override def next(): Int = {
    if (!hasNext) throw new NoSuchElementException
    val currentExplorer = intSequenceExplorer
    intSequenceExplorer = currentExplorer.next
    currentExplorer.value
  }
}
