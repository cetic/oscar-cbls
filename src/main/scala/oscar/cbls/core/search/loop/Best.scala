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

package oscar.cbls.core.search.loop

/** By using this loop behaviour, the exploration visits all the neighbors to find the most
  * improving one.
  *
  * @param maxNeighbors
  *   The maximum number of neighbors to explore.
  */
class Best(maxNeighbors: () => Int = () => Int.MaxValue) extends LoopBehavior(maxNeighbors) {

  override def toIterable[T](baseIterable: Iterable[T]): (BoundedIterable[T], () => Unit) = {
    val iterable: BoundedIterable[T] = new BoundedIterable[T] {

      override def iterator: BoundedIterator[T] = new BoundedIterator[T] {

        private val baseIterator: Iterator[T] = baseIterable.iterator

        override protected var remainingNeighbors: Int = maxNeighbors()

        override def hasUnboundedNext: Boolean = baseIterator.hasNext

        override def unboundedNext(): T = baseIterator.next()

        override def hasNext: Boolean = remainingNeighbors > 0 && baseIterator.hasNext

        override def next(): T = {
          remainingNeighbors -= 1
          baseIterator.next()
        }
      }

      // We want to find the best neighbor. We cannot just stop when an improving move is found.
      // We need to test all the possible move. So, here, this function does nothing.
      override def stop(): Unit = {}
    }

    (iterable, iterable.stop _)
  }
}
