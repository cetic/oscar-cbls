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

/** Trait to define an iterator that can perform a bounded number of iterations. This upper bound
  * is given by the `remainingNeighbors` variable.
  */
trait BoundedIterator[T] extends Iterator[T] {

  protected var remainingNeighbors: Int

  /** Act as [[scala.collection.Iterator.hasNext]] but ignore the upper bound. */
  def hasUnboundedNext: Boolean

  /** Act as [[scala.collection.Iterator.next]] but ignore the upper bound. */
  def unboundedNext(): T
}

/** Trait to define iterable working with [[BoundedIterator]]. */
trait BoundedIterable[T] extends Iterable[T] {
  override def iterator: BoundedIterator[T]

  /** Used to stop iteration during neighborhood exploration. */
  def stop(): Unit
}
