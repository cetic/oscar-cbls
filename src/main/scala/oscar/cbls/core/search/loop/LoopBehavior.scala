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

/** Factory to create concrete implementation of [[LoopBehavior]]. */
object LoopBehavior {

  /** Creates a [[First]] loop behavior
    *
    * @param maxNeighbors
    *   The maximum number of neighbors to explore.
    */
  def first(maxNeighbors: () => Int = () => Int.MaxValue): First = new First(maxNeighbors)

  /** Creates a [[Best]] loop behavior.
    *
    * @param maxNeighbors
    *   The maximum number of neighbors to explore.
    */
  def best(maxNeighbors: () => Int = () => Int.MaxValue): Best = new Best(maxNeighbors)
}

/** Abstract used to define the behavior for loops used during neighborhoods exploration.
  * @param maxNeighbors
  *   The maximum number of neighbors to explore.
  */
abstract class LoopBehavior(maxNeighbors: () => Int = () => Int.MaxValue) {

  /** @param baseIterable
    *   Collection of variables to be explored.
    * @tparam T
    *   Type of the explored variables.
    * @return
    *   A bounded version of `baseIterable` and a function to stop an iteration. Used to iterate
    *   over neighbors during a search. The `stop` function is used to stop the search.
    */
  def toIterable[T](baseIterable: Iterable[T]): (BoundedIterable[T], () => Unit)

  /** @param baseIterable
    *   Collection of variables to be explored.
    * @tparam T
    *   Type of the explored variables.
    * @return
    *   A bounded iterator associated to `baseIterable` and a function to stop an iteration.
    */
  def toIterator[T](baseIterable: Iterable[T]): (BoundedIterator[T], () => Unit) = {
    val (iterable, stop) = toIterable(baseIterable)
    (iterable.iterator, stop)
  }

}
