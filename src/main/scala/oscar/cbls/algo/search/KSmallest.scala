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

package oscar.cbls.algo.search

import oscar.cbls.algo.heap.BinaryHeap

/** This class serves to compute the k-smallest values of a given vector. this computation can be
  * done either one-shot, or with gradually increasing k
  */
object KSmallest {

  /** returns the k smallest elements, but they are not sorted between themselves.
    * @param a
    * @param k
    * @param key
    * @return
    */
  def getkSmallests(a: Array[Int], k: Int, key: Int => Long): List[Int] = {
    val heap = new BinaryHeap[Int](indice => -key(a(indice)), 2 * k)
    for (i <- a.indices) {
      heap.insert(i)
      if (i >= k) heap.popFirst()
    }
    heap.toList.map(a(_))
  }

  def doSortGetLater(a: Array[Int], key: Int => Long): KSmallest = new KSmallest(a, key)

  def lazySort(a: Array[Int], key: Int => Long): Iterable[Int] = new LazyQuicksort(a, key)

  def kFirst(k: Int, values: Iterable[Int], filter: Int => Boolean = _ => true): Iterable[Int] = {
    def kFirstAccumulator(sortedNeighbors: Iterator[Int], k: Int): List[Int] = {
      require(k >= 0)
      if (k == 0 || !sortedNeighbors.hasNext) {
        null
      } else {
        val neighbor = sortedNeighbors.next()
        if (filter(neighbor))
          neighbor +: kFirstAccumulator(sortedNeighbors, k - 1)
        else
          kFirstAccumulator(sortedNeighbors, k)
      }
    }
    //////////
    kFirstAccumulator(values.iterator, k)
  }
}

/** @param a
  *   array of values
  * @param key
  *   value from a to key
  */
class KSmallest(a: Array[Int], key: Int => Long = a => a) {
  // zipWithIndex puts index in second position of the couple
  val sortedPositions: List[Int] = a.toList.zipWithIndex.sortBy(couple => key(couple._1)).map(_._2)

  def apply(k: Int): List[Int] = sortedPositions.take(k)

  def apply(k: Int, filter: Int => Boolean): List[Int] = {
    def kSmallestAcc(sorted: List[Int], k: Int): List[Int] = {
      require(k >= 0)
      if (k == 0) return Nil
      sorted match {
        case Nil => Nil
        case h :: t =>
          if (filter(h)) h :: kSmallestAcc(t, k - 1)
          else kSmallestAcc(t, k)
      }
    }
    kSmallestAcc(sortedPositions, k)
  }
}
