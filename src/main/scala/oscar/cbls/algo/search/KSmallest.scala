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

import collection.mutable.{PriorityQueue => PQ}

/** This class serves to compute the k-smallest values of a given vector. this computation can be
  * done either one-shot, or with gradually increasing k
  */
object KSmallest {

  /** Returns the k integers with lowest priority in a collection, according to the provided key. In
    * particular, if the key is the identity (the default), it returns the k smallest elements, and
    * if the key is x => -x, it returns the k largest elements. The resulting collection is sorted
    * according to priority; e.g., in increasing order if the key is the identity.
    *
    * @param k
    *   the number of elements to select. Cannot be negative or larger than the size of the
    *   collection
    * @param xs
    *   the collection from which the elements are selected
    * @param key
    *   the function that can be used to alter the order of the elements
    * @return
    *   the list of with k smallest elements, sorted
    */
  def getKSmallest(k: Int, xs: Iterable[Int], key: Int => Long = x => x): List[Int] = {
    require(k >= 0, "Cannot pick a negative number of elements")
    require(k <= xs.size, "Cannot take more elements than the size of the collection")
    val pq: PQ[Int] = PQ.from(xs)(Ordering.by(-key(_)))
    val out         = List.newBuilder[Int]
    for (_ <- 0 until k) out += pq.dequeue()
    out.result()
  }

  def apply(a: Array[Int], key: Int => Long): KSmallest = new KSmallest(a, key)

  /** This method takes the first k elements (according to the order in which its iterator returns
    * them) that satisfy a given predicate. Note: might return different results for different runs,
    * unless the underlying collection type is ordered.
    *
    * @param k
    *   the number of elements to pick
    * @param xs
    *   the collection of elements
    * @param filter
    *   the function determining which elements are picked
    * @return
    */
  def kFirst(k: Int, xs: Iterable[Int], filter: Int => Boolean = _ => true): Iterable[Int] = {
    require(k >= 0, "Cannot pick a negative number of elements")
    require(k <= xs.size, "Cannot take more elements than the size of the collection")
    xs.view.filter(filter).take(k).to(List)
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
