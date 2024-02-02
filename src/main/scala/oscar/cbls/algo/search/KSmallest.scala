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
    validateK(k, xs)
    val pq: PQ[Int] = PQ.from(xs)(Ordering.by(-key(_)))
    val out         = List.newBuilder[Int]
    for (_ <- 0 until k) out += pq.dequeue()
    out.result()
  }

  def apply(a: Seq[Int], key: Int => Long = a => a): KSmallest = new KSmallest(a, key)

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
  def getKFirst(k: Int, xs: Iterable[Int], filter: Int => Boolean = _ => true): Iterable[Int] = {
    validateK(k, xs)
    xs.view.filter(filter).take(k).to(List)
  }

  // helper method to validate k with respect to the input collection
  private def validateK(k: Int, xs: Iterable[Int]): Unit = {
    require(k >= 0, "Cannot pick a negative number of elements")
    require(k <= xs.size, "Cannot take more elements than the size of the collection")
  }
}

/** This class allows to defer the query of the k smallest elements of a collection to a later time.
  * The query can be done with an additional filter. Internally, this class sorts the entire
  * collection according to the given key.
  *
  * @param xs
  *   the collection
  * @param key
  *   function according to which the collection is sorted
  */
class KSmallest(xs: Seq[Int], key: Int => Long = a => a) {
  private val sorted: Seq[Int] = xs.sortBy(key)

  /** Return the first k elements of the collection after it's been sorted.
    * @param k
    *   the number of elements
    */
  def apply(k: Int): Seq[Int] = {
    KSmallest.validateK(k, xs)
    sorted.take(k)
  }

  /** Return the first k elements of the sorted collection satisfying a given predicate. Depending
    * on the predicate, fewer than k elements may be returned.
    * @param k
    *   the number of elements
    * @param filter
    *   the predicate
    */
  def apply(k: Int, filter: Int => Boolean): Seq[Int] = {
    KSmallest.validateK(k, xs)
    sorted.view.filter(filter).take(k).to(Seq)
  }
}
