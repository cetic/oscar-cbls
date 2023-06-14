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

package oscar.cbls.algo.heap

/** This class is a faster version of a heap, where several items stored in it have same index. The
  * heap is actually made of an array, storing lists containing items with same position. A binomial
  * heap is maintained to record the lowest position in the heap. This is more efficient if it often
  * occurs that elements have the same position. keys is assumed to start at zero.
  *
  * @author
  *   renaud.delandtsheer@cetic.be
  */
class AggregatedBinomialHeap[T](priority: T => Int, val maxSize: Int)
    extends AbstractHeap[T] {

  private[this] val b = new BinomialHeap[Int](a => a, maxSize)

  private[this] val a: Array[List[T]] = Array.tabulate(maxSize)(_ => null)

  private[this] var checkEmpty: Boolean = true

  /** makes the datastruct empty* */
  override def dropAll(): Unit = {
    for (i <- b) a(i) = null
    checkEmpty = true
    b.dropAll()
  }

  override def insert(elem: T): Unit = {
    val position              = priority(elem)
    val otherWithSamePosition = a(position)
    if (otherWithSamePosition == null || otherWithSamePosition.isEmpty) {
      a(position) = List(elem)
      b.insert(position)
      checkEmpty = false
    } else {
      // this is the desired branch, as it is O(1L)
      a(position) = elem :: otherWithSamePosition
    }
  }

  override def getFirsts: List[T] = {
    a(b.getFirst)
  }

  override def popFirsts: List[T] = {
    val position = b.popFirst()
    val toReturn = a(position)
    a(position) = List.empty
    checkEmpty = b.isEmpty
    toReturn
  }

  override def isEmpty: Boolean = checkEmpty
  override def size: Int        = throw new Error("too inefficient")

  override def getFirst: T = a(b.getFirst).head

  override def popFirst(): T = {
    val position = b.getFirst
    val liste    = a(position)
    val toreturn = liste.head
    a(position) = liste.tail
    if (liste.tail.isEmpty) {
      b.popFirst()
      checkEmpty = b.isEmpty
    }
    toreturn
  }

  override def iterator: Iterator[T] = {
    var acc: List[T] = List()
    for (position <- b) {
      var curr = a(position)
      while (curr != null) {
        acc = curr.head :: acc
        curr = curr.tail
      }
    }
    acc.iterator
  }
}
