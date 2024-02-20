package oscar.cbls.algo.search

import scala.annotation.tailrec

object LazyQuicksort {
  def apply(a: Array[Int], key: Int => Long = a => a) = new LazyQuicksort(a, key)
}

/** this implementation will perform a lazy sort. it will sort on demand, as required by the
  * iterator, or by an explicit call to sortUntil
  *
  * @param array
  *   an array containing the values to sort. the array will be modified by this procedure, to clone
  *   it if you need it somewhere else!
  */
class LazyQuicksort(val array: Array[Int], key: Int => Long = a => a) extends Iterable[Int] {

  private class QList(val left: Int, val right: Int, val tail: QList)

  private[this] var toDo: QList = new QList(0, array.length - 1, null)

  private[this] var lastSortedPosition = -1

  def sortUntil(k: Int): Unit = {
    if (array.length == 0 || k <= lastSortedPosition) return
    while (true) {
      if (toDo == null) return
      val l = toDo.left
      if (l <= k) {
        val r = toDo.right
        toDo = toDo.tail
        sort1(l, r)
      } else {
        lastSortedPosition = l - 1
        return
      }
    }
  }

  @inline
  @tailrec
  private[this] def sort1(l: Int, r: Int): Unit = {
    val pivot: Long = key(array((l + r) / 2))
    var i           = l
    var j           = r
    while (i <= j) {
      while (key(array(i)) < pivot) i += 1
      while (key(array(j)) > pivot) j -= 1
      // we know that array(i) >= pivot && array(j) <= pivot
      if (i <= j) {
        val t = array(i)
        array(i) = array(j)
        array(j) = t
        i += 1
        j -= 1
      }
    }
    if (i < r) toDo = new QList(i, r, toDo)
    if (l < j) sort1(l, j)
    else
      // this is an incomplete update, but this is an approximate value, so we do not care too much.
      lastSortedPosition = j
  }

  def apply(nThSmallestValue: Int): Int = {
    sortUntil(nThSmallestValue)
    array(nThSmallestValue)
  }

  override def iterator: Iterator[Int] = new LazyQuickSortIterator(this)

  class LazyQuickSortIterator(l: LazyQuicksort) extends Iterator[Int] {
    var nextPos: Int       = 0
    override val size: Int = l.array.length

    override def hasNext: Boolean = {
      nextPos < length
    }

    override def next(): Int = {
      l.sortUntil(nextPos)
      val toReturn = l.array(nextPos)
      nextPos += 1
      toReturn
    }
  }
}
