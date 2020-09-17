/**
 * *****************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 * ****************************************************************************
 */

package oscar.cbls.algo.search

import oscar.cbls.algo.heap.BinomialHeap
import oscar.cbls.algo.quick.QList
import oscar.cbls.util.StopWatch

import scala.annotation.tailrec

/**
 * This class serves to compute the k-smallest values of a given vector.
 * this computation can be done either one-shot, or with gradually increasing k
 */
object KSmallest {

  /**
   * returns the k smallest elements, but they are not sorted between themselves.
   * @param a
   * @param k
   * @param key
   * @return
   */
  def getkSmallests(a:Array[Int],k:Int, key:Int => Long):List[Int] = {
    val heap = new BinomialHeap[Int](indice => -key(a(indice)),2*k)
    for(i <- a.indices){
      heap.insert(i)
      if(i >= k)heap.popFirst()
    }
    heap.toList.map(a(_))
  }

  def doSortGetLater(a:Array[Int],key:Int => Long):KSmallest = new KSmallest(a,key)

  def lazySort(a:Array[Int], key:Int=>Long):Iterable[Int] = new LazyQuicksort(a,key)

  def kFirst(k: Int, values:Iterable[Int], filter: (Int => Boolean) = _ => true): Iterable[Int] = {
    def kFirstAccumulator(sortedNeighbors: Iterator[Int], k: Int): QList[Int] = {
      require(k >= 0)
      if(k == 0 || !sortedNeighbors.hasNext){
        null
      }else{
        val neighbor = sortedNeighbors.next()
        if (filter(neighbor))
          QList(neighbor,kFirstAccumulator(sortedNeighbors, k - 1))
        else
          kFirstAccumulator(sortedNeighbors, k)
      }
    }
    //////////
    QList.toIterable(kFirstAccumulator(values.iterator, k))
  }
}

/**
  *
  * @param a array of values
  * @param key value from a to key
  */
class KSmallest(a:Array[Int],key:Int => Long = a => a){
  //zipWithIndex puts index in second position of the couple
  val sortedPositions:List[Int] = a.toList.zipWithIndex.sortBy(couple => key(couple._1)).map(_._2)

  def apply(k:Int):List[Int] = sortedPositions.take(k)

  def apply(k:Int,filter:Int=>Boolean): List[Int] = {
    def kSmallestAcc(sorted: List[Int], k: Int): List[Int] = {
      require(k >= 0)
      if(k == 0) return Nil
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

object testQuickSort extends App with StopWatch{

  val n = 10000000
  val k = 500
  val randomValues = Array.tabulate(n)(_ => (math.random * Int.MaxValue).toInt)

  startWatch()
  val s = KSmallest.getkSmallests(randomValues,k,x => x)
  val watch1 = getWatch

  startWatch()
  val qs = new LazyQuicksort(randomValues)
  val it = qs.iterator
  for(i <- 1 to k){
    val j = it.next()
  }
  val watch2 = getWatch

  println(s"nonLazy:$watch1")
  println(s"lazy:$watch2")
}

/**
 * this implementation will perform a lazy sort.
 * it will sort on demand, as required by the iterator, or by an explicit call to sortUntil
 * @param array an array containing the values to sort. the array will be modified by this procedure, to clone it if you need it somewhere else!
 */
class LazyQuicksort(val array:Array[Int], key:Int => Long = a => a) extends Iterable[Int] {

  class QList(val left:Int, val right:Int, val tail:QList)
  private[this] var toDo: QList = new QList(0, array.length - 1,null)

  private[this] var lastSortedPosition = -1
  def sortUntil(k: Int): Unit = {
    if(k <= lastSortedPosition) return
    while (true) {
      if (toDo == null) return
      val l = toDo.left
      if(l <= k){
        val r = toDo.right
        toDo = toDo.tail
        sort1(l, r)
      }else {
        lastSortedPosition = l - 1
        return
      }
    }
  }

  @inline
  @tailrec
  private[this] def sort1(l: Int, r: Int): Unit = {
    val pivot: Long = key(array((l + r) / 2))
    var i = l
    var j = r
    while (i <= j) {
      while (key(array(i)) < pivot) i += 1
      while (key(array(j)) > pivot) j -= 1
      //we know that array(i) >= pivot && array(j) <= pivot
      if (i <= j) {
        val t = array(i)
        array(i) = array(j)
        array(j) = t
        i += 1
        j -= 1
      }
    }
    if (i < r) toDo = new QList(i, r,toDo)
    if (l < j) sort1(l, j)
    else lastSortedPosition = j //this is an incomplete update, but this is an approximate value, so we do not care too much.
  }

  def apply(nThSmallestValue:Int):Int = {
    sortUntil(nThSmallestValue)
    array(nThSmallestValue)
  }

  override def iterator: Iterator[Int] = new LazyQuickSortIterator(this)

  class LazyQuickSortIterator(l:LazyQuicksort) extends Iterator[Int]{
    var nextPos:Int = 0
    override val length: Int = l.array.length

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
