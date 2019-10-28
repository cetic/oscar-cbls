/*******************************************************************************
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
  ******************************************************************************/
package oscar.cbls.algo.heap

import oscar.cbls.algo.quick.{QArrayList, QList}

import scala.collection.Iterator

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

/**This class is a faster version of a heap, where several items stored in it have same index.
 * The heap is actually made of an array,
 * storing lists containing items with same position.
 * A binomial heap is maintained to record the lowest position in the heap.
 * This is more efficient if it often occurs that elements have the same position.
 * keys is assumed to start at zero.
  *
  * @author renaud.delandtsheer@cetic.be
 */
class AggregatedBinomialHeapQList[T](GetKey:T => Int,val maxPosition:Int) extends AbstractHeap[T] {

  private[this] val b = new BinomialHeap[Int](a => a, maxPosition)

  private[this] val a:Array[QList[T]] = Array.tabulate (maxPosition)(_ => null)

  private[this] var empty:Boolean = true

  /**makes the datastruct empty**/
  override def dropAll(){
    for (i <- b) a(i) = null
    empty = true
    b.dropAll()
  }

  override def insert(elem:T){
    val position = GetKey(elem)
    val otherWithSamePosition = a(position)
    if (otherWithSamePosition == null){
      a(position) = QList(elem)
      b.insert(position)
      empty = false
    }else{
      //this is the desired branch, as it is O(1L)
      a(position) = QList(elem, otherWithSamePosition)
    }
  }

  override def getFirsts:List[T] = throw new Error("too inefficient")

  override def popFirsts:List[T] = throw new Error("too inefficient")

  override def isEmpty:Boolean = empty
  override def size: Int = throw new Error("too inefficient")

  override def getFirst: T = a(b.getFirst).head

  override def popFirst(): T = {
    val position = b.getFirst
    val liste = a(position)
    val toreturn = liste.head
    a(position) = liste.tail
    if (liste.tail == null){
      b.popFirst()
      empty = b.isEmpty
    }
    toreturn
  }

  override def iterator: Iterator[T] = {
    var acc:List[T] = List()
    for (position <- b){
      var curr = a(position)
        while(curr != null){
          acc = curr.head :: acc
          curr = curr.tail
        }
    }
    acc.iterator
  }
}


/**This class is a faster version of a heap, where several items stored in it have same index.
  * The heap is actually made of an array, storing lists containing items with same position.
  * A binomial heap is maintained to record the lowest position in the heap.
  * This is more efficient if it often occurs that elements have the same position.
  * keys is assumed to start at zero.
  *
  * @author renaud.delandtsheer@cetic.be
  */
class AggregatedBinomialHeapArrayList[T](GetKey:T => Int,val maxPosition:Int, initialSizeForArrayList:Int = 10)(implicit val X:Manifest[T]) extends AbstractHeap[T] {

  private[this] val b = new BinomialHeap[Int](a => a, maxPosition)

  private[this] val a:Array[QArrayList[T]] = Array.tabulate (maxPosition)(_ => new QArrayList[T](initialSizeForArrayList))

  private[this] var msize:Long = 0

  /**makes the datastruct empty*/
  def dropAll(){
    for (i <- b) a(i).setEmpty()
    msize = 0
    b.dropAll()
  }

  def insert(elem:T){
    val position = GetKey(elem)
    val otherWithSamePosition = a(position)
    if(otherWithSamePosition.isEmpty) b.insert(position)
    otherWithSamePosition.put(elem)
  }

  override def isEmpty:Boolean = b.isEmpty

  def popFirst(): T = {
    val position = b.getFirst
    val arrayList = a(position)
    val toreturn = arrayList.pop()
    if (arrayList.isEmpty){
      b.popFirst()
    }
    toreturn
  }

  override def getFirsts: List[T] = throw new Exception ("not available")

  override def getFirst: T =  throw new Exception ("not available")

  override def popFirsts: List[T] =  throw new Exception ("not available")

  override def iterator: scala.Iterator[T] =  throw new Exception ("not available")
}
