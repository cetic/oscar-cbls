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
/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

package oscar.cbls.algo.heap

import scala.collection.Iterator
import scala.collection.immutable.SortedMap

/**
 * This is a binary heap that is efficient; all operations are in O(log(n))
 * smallest first
 * @param initialGetKey a function that returns an integer for each element inserted in the heap this value is used to sort the heap content
 * @param maxsize the maximum number of elements that can be inserted in this heap
 * @param X the manifest of T, to create arrays of T's
 * @tparam T the type of elements included in the heap
 * @author renaud.delandtsheer@cetic.be
 */
class BinomialHeap[T](initialGetKey:T => Long,val maxsize:Int)
                     (implicit val X:Manifest[T]) extends AbstractHeap[T] {
  val heapArray:Array[T] = new Array[T](maxsize)
  private var msize:Int=0

  var GetKey:T => Long = initialGetKey

  /**changes the key getter according to which the heap is sorted.
   * Can be costly if the heap is not empty.
   * @param KeyGetter the new key getter
   */
  def keyGetter_=(KeyGetter:T => Long): Unit = {
    if(msize>0L){
      val content:List[T] = this.toList
      dropAll()
      GetKey = KeyGetter
      content foreach insert
    }else{
      GetKey = KeyGetter
    }
  }

  def keyGetter:T => Long = GetKey

  override def size: Int = msize
  override def isEmpty:Boolean = msize == 0L

  override def toString:String = {
    heapArray.toList.toString
  }

  /**makes the datastruct empty, but does not frees the space*/
  override def dropAll(): Unit = {
    msize = 0
  }

  /**log(n)*/
  override def insert(elem:T): Unit = {
    //insert en derniere position, puis bubble up
    heapArray(msize)=elem
    msize +=1
    pushUp(msize-1)
  }

  /**O(1L) operation*/
  private def swapPositions(position1:Int,position2:Int): Unit = {
    val tmp:T = heapArray(position1)
    heapArray(position1)=heapArray(position2)
    heapArray(position2)=tmp
  }

  //returns the last position of the moved item
  private def pushDown():Long = {
    var position = 0
    while(true)
      if(leftChild(position) < msize && GetKey(heapArray(position)) > GetKey(heapArray(leftChild(position)))){
        //examiner aussi left child
        if(rightChild(position) < msize && GetKey(heapArray(rightChild(position))) < GetKey(heapArray(leftChild(position)))){
          //c'est avec le right child qu'il faut inverser
          swapPositions(position,rightChild(position))
          position = rightChild(position)
        }else{
          //c'est avec le left chile qu'il faut inverser
          swapPositions(position,leftChild(position))
          position = leftChild(position)
        }
      }else if(rightChild(position) < msize && GetKey(heapArray(position)) > GetKey(heapArray(rightChild(position)))){
        //only consider right child
        swapPositions(position,rightChild(position))
        position = rightChild(position)
      }else{
        return position
      }
    position //jamais execute
  }

  private def pushUp(startposition:Int): Long = {
    var position = startposition
    while(true){
      val fatherposition:Int = father(position)
      if (fatherposition >= 0 && GetKey(heapArray(position)) < GetKey(heapArray(fatherposition))){
        swapPositions(position,fatherposition)
        position = fatherposition
      }else{
        return position
      }
    }
    position //never reached
  }

  private def leftChild(position:Int):Int = (position+1)*2-1
  private def rightChild(position:Int):Int = (position+1)*2
  private def father(position:Int):Int = (position-1)/2

  /**O(firsts)*/
  override def getFirsts:List[T] = {
    def ExploreFirsts(value:Long,startposition:Int,acc:List[T]):List[T] = {
      if(startposition < msize && GetKey(heapArray(startposition)) == value){
        val acc1 = ExploreFirsts(value,leftChild(startposition),heapArray(startposition) :: acc)
        ExploreFirsts(value,rightChild(startposition),acc1)
      }else{
        acc
      }
    }
    if(msize == 0L)List.empty
    else ExploreFirsts(GetKey(heapArray(0)),0,List.empty)
  }
  /**O(1L)*/
  override def getFirst:T=heapArray(0)

  /**O(log(n))*/
  override def popFirst():T={
    val toreturn:T = heapArray(0)
    if(msize == 1){
      heapArray(0)=null.asInstanceOf[T]
      msize = 0
    }else{
      swapPositions(0,msize-1)
      msize -=1
      heapArray(msize)=null.asInstanceOf[T]
      pushDown()
    }
    toreturn
  }

  /**you cannot modify the hep when iterating through this iterator*/
  override def iterator: Iterator[T] = new BinomialHeapIterator(heapArray,msize)

  override def popFirsts: List[T] = {
    if (isEmpty) return List.empty
    var acc = List(popFirst())
    val key = GetKey(acc.head)
    while(!isEmpty && GetKey(getFirst) == key){
      acc = popFirst() :: acc
    }
    acc
  }
}

class BinomialHeapIterator[T](HeapArray:Array[T],size:Int) extends Iterator[T]{
  var current:Int = size

  def hasNext: Boolean = current > 0

  def next(): T = {
    current = current-1
    HeapArray(current)
  }
}

/**
 * This is a binary heap that is less efficient than the [[oscar.cbls.algo.heap.BinomialHeap]].
 * It offers more operations, such as delete and update value.
 * smallest first
 * @param getKey a function that returns an integer for each element inserted i nthe heap this value is used to sort the heap content
 * @param maxsize the maximum number of elements that can be inserted in this heap
 * @param X the manifest of T, to create arrays of T's
 * @tparam T the type of elements included in the heap
 * @author renaud.delandtsheer@cetic.be
 * */
class BinomialHeapWithMove[T](getKey:T => Long,val maxsize:Int)(implicit val A:Ordering[T],implicit val X:Manifest[T]){
  private[this] val heapArray:Array[T] = new Array[T](maxsize)
  var size:Int=0
  var position:SortedMap[T,Int]=SortedMap.empty

  def isEmpty:Boolean = size == 0L

  def contains(value:T):Boolean = position.contains(value)

  def checkInternals(): Unit = {
    for(i <- heapArray.indices if i < size-1){
      if (leftChild(i) < size){
        require(getKey(heapArray(i)) <= getKey(heapArray(leftChild(i))),"heap error " + this + i)
        require(father(leftChild(i)) == i,"heap error " + this)
      }
      if (rightChild(i) < size){
        require(getKey(heapArray(i)) <= getKey(heapArray(rightChild(i))),"heap error " + this)
        require(father(rightChild(i)) == i,"heap error " + this)
      }
    }

    for(t <- position.keys){
      assert(heapArray(position(t)) == t)
    }
  }

  override def toString:String = {
    heapArray.toList.toString
  }

  def insert(elem:T): Unit = {
    //insert en derniere position, puis bubble up
    heapArray(size)=elem
    position +=((elem,size))
    size +=1
    pushUp(size-1)
  }

  def getElements:Iterable[T] = {
    position.keys
  }

  private def swapPositions(position1:Int,position2:Int): Unit = {
    position+=((heapArray(position1),position2))
    position+=((heapArray(position2),position1))

    val tmp:T = heapArray(position1)
    heapArray(position1)=heapArray(position2)
    heapArray(position2)=tmp
  }

  //returns the last position of the moved item
  private def pushDown(startposition:Int):Int = {
    var position = startposition
    while(true)
      if(leftChild(position) < size && getKey(heapArray(position)) > getKey(heapArray(leftChild(position)))){
        //examiner aussi left child
        if(rightChild(position) < size && getKey(heapArray(rightChild(position))) < getKey(heapArray(leftChild(position)))){
          //c'est avec le right child qu'il faut inverser
          swapPositions(position,rightChild(position))
          position = rightChild(position)
        }else{
          //c'est avec le left chile qu'il faut inverser
          swapPositions(position,leftChild(position))
          position = leftChild(position)
        }
      }else if(rightChild(position) < size && getKey(heapArray(position)) > getKey(heapArray(rightChild(position)))){
        //only consider right child
        swapPositions(position,rightChild(position))
        position = rightChild(position)
      }else{
        return position
      }
    position //jamais execute
  }

  private def pushUp(startposition:Int):Int = {
    var position = startposition
    while(true){
      val fatherposition:Int = father(position)
      if (fatherposition >= 0 && getKey(heapArray(position)) < getKey(heapArray(fatherposition))){
        swapPositions(position,fatherposition)
        position = fatherposition
      }else{
        return position
      }
    }
    position //never reached
  }

  private def leftChild(position:Int):Int = (position+1)*2-1
  private def rightChild(position:Int):Int = (position+1)*2
  private def father(position:Int):Int = (position-1)/2

  def getFirsts:List[T] = {
    def ExploreFirsts(value:Long,startposition:Int,acc:List[T]):List[T] = {
      if(startposition < size && getKey(heapArray(startposition)) == value){
        val acc1 = ExploreFirsts(value,leftChild(startposition),heapArray(startposition) :: acc)
        ExploreFirsts(value,rightChild(startposition),acc1)
      }else{
        acc
      }
    }
    if(size == 0)List.empty
    else ExploreFirsts(getKey(heapArray(0)),0,List.empty)
  }

  def getFirst:T=heapArray(0)

  def removeFirst():T={
    val toreturn:T = heapArray(0)
    swapPositions(0,size-1)
    size -=1
    position -= toreturn
    heapArray(size)=null.asInstanceOf[T]
    pushDown(0)
    toreturn
  }

  def notifyChange(elem:T): Unit = {
    val startposition = position(elem)
    pushDown(pushUp(startposition))
  }

  def delete(elem:T): Unit ={
    val startposition:Int = position(elem)
    if (startposition == size-1){
      size -=1
      position -= elem
      heapArray(size)=null.asInstanceOf[T]
    }else{
      swapPositions(startposition,size-1)
      size -=1
      position -= elem
      heapArray(size)=null.asInstanceOf[T]
      pushDown(pushUp(startposition))
    }
  }

  /**
   * removes one elem from the heap if present
   * @param elem
   * @return trus if it was in the heap, false otherwise
   */
  def deleteIfPresent(elem:T):Boolean = {
    position.get(elem) match{
      case None => false
      case Some(startposition) =>
        delete(elem)
        true
    }
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 * @param maxId
 */
class ArrayMap(maxId:Int) extends scala.collection.mutable.Map[Int, Long]{
  
  val array:Array[Long] = Array.fill[Long](maxId)(-1)
  def get(key: Int): Option[Long] =  {
    val v = array(key)
    if(v == -1) None
    else Some(array(key))
  }

  override def contains(key : Int) : Boolean = array(key) != -1

  def iterator: Iterator[(Int, Long)] = {throw new Exception("enumeration not supported"); null}

  def +=(kv: (Int, Long)): this.type = {
    array(kv._1) = kv._2
    this
  }

  def -=(key: Int): this.type = {
    array(key) = -1
    this
  }
}

/**
 * beware that this heap does not delete references that are passed to it, so GC will not be able to recover this space.
 * it does not hurt if you only use this datastruct to store object that are permanently living in you memory anyway
 * @author renaud.delandtsheer@cetic.be
 * @param getKey
 * @param maxsize
 * @param position
 * @param A
 * @param X
 * @tparam T
 */
class BinomialHeapWithMoveExtMem[T](getKey:T => Long, val maxsize:Int, position:scala.collection.mutable.Map[T,Int])(implicit val A:Ordering[T], implicit val X:Manifest[T]){

  private[this] val heapArray:Array[T] = new Array[T](maxsize)
  var size:Int=0

  def checkInternals(): Unit = {
    for(i <- heapArray.indices if i < size-1){
      if (leftChild(i) < size){
        require(getKey(heapArray(i)) <= getKey(heapArray(leftChild(i))),"heap error " + this + i)
        require(father(leftChild(i)) == i,"heap error " + this)
      }
      if (rightChild(i) < size){
        require(getKey(heapArray(i)) <= getKey(heapArray(rightChild(i))),"heap error " + this)
        require(father(rightChild(i)) == i,"heap error " + this)
      }
    }
  }

  def isEmpty:Boolean = size == 0

  override def toString:String = {
    heapArray.toList.toString
  }

  def insert(elem:T): Unit = {
    //insert en derniere position, puis bubble up
    heapArray(size)=elem
    position +=((elem,size))
    size +=1
    pushUp(size-1)
  }

  def getElements:Iterable[T] = {
    position.keys
  }

  def contains(value:T):Boolean = position.contains(value)

  private def swapPositions(position1:Int,position2:Int): Unit = {
    position+=((heapArray(position1),position2))
    position+=((heapArray(position2),position1))

    val tmp:T = heapArray(position1)
    heapArray(position1)=heapArray(position2)
    heapArray(position2)=tmp
  }

  //returns the last position of the moved item
  private def pushDown(startposition:Int):Int = {
    var position = startposition
    val positionKey = getKey(heapArray(position))
    while(true)
      if(leftChild(position) < size && positionKey > getKey(heapArray(leftChild(position)))){
        //examiner aussi left child
        if(rightChild(position) < size && getKey(heapArray(rightChild(position))) < getKey(heapArray(leftChild(position)))){
          //c'est avec le right child qu'il faut inverser
          swapPositions(position,rightChild(position))
          position = rightChild(position)
        }else{
          //c'est avec le left chile qu'il faut inverser
          swapPositions(position,leftChild(position))
          position = leftChild(position)
        }
      }else if(rightChild(position) < size && positionKey > getKey(heapArray(rightChild(position)))){
        //only consider right child
        swapPositions(position,rightChild(position))
        position = rightChild(position)
      }else{
        return position
      }
    position //jamais execute
  }

  private def pushUp(startposition:Int):Int = {
    var position = startposition
    val positionKey = getKey(heapArray(position))
    while(true){
      val fatherposition:Int = father(position)
      if (fatherposition >= 0 && positionKey < getKey(heapArray(fatherposition))){
        swapPositions(position,fatherposition)
        position = fatherposition
      }else{
        return position
      }
    }
    position //never reached
  }

  @inline
  private def leftChild(position:Int):Int = (position+1)*2-1
  @inline
  private def rightChild(position:Int):Int =(position+1)*2
  @inline
  private def father(position:Int):Int =  (position-1)/2

  def getFirsts:List[T] = {
    def ExploreFirsts(value:Long,startposition:Int,acc:List[T]):List[T] = {
      if(startposition < size && getKey(heapArray(startposition)) == value){
        val acc1 = ExploreFirsts(value,leftChild(startposition),heapArray(startposition) :: acc)
        ExploreFirsts(value,rightChild(startposition),acc1)
      }else{
        acc
      }
    }
    if(size == 0)List.empty
    else ExploreFirsts(getKey(heapArray(0)),0,List.empty)
  }

  def getFirst:T=heapArray(0)

  def removeFirst():T={
    val toreturn:T = heapArray(0)
    swapPositions(0,size-1)
    size -=1
    position -= toreturn
    pushDown(0)
    toreturn
  }

  def notifyChange(elem:T): Unit = {
    val startposition = position(elem)
    pushDown(pushUp(startposition))
  }

  def delete(elem:T): Unit ={
    val startposition:Int = position(elem)
    if (startposition == size-1){
      size -=1
      position -= elem
    }else{
      swapPositions(startposition,size-1)
      size -=1
      position -= elem
      pushDown(pushUp(startposition))
    }
  }

  /**
   * removes one elem from the heap if present
   * @param elem
   * @return trus if it was in the heap, false otherwise
   */
  def deleteIfPresent(elem:T):Boolean = {
    if(contains(elem)){
      delete(elem)
      true
    }else false
  }
}



/**
 * beware that this heap does not delete references that are passed to it, so GC will not be able to recover this space.
 * it does not hurt if you only use this datastruct to store object that are permanently living in you memory anyway
 * @author renaud.delandtsheer@cetic.be
 * @param getKey
 * @param maxsize
 * @param maxKey
 */
class BinomialHeapWithMoveInt(getKey:Int => Int,val maxsize:Int, val maxKey:Int){
  private[this] val heapArray:Array[Int] = new Array[Int](maxsize)

  private[this] val position:Array[Int] = Array.fill[Int](maxKey+1)(-1)

  var size:Int=0

  def checkInternals(): Unit ={
    for(i <- heapArray.indices if i < size-1L){
      if (leftChild(i) < size){
        require(getKey(heapArray(i)) <= getKey(heapArray(leftChild(i))),"heap error " + this + i)
        require(father(leftChild(i)) == i,"heap error " + this)
      }
      if (rightChild(i) < size){
        require(getKey(heapArray(i)) <= getKey(heapArray(rightChild(i))),"heap error " + this)
        require(father(rightChild(i)) == i,"heap error " + this)
      }
    }
  }

  def isEmpty:Boolean = size == 0

  override def toString:String = {
    heapArray.toList.toString
  }

  def insert(elem:Int): Unit = {
    //insert en derniere position, puis bubble up
    heapArray(size)=elem
    position(elem) = size
    size +=1
    pushUp(size-1)
  }

  def contains(value:Int):Boolean = position(value) != -1

  private def swapPositions(position1:Int,position2:Int): Unit ={
    position(heapArray(position1)) = position2
    position(heapArray(position2)) = position1

    val tmp:Int = heapArray(position1)
    heapArray(position1)=heapArray(position2)
    heapArray(position2)=tmp
  }

  //returns the last position of the moved item
  private def pushDown(startposition:Int):Int = {
    var position = startposition
    val positionKey = getKey(heapArray(position))
    while(true)
      if(leftChild(position) < size && positionKey > getKey(heapArray(leftChild(position)))){
        //examiner aussi left child
        if(rightChild(position) < size && getKey(heapArray(rightChild(position))) < getKey(heapArray(leftChild(position)))){
          //c'est avec le right child qu'il faut inverser
          swapPositions(position,rightChild(position))
          position = rightChild(position)
        }else{
          //c'est avec le left chile qu'il faut inverser
          swapPositions(position,leftChild(position))
          position = leftChild(position)
        }
      }else if(rightChild(position) < size && positionKey > getKey(heapArray(rightChild(position)))){
        //only consider right child
        swapPositions(position,rightChild(position))
        position = rightChild(position)
      }else{
        return position
      }
    require(false) //jamais execute
    position
  }

  private def pushUp(startposition:Int):Int = {
    var position = startposition
    val positionKey = getKey(heapArray(position))
    while(true){
      val fatherposition:Int = father(position)
      if (fatherposition >= 0 && positionKey < getKey(heapArray(fatherposition))){
        swapPositions(position,fatherposition)
        position = fatherposition
      }else{
        return position
      }
    }
    position //never reached
  }

  @inline
  private def leftChild(position:Int):Int = (position+1)*2-1
  @inline
  private def rightChild(position:Int):Int = (position+1)*2
  @inline
  private def father(position:Int):Int = (position-1)/2

  def getFirsts:List[Int] = {
    def ExploreFirsts(value:Int,startposition:Int,acc:List[Int]):List[Int] = {
      if(startposition < size && getKey(heapArray(startposition)) == value){
        val acc1 = ExploreFirsts(value,leftChild(startposition),heapArray(startposition) :: acc)
        ExploreFirsts(value,rightChild(startposition),acc1)
      }else{
        acc
      }
    }
    if(size == 0)List.empty
    else ExploreFirsts(getKey(heapArray(0)),0,List.empty)
  }

  def getFirst:Int=heapArray(0)

  /**
    *
    * removes the smallest element and returns its value
    * @return
    */
  def removeFirst():Int={
    val toreturn:Int = heapArray(0)
    swapPositions(0,size-1)
    size -=1
    position(toreturn) = -1
    pushDown(0)
    toreturn
  }

  def notifyChange(elem:Int): Unit = {
    val startposition = position(elem)
    pushDown(pushUp(startposition))
  }

  def delete(elem:Int): Unit = {
    val startposition:Int = position(elem)
    if (startposition == size-1){
      size -=1
      position(elem) = -1
    }else{
      swapPositions(startposition,size-1)
      size -=1
      position(elem) = -1
      pushDown(pushUp(startposition))
    }
  }

  /**
   * removes one elem from the heap if present
   * @param elem
   * @return trus if it was in the heap, false otherwise
   */
  def deleteIfPresent(elem:Int):Boolean = {
    if(contains(elem)){
      delete(elem)
      true
    }else false
  }
}




/**
  * beware that this heap does not delete references that are passed to it, so GC will not be able to recover this space.
  * it does not hurt if you only use this datastruct to store object that are permanently living in you memory anyway
  * @author renaud.delandtsheer@cetic.be
  * @param getKey
  * @param maxsize
  * @param maxKey
  */
class BinomialHeapWithMoveLong(getKey:Int => Long,val maxsize:Int, val maxKey:Int){
  private[this] val heapArray:Array[Int] = new Array[Int](maxsize)

  private[this] val position:Array[Int] = Array.fill[Int](maxKey+1)(-1)

  var size:Int=0

  def checkInternals(): Unit = {
    for(i <- heapArray.indices if i < size-1L){
      if (leftChild(i) < size){
        require(getKey(heapArray(i)) <= getKey(heapArray(leftChild(i))),"heap error " + this + i)
        require(father(leftChild(i)) == i,"heap error " + this)
      }
      if (rightChild(i) < size){
        require(getKey(heapArray(i)) <= getKey(heapArray(rightChild(i))),"heap error " + this)
        require(father(rightChild(i)) == i,"heap error " + this)
      }
    }
  }

  def isEmpty:Boolean = size == 0

  override def toString:String = {
    heapArray.toList.toString
  }

  // TODO Should add warning in case of duplicated value. This heap **cannot** contain duplicated values.
  def insert(elem:Int): Unit = {
    //insert en derniere position, puis bubble up
    heapArray(size)=elem
    position(elem) = size
    size +=1
    pushUp(size-1)
  }

  def contains(value:Int):Boolean = position(value) != -1

  private def swapPositions(position1:Int,position2:Int): Unit = {
    position(heapArray(position1)) = position2
    position(heapArray(position2)) = position1

    val tmp:Int = heapArray(position1)
    heapArray(position1)=heapArray(position2)
    heapArray(position2)=tmp
  }

  //returns the last position of the moved item
  private def pushDown(startposition:Int):Int = {
    var position = startposition
    val positionKey = getKey(heapArray(position))
    while(true)
      if(leftChild(position) < size && positionKey > getKey(heapArray(leftChild(position)))){
        //examiner aussi left child
        if(rightChild(position) < size && getKey(heapArray(rightChild(position))) < getKey(heapArray(leftChild(position)))){
          //c'est avec le right child qu'il faut inverser
          swapPositions(position,rightChild(position))
          position = rightChild(position)
        }else{
          //c'est avec le left chile qu'il faut inverser
          swapPositions(position,leftChild(position))
          position = leftChild(position)
        }
      }else if(rightChild(position) < size && positionKey > getKey(heapArray(rightChild(position)))){
        //only consider right child
        swapPositions(position,rightChild(position))
        position = rightChild(position)
      }else{
        return position
      }
    require(false) //jamais execute
    position
  }

  private def pushUp(startposition:Int):Int = {
    var position = startposition
    val positionKey = getKey(heapArray(position))
    while(true){
      val fatherposition:Int = father(position)
      if (fatherposition >= 0 && positionKey < getKey(heapArray(fatherposition))){
        swapPositions(position,fatherposition)
        position = fatherposition
      }else{
        return position
      }
    }
    position //never reached
  }

  @inline
  private def leftChild(position:Int):Int = (position+1)*2-1
  @inline
  private def rightChild(position:Int):Int = (position+1)*2
  @inline
  private def father(position:Int):Int = (position-1)/2

  def getFirsts:List[Int] = {
    def ExploreFirsts(key:Long,startposition:Int,acc:List[Int]):List[Int] = {
      if(startposition < size && getKey(heapArray(startposition)) == key){
        val acc1 = ExploreFirsts(key,leftChild(startposition),heapArray(startposition) :: acc)
        ExploreFirsts(key,rightChild(startposition),acc1)
      }else{
        acc
      }
    }
    if(size == 0)List.empty
    else ExploreFirsts(getKey(heapArray(0)),0,List.empty)
  }

  def getFirst:Long=heapArray(0)

  /**
    *
    * removes the smallest element and returns its value
    * @return
    */
  def removeFirst():Int={
    val toreturn:Int = heapArray(0)
    swapPositions(0,size-1)
    size -=1
    position(toreturn) = -1
    pushDown(0)
    toreturn
  }

  def notifyChange(elem:Int): Unit = {
    val startposition = position(elem)
    pushDown(pushUp(startposition))
  }

  def delete(elem:Int): Unit = {
    require(size > 0, "Attempt to delete on empty heap")
    val startposition:Int = position(elem)
    if (startposition == size-1){
      size -=1
      position(elem) = -1
    }else{
      swapPositions(startposition,size-1)
      size -=1
      position(elem) = -1
      pushDown(pushUp(startposition))
    }
  }

  /**
    * removes one elem from the heap if present
    * @param elem
    * @return trus if it was in the heap, false otherwise
    */
  def deleteIfPresent(elem:Int):Boolean = {
    if(contains(elem)){
      delete(elem)
      true
    }else false
  }
}
