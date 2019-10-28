package oscar.cbls.algo.magicArray

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

import oscar.cbls.algo.rb.RedBlackTreeMap

import scala.reflect.ClassTag
import scala.util.Random

object ImmutableArray{
  def createFromBaseArrayNeverModified[T:ClassTag](baseValueNeverModified:Array[T]):ImmutableArray[T] = {
    new ImmutableArray[T](baseValueNeverModified,
      baseValueNeverModified.length,
      RedBlackTreeMap.empty[T])
  }
  def createAndImportBaseValues[T:ClassTag](baseValues:Iterable[T]):ImmutableArray[T] = {
    val size = baseValues.size
    val it = baseValues.iterator
    createFromBaseArrayNeverModified(Array.tabulate[T](size)(id => it.next()))
  }
}

class ImmutableArray[T:ClassTag](baseValueNeverModified:Array[T],
                        override val size:Int,
                        updates:RedBlackTreeMap[T]) extends Iterable[T]{
  def apply(id: Int): T =
    if(id >= size) throw new ArrayIndexOutOfBoundsException
    else updates.getOrElse(id,baseValueNeverModified(id))

  def update(id: Long, value: T, fast: Boolean): ImmutableArray[T] = {
    val tmp = if(id == size) new ImmutableArray[T](baseValueNeverModified,size+1,updates.insert(id,value))
    else if (id < size) new ImmutableArray[T](baseValueNeverModified,size,updates.insert(id,value))
    else throw new ArrayIndexOutOfBoundsException
    if(fast) tmp else tmp.flatten()
  }

  def flatten():ImmutableArray[T] = new ImmutableArray(Array.tabulate[T](size)(id => this.apply(id)), size, RedBlackTreeMap.empty[T])

  override def iterator: Iterator[T] = new ImmutableArrayIterator[T](this)
}

class ImmutableArrayIterator[T](on:ImmutableArray[T])extends Iterator[T]{
  var nextPos = 0

  override def hasNext: Boolean = nextPos < on.size

  override def next(): T = {
    val toReturn = on(nextPos)
    nextPos+=1
    toReturn
  }
}
