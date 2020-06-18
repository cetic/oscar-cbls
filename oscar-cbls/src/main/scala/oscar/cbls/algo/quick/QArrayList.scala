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
package oscar.cbls.algo.quick

class QArrayList[@specialized T](initialLength:Int)(implicit val X:Manifest[T]) {
  private[this] var internalArray:Array[T] = new Array[T](initialLength)
  private[this] var size:Int = 0
  private[this] var maxSize = initialLength
  private[this] val nullT = null.asInstanceOf[T]

  def put(elem:T): Unit ={
    if(size == maxSize) {
      //need to double the size of the array

      maxSize *= 10
      val newArray = new Array[T](maxSize)

      var toMove = size
      while (toMove != 0) {
        toMove -= 1
        newArray(toMove) = internalArray(toMove)
      }

      internalArray = newArray
    }
    internalArray(size) = elem
    size += 1
  }

  /**returns null if was empty*/
  def pop():T = {
    if(size == 0){
      nullT
    }else{
      size -=1
      internalArray(size)
    }
  }

  def isEmpty:Boolean = size == 0

  def setEmpty(): Unit ={
    size = 0
  }
}
