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
package oscar.cbls.algo.magicArray

import oscar.cbls.algo.quick.QList

object MagicBoolArray {
  /**
   * create a Magical Array Of Boolean of given length
   * @param n the length
   * @return a Magical Array Of Boolean or null if length is less than zero
   */
  def apply(n:Int,initVal:Boolean = false):MagicBoolArray ={
    require(n >= 0L, "cannot create magic array of negative size")
    new MagicBoolArray(n,initVal)
  }
}

/**
 * This represents an array of boolean with O(1) setAll and O(1) clearAll
 * @author Jannou Brohée on 3/10/16.
 * @param length Maximum length of magical array
 */
class MagicBoolArray(val length:Int,initVal:Boolean = false){

  private[this] val threshold:Long = Long.MaxValue-10L

  // Made public for testing purposes
  var global:Long = 1L

  private[this] val internalArray:Array[Long] = Array.fill[Long](length)(if(initVal) 1L else 0L)

  val indices: Range = 0 until length

  /**
   * Set the new value of element at specific index
   * @param id the index of the element
   * @param value the new element's value (true/false)
   * @return the old value
   * @note in O(1) // trivial
   */
  def update(id:Int, value:Boolean):Boolean = {
    assert(id<length && 0<=id)
    val oldInternalArray = internalArray(id)
    if(value) internalArray(id)=global
    else internalArray(id)=global-1L
    oldInternalArray>=global
  }

  /**
   * Return the value of the element at specific index
   * @param id the index of the element
   * @return true or false
   * @note complexity is O(1)
   */
  def apply(id:Int): Boolean ={
    require(0<=id && id<length, "got id:" + id + "length:" + length)
    internalArray(id)>=global
  }

  /**
   * Sets the value of each element to "value"
   * @note complexity is O(1)
   */
  def all_= (value:Boolean): Unit ={
    if(value) {
      if (Math.abs(global) == threshold) {
        global = 0L
        resetArray()
      } else {
        global = -Math.abs(global)-1L
      }
    }else{
      if(Math.abs(global)==threshold){
        global = 1L
        resetArray()
      }else{
        global = Math.abs(global)+1L
      }
    }
  }

  @inline
  private [this] def resetArray(): Unit ={
    var i = internalArray.length
    while(i > 0){
      i -= 1
      internalArray(i) = 0L
    }
  }

  def all:Boolean = ???

  /**
   * Creates a new iterator over the indexes of elements which value is true.
   * this is a O(this.length) method
   * @return the new iterator
   */
  def indicesAtTrue:Iterator[Int] ={
    var toReturn:QList[Int]=null
    for(n <-0 until length){
      if(internalArray(n)>=global){
        toReturn = QList(n,toReturn)
      }
    }
    toReturn.iterator
  }

  def indicesAtTrueAsQList:QList[Int] ={
    var toReturn:QList[Int]=null
    for(n <-0 until length){
      if(internalArray(n)>=global){
        toReturn = QList(n,toReturn)
      }
    }
    toReturn
  }

  override def toString: String = s"[${indicesAtTrue.mkString(",")}]"
}
