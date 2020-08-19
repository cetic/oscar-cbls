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

object IterableMagicBoolArray {
  /**
    * create a Magical Array Of Boolean of given length
    * @param n the length
    * @return a Magical Array Of Boolean or null if length is less than zero
    */
  def apply(n:Int,initVal:Boolean = false):IterableMagicBoolArray ={
    require(n >= 0, "cannot create magic array of negative size")
    new IterableMagicBoolArray(n,initVal)
  }
}

class IterableMagicBoolArray(override val length:Int, initVal:Boolean = false)
  extends MagicBoolArray(length,initVal){

  private var positionsAtTrueOverApproximated:QList[Int] = _
  private val isPositionInOverApproximationQList:MagicBoolArray = MagicBoolArray(length)
  private var overApproximationIsComplete:Boolean = initVal

  private var anyIndividualSetToFalse:Boolean = false

  private var nbTrue:Int = if(initVal) length else 0

  val nbIndicesAtTrue:Int = nbTrue

  /**
    * Sets the value of each element to "value"
    * @note complexity is O(1)
    */
  override def all_= (value : Boolean) : Unit = {
    super.all_=(value)
    if(value){
      positionsAtTrueOverApproximated = null
      isPositionInOverApproximationQList.all_=(false)
      overApproximationIsComplete = true
    }else{
      positionsAtTrueOverApproximated = null
      isPositionInOverApproximationQList.all_=(false)
      overApproximationIsComplete = false
    }
    nbTrue= if(value) length else 0
  }

  override def update(id : Int, value : Boolean):Boolean = {
    val oldValue = super.update(id, value)
    if(value){
      if(!oldValue){
        if (!overApproximationIsComplete) {
          if (!isPositionInOverApproximationQList.update(id,value = true)) {
            positionsAtTrueOverApproximated = QList(id, positionsAtTrueOverApproximated)
          }
        }
        nbTrue += 1
      }
    }else{
      if(oldValue){
        anyIndividualSetToFalse = true
        nbTrue -= 1
      }
    }
    oldValue
  }

  override def indicesAtTrue : Iterator[Int] = {
    if(overApproximationIsComplete){
      if(anyIndividualSetToFalse){
        super.indicesAtTrue
      }else{
        indices.iterator
      }
    }else{
      if(anyIndividualSetToFalse){
        positionsAtTrueOverApproximated.filter(this(_)).iterator
      }else{
        positionsAtTrueOverApproximated.iterator
      }
    }
  }

  override def indicesAtTrueAsQList: QList[Int] = {
    if(overApproximationIsComplete){
      if(anyIndividualSetToFalse){
        super.indicesAtTrueAsQList
      }else{
        QList.nonReversedBuildFromIterable(indices)
      }
    }else{
      if(anyIndividualSetToFalse && positionsAtTrueOverApproximated != null){
        positionsAtTrueOverApproximated.qFilter(this(_))
      }else{
        positionsAtTrueOverApproximated
      }
    }
  }
}
