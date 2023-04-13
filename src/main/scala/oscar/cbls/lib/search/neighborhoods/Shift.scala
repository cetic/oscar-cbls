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

package oscar.cbls.lib.search.neighborhoods

import oscar.cbls.algo.search.HotRestart
import oscar.cbls.core.computation.CBLSIntVar
import oscar.cbls.core.search.{Move, EasyNeighborhood}

/**
 * will shift a block of value to the right(doing it also to the left is redundant)
 *
 * @param vars an array of [[oscar.cbls.core.computation.CBLSIntVar]] defining the search space
 * @param searchZone1 a subset of the indices of vars to consider in order to determine the block's extremities
 *                   if none provided, all the array will be considered each time
 * @param maxShiftSize the max size of the shift, given the first indice considered in the shift
 * @param maxOffsetLength the max length of the movement to perform
 * @param best if true, the neighborhood will try to find the best solution possible
 *             (not very usefull because browsing all the possibilities can be very long)
 * @param name the name of the neighborhood
 * @param hotRestart  if true, the exploration order in case you ar not going for the best
 *                    is a hotRestart for the first swapped variable
 *                    even if you specify a searchZone that is: the exploration starts again
 *                    at the position where it stopped, and consider the indices in increasing order
 *                    if false, consider the exploration range in natural order from the first position.
 * @author fabian.germeau@cetic.be
 **/
case class ShiftNeighborhood(vars:Array[CBLSIntVar],
                             name:String = "ShiftNeighborhood",
                             searchZone1:()=>Iterable[Int] = null,
                             maxShiftSize:Int = Int.MaxValue,
                             maxOffsetLength:Int = Int.MaxValue,
                             best:Boolean = false,
                             hotRestart: Boolean = true)
  extends EasyNeighborhood[ShiftMove](best,name){
  /**
   * This is the method you must implement and that performs the search of your neighborhood.
   * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
   * as explained in the documentation of this class
   */

  var startIndice:Int = 0
  var currentShiftOffset:Int = 0
  var currentShiftSize:Int = 1
  var currentStart:Int = 0

  override def exploreNeighborhood(): Unit ={
    val searchZoneObject = if(searchZone1 == null)null else searchZone1()
    val currentSearchZone = if(searchZone1 == null) vars.indices else searchZoneObject

    val firstIndices =
      if(hotRestart && !best)HotRestart(currentSearchZone, startIndice)
      else currentSearchZone

    val initialValues: Array[Long] = vars.map(_.value)

    /*We first determine the left border of the shift block, then we determine the offset value
    * and finally the right border value.
    * The next part of the idea is to go back to the original sequence only if we have found a suitable solution or
    * if we have to augment the offset value.
    * This way the complexity of augmenting the size of the block shifted is O(offset+1)
    * rather than O((offset+length) * 2L)*/

    for(firstIndice: Int <- firstIndices){
      currentStart = firstIndice
      for(i <- Math.max(-currentStart,-maxOffsetLength) to Math.min(vars.length-2,maxOffsetLength)){
        var modifHasOccured = false
        if(i != 0) {
          currentShiftOffset = i
          for(secondIndice: Int <- firstIndice until currentSearchZone.size){
            if(secondIndice - firstIndice <= Math.min(maxShiftSize-1,vars.length-1-currentShiftOffset-firstIndice)){
              currentShiftSize = secondIndice - currentStart + 1
              val newObj = doSmartShiftNeighborhood()
              modifHasOccured = true
              if (evaluateCurrentMoveObjTrueIfStopRequired(newObj)) {
                startIndice = (currentStart + 1) % vars.length
                undoSmartShiftNeighborhood()
                return
              }
            }
          }
          if(modifHasOccured) undoSmartShiftNeighborhood()
        }
      }
    }

    def undoSmartShiftNeighborhood(): Unit ={
      if(currentShiftOffset > 0) {
        for (i <- currentStart until currentStart + currentShiftOffset + currentShiftSize) {
          vars(i) := initialValues(i)
        }
      }else{
        for (i <- currentStart + currentShiftOffset until currentStart + currentShiftSize){
          vars(i) := initialValues(i)
        }
      }
    }

    def doSmartShiftNeighborhood(): Long ={
      //If the block is moved on the right
      if(currentShiftOffset > 0){
        //The values are changed
        val tempVal = vars(currentStart + currentShiftOffset + currentShiftSize - 1).value
        vars(currentStart + currentShiftOffset + currentShiftSize - 1) := vars(currentStart).value
        if(currentShiftOffset != 1){
          for (i <- currentStart to currentStart + currentShiftOffset - 2) {
            vars(i) := vars(i + 1).value
          }
        }
        vars(currentStart + currentShiftOffset - 1) := tempVal
      }
      //If the block is moved on the left
      else{
        //The values are changed
        val tempVal = vars(currentStart + currentShiftSize - 1).newValue
        if(currentShiftOffset == -1){
          vars(currentStart + currentShiftSize - 1) := vars(currentStart + currentShiftOffset + currentShiftSize -1).newValue
          vars(currentStart + currentShiftOffset + currentShiftSize - 1) := tempVal
        }else {
          for (i <- currentStart + currentShiftSize - 1 to currentStart + currentShiftSize + currentShiftOffset by -1) {
            vars(i) := vars(i - 1).value
          }
          vars(currentStart + currentShiftOffset + currentShiftSize - 1) := tempVal
        }
      }
      // Return the (possibly changed) obj.value
      obj.value
    }
  }

  override def instantiateCurrentMove(newObj: Long) = ShiftMove(currentStart,currentShiftSize,currentShiftOffset,vars,newObj,name)

  override def reset(): Unit = {
    startIndice = 0
  }
}

/** standard move that switch a block of value to the right
  *
  * @param startIndice the indice of the item beginning the block to switch
  * @param length the length of the block to switch
  * @param offset the size off the movement that has to be performed to the right
  * @param objAfter the objective after this assignation will be performed
  * @param neighborhoodName the name of the neighborhood that generated this move, used for pretty printing purpose.
  *                         Notice that the name is not the type of the neighborhood.
  * @author fabian.germeau@cetic.be
  * */
case class ShiftMove(startIndice:Int,length:Int,offset:Int,variables:Array[CBLSIntVar], override val objAfter:Long, override val neighborhoodName:String = null)
  extends Move(objAfter,neighborhoodName){

  def shiftedElements: Range = startIndice to startIndice + length
  override def toString: String = {
    s"${neighborhoodNameToString}ShiftMove(startIndice:$startIndice; length:$length; offset:$offset$objToString)"
  }

  /** to actually take the move */
  override def commit(): Unit = {
    val initialValues: Array[Long] = Array.tabulate(variables.length)(variables(_).value)
    //If the block is moved on the right
    if(offset > 0){
      //The values are changed
      for(i <- startIndice until startIndice + offset + length){
        if(i < startIndice + offset){
          variables(i) := initialValues(i + length)
        }
        else{
          variables(i) := initialValues(i - offset)
        }
      }
    }
    //If the block is moved on the left
    else{
      //The values are changed (and don't forget, here offset is negative
      for(i <- startIndice + offset until startIndice + length){
        if(i < startIndice + offset + length){
          variables(i) := initialValues(i - offset)
        }
        else{
          variables(i) := initialValues(i - length)
        }
      }
    }
  }
}
