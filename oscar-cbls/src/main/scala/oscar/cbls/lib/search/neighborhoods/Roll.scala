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
import oscar.cbls.core.computation.{CBLSIntVar, InvariantHelper}
import oscar.cbls.core.search.{EasyNeighborhood, Move}

import scala.annotation.tailrec

/**
 * This neighborhood will consider roll moves that roll the value of contiguous CBLSIntVar in the given array
 *
 * @param vars an array of [[oscar.cbls.core.computation.CBLSIntVar]] defining the search space
 * @param searchZone a subset of the indices of vars to consider for the roll
 *                   If none is provided, all the array will be considered each time
 * @param bridgeOverFrozenVariables if false, contiguous variables are the ones that are adjacent in the array,
 *                                  so that if a variable is not in the search zone,
 *                                  no roll can involve vars on its left and on its right
 *                                  if true, variable in the search zone will simply be ignored
 * @param maxShiftSize the max size of the roll, given the first indice considered in the roll
 * @param name the name of the neighborhood
 * @param hotRestart  if true, the exploration order in case you ar not going for the best
 *                    is a hotRestart for the first swapped variable
 *                    even if you specify a searchZone that is: the exploration starts again
 *                    at the position where it stopped, and consider the indices in increasing order
 *                    if false, consider the exploration range in natural order from the first position.
 *  @param checkForDifferentValues if true, will check that vars involved in roll have different values before exploring
 **/
//TODO: also implement minimal roll size (we prefer to use swap instead of roll)
case class RollNeighborhood(vars:Array[CBLSIntVar],
                            name:String = "RollNeighborhood",
                            searchZone:()=>Set[Int] = null,
                            bridgeOverFrozenVariables:Boolean = false,
                            maxShiftSize:Int=>Int = _ => Int.MaxValue, //the max size of the roll, given the ID of the first variable
                            //minRollSize:Long, //TODO
                            checkForDifferentValues:Boolean = false,
                            best:Boolean = false,
                            hotRestart:Boolean = true)
  extends EasyNeighborhood[RollMove](best,name){
  //the indice to start with for the exploration
  var startIndice:Int = 0

  override def exploreNeighborhood(): Unit ={

    val searchZoneObject = if(searchZone == null) null else searchZone()
    val currentSearchZone = if(searchZone == null) vars.indices else searchZoneObject

    @inline
    def searchZoneContains(i:Int):Boolean = {
      if(searchZone == null) vars.indices.contains(i) else searchZoneObject.contains(i)
    }

    val firstIndices = if (hotRestart && !best)
      HotRestart(currentSearchZone, startIndice)
    else currentSearchZone

    for (firstIndice: Int <- firstIndices) {

      val currentMaxShiftSize = maxShiftSize(firstIndice)

      var currentEnd = firstIndice
      var currentRollSize = 1
      currentRollCluster = List(vars(currentEnd))
      initValue = List(vars(currentEnd).value)

      @tailrec
      def advance(lastIndice:Int):(Int,Boolean) = {
        if(currentRollSize >= currentMaxShiftSize) return (0,false)
        val potentialAdd = currentEnd+1
        if(potentialAdd >= vars.length) return (0,false)
        if(searchZoneContains(potentialAdd)){
          (potentialAdd,true)
        }else if(bridgeOverFrozenVariables){
          advance(potentialAdd)
        }else{
          (0,false)
        }
      }

      var (newEnd,isNewEnd) = advance(currentEnd)
      while(isNewEnd){
        //updating roll
        currentEnd = newEnd
        currentRollSize +=1
        currentRollCluster = vars(currentEnd) :: currentRollCluster
        initValue = vars(currentEnd).value :: initValue

        val shouldExplore = if (checkForDifferentValues){
          val (minBound,maxBound) = InvariantHelper.getMinMaxBoundsInt(initValue)
          minBound != maxBound
        } else true

        if(shouldExplore) {
          //performing rolls
          rollOffset = 1
          while (rollOffset < currentRollSize) {
            //check this roll
            doRollOneLeft()
            if (evaluateCurrentMoveObjTrueIfStopRequired(obj.value)) {
              startIndice = advance(firstIndice)._1
              assignAll(currentRollCluster, initValue)
              return
            }
            rollOffset += 1
          }
          assignAll(currentRollCluster, initValue)
        }

        val tmp = advance(currentEnd)
        newEnd = tmp._1
        isNewEnd = tmp._2
      }
    }
  }

  var rollOffset:Int = 0
  var currentRollCluster:List[CBLSIntVar] = List.empty
  var initValue:List[Long] = List.empty

  def doRollOneLeft(): Unit ={
    val i = currentRollCluster.head.value
    var currentPos = currentRollCluster
    while(true){
      currentPos match{
        case h1::h2::t =>
          h1 := h2.value
          currentPos = h2 :: t
        case h1 :: Nil =>
          h1 := i
          return
        case Nil => throw new Error("empty roll?")
      }
    }
  }

  def assignAll(vars:List[CBLSIntVar],vals:List[Long]): Unit ={
    (vars, vals) match {
      case (hVar :: t1, hVal :: t2) =>
        hVar := hVal
        assignAll(t1,t2)
      case (Nil,Nil) => // Do nothing
      case _ => throw new Error("bug in assignAll")
    }
  }

  override def instantiateCurrentMove(newObj: Long) =
    RollMove(currentRollCluster,rollOffset,newObj,name)

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}

case class RollMove(l:List[CBLSIntVar],offset:Int, override val objAfter:Long, override val neighborhoodName:String = null)
  extends Move(objAfter,neighborhoodName){
  /** to actually take the move */
  override def commit(): Unit ={
    val variables = l.toArray
    val initialValues:Array[Long] = variables.map(_.value)
    for(i <- variables.indices){
      variables(i) := initialValues((i+offset) % variables.length)
    }
  }

  override def toString: String = {
    s"${neighborhoodNameToString}RollMove($l, offset:$offset$objToString)"
  }

}
