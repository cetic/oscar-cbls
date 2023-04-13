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
import oscar.cbls.core.computation.{CBLSIntVar, Variable}
import oscar.cbls.core.search.{EasyNeighborhoodMultiLevel, First, LoopBehavior, Move}

/**
  * This neighborhood searches for moves of the form
  * vars(i) :+= delta*factor1   vars(j) :+= delta*factor2
  *
  * where:
  *
  * i is explored in the outer loop, specified by searchZone1
  * and First or Best are selected by selectFirstVariableBehavior
  *
  * j is explored in the inner loop, specified by searchZone2
  * and First or Best are selected by selectSecondVariableBehavior
  *
  * delta is explored through numeric method specified by searchZoneForDelta
  * so delta is not explored exhaustively, unlike i and j.
  *
  * factor1 and factor2 are specified through appliedFactors; by default we use (1,1)
  *
  * @param vars the array of vars taht is searched
  * @param name the name of the neighborhood, used for console printing
  * @param searchZone1 the set of indices to explore for i.
  * @param searchZone2 the set of indices to explore for j,
  *                    which is determined based on the current indice of the frist variable, and its value.
  * @param appliedFactors given variableID1,value1,variableID2,value2 you can specify factor1,factor2
  *                       both are expected to be > 0 (notice that the delta can be negative)
  * @param searchZoneForDelta specifies the numeric method to use for searching the values for delta.
  *                           you are given (firstVarIndice,oldValOfFirstVar)(secondVarIndice,oldValOfSecondVar) and ou return the linear optimization method
  * @param symmetryCanBeBrokenOnIndices trus if the neighborhood has to ensure that i < j
  *                                     default is true
  * @param selectFirstVariableBehavior specifies First or Best for i
  * @param selectSecondVariableBehavior specifies First or Best for j
  * @param hotRestart true if a hot restart is to be used on the first varaible selection, false otherwise.
  *                   default is true
  */
case class TransferNeighborhood(vars:Array[CBLSIntVar],
                                name:String = "TransferNeighborhood",
                                searchZone1:()=>Iterable[Int] = null,
                                searchZone2:() => (Int,Int)=>Iterable[Int] = null,
                                appliedFactors:() => (Int,Long,Int,Long) => (Long,Long) = () => (_,_,_,_) => (1L,1L),
                                searchZoneForDelta:() => (Long,Long) => (Long,Long) => LinearOptimizer,
                                symmetryCanBeBrokenOnIndices:Boolean = true,
                                selectFirstVariableBehavior:LoopBehavior = First(),
                                selectSecondVariableBehavior:LoopBehavior = First(),
                                hotRestart:Boolean = true)
  extends EasyNeighborhoodMultiLevel[TransferMove](name){

  //the indice to start with for the exploration
  var firstVarIndice:Int = 0
  var firstVar:CBLSIntVar = null
  var oldValOfFirstVar:Int = 0

  var secondVarIndice:Int = -1
  var secondVar:CBLSIntVar = null
  var oldValOfSecondVar:Int = 0

  var delta:Long = 0L
  var factor1:Long = 1L
  var factor2:Long = 1L

  override def exploreNeighborhood(initialObj: Long): Unit ={

    val firstIterationSchemeZone =
      if (searchZone1 == null) {
        if (hotRestart) {
          if (firstVarIndice >= vars.length) firstVarIndice = 0
          HotRestart(vars.indices, firstVarIndice)
        } else vars.indices
      } else if (hotRestart) HotRestart(searchZone1(), firstVarIndice) else searchZone1()

    val searchZone2ForThisSearch = if (searchZone2 == null) null else searchZone2()
    val searchZoneForDeltaL1 = searchZoneForDelta()

    val appliedFactorsForThisTime = appliedFactors()

    val (iIterator,notifyFound1) = selectFirstVariableBehavior.toIterator(firstIterationSchemeZone)
    while (iIterator.hasNext) {
      firstVarIndice = iIterator.next()
      firstVar = vars(firstVarIndice)
      oldValOfFirstVar = firstVar.newValueInt

      val secondIterationSchemeZone = if (searchZone2ForThisSearch == null) vars.indices else searchZone2ForThisSearch(firstVarIndice,oldValOfFirstVar)
      val searchZoneForDeltaL2 = searchZoneForDeltaL1(firstVarIndice,oldValOfFirstVar)

      val (jIterator,notifyFound2) = selectSecondVariableBehavior.toIterator(secondIterationSchemeZone)
      while (jIterator.hasNext) {
        secondVarIndice = jIterator.next()
        secondVar = vars(secondVarIndice)
        oldValOfSecondVar = secondVar.newValueInt

        if ((!symmetryCanBeBrokenOnIndices || firstVarIndice < secondVarIndice) //we break symmetry on variables
          && firstVarIndice != secondVarIndice) {

          val iterationOnDelta:LinearOptimizer = searchZoneForDeltaL2(secondVarIndice,oldValOfSecondVar)

          val (factor1i,factor2i) = appliedFactorsForThisTime(firstVarIndice,oldValOfFirstVar,secondVarIndice,oldValOfSecondVar)

          factor1 = factor1i
          factor2 = factor2i

          require(factor1 > 0)

          require(factor2 > 0)

          def evaluate(delta:Long): Long ={
            //println("Delta :" + delta)
            this.delta = delta
            firstVar := oldValOfFirstVar + delta
            secondVar := oldValOfSecondVar - ((delta * factor2) / factor1)
            val newObj = obj.value
            firstVar := oldValOfFirstVar
            secondVar := oldValOfSecondVar
            newObj
          }

          val minValueForDelta = (firstVar.min - oldValOfFirstVar) max (oldValOfSecondVar - secondVar.max) * factor1 /factor2
          val maxValueForDelta = (firstVar.max - oldValOfFirstVar) min (oldValOfSecondVar - secondVar.min) * factor1 /factor2

          val (bestDelta,objForDelta) = iterationOnDelta.search(0L, initialObj, minValueForDelta, maxValueForDelta, evaluate)

          this.delta = bestDelta

          if (bestDelta != 0 && evaluateCurrentMoveObjTrueIfSomethingFound(objForDelta)) {
            notifyFound1()
            notifyFound2()
          }
        }
      }
    }

    firstVarIndice = firstVarIndice + 1
    secondVarIndice = -1
    if (firstVar != null) {
      require(firstVar.newValue == oldValOfFirstVar)
    }
    if (secondVar != null) {
      require(secondVar.newValue == oldValOfSecondVar)
    }
    firstVar = null
    secondVar = null
  }

  override def instantiateCurrentMove(newObj: Long) =
    TransferMove(
      firstVar, oldValOfFirstVar, firstVarIndice,
      secondVar, oldValOfSecondVar, secondVarIndice,
      factor1, factor2, delta: Long,
      newObj, name)

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    firstVarIndice = 0
  }
}


/** standard move that swaps the value of two CBLSIntVar
  *
  * @param firstVar the variable
  * @param secondVar the other variable
  * @param objAfter the objective after this assignation will be performed
  * @param neighborhoodName a string describing the neighborhood hat found the move (for debug purposes)
  * @author renaud.delandtsheer@cetic.be
  */
case class TransferMove(firstVar:CBLSIntVar, oldValOfFirstVar:Long, firstVarIndice:Long,
                        secondVar:CBLSIntVar, oldValOfSecondVar: Long, secondVarIndice:Long,
                        factor1:Long,factor2:Long,delta:Long,
                        override val objAfter:Long, override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName){

  val newValOfFirstVar = oldValOfFirstVar + delta
  val newValOfSecondVar = oldValOfSecondVar - ((delta * factor2) / factor1)

  override def commit(): Unit = {
    firstVar := newValOfFirstVar
    secondVar := newValOfSecondVar
  }

  override def toString: String  = {
    s"${neighborhoodNameToString}Transfer($firstVar := $newValOfFirstVar $secondVar:=$newValOfSecondVar$objToString)"
  }

  override def touchedVariables: List[Variable] = List(firstVar,secondVar)
}
