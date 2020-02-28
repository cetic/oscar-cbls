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

import oscar.cbls._
import oscar.cbls.algo.search.{IdenticalAggregator, HotRestart}
import oscar.cbls.core.computation.{Variable, CBLSIntVar}
import oscar.cbls.core.search.{Move, EasyNeighborhoodMultiLevel, First, LoopBehavior}


/**
 * will iteratively swap the value of two different variables in the array
 *
 * @param vars an array of [[oscar.cbls.core.computation.CBLSIntVar]] defining the search space
 * @param searchZone1 a subset of the indices of vars to consider for the first moved point
 *                   If none is provided, all the array will be considered each time
 * @param searchZone2 a subset of the indices of vars to consider for the second moved point
 *                   If none is provided, all the array will be considered each time
 *                   it receives the indice of the first var, and the old value of the first var
 * @param symmetryCanBeBrokenOnIndices if set to true, the neighborhood will break symmetries on indices of swapped vars
 *                            that is: the first variable will always have an indice strictly smaller than the second swapped variable
 *                            typically, you always want it except if you have specified one or two searchZones, and they are different
 * @param symmetryCanBeBrokenOnValue if set to true, the neighborhood will break symmetries on values of swapped vars
 *                            that is: thee first variable will always have a value strictly smaller than the value of second swapped variable
 *                            you do not want to have both symmetryCanBeBrokenOnIndices and symmetryCanBeBrokenOnValue
 * @param selectFirstVariableBehavior how should iterate over the first variable?
 * @param selectSecondVariableBehavior how should it iterate over the second variable?
 * @param name the name of the neighborhood
 * @param symmetryClassOfVariables1 a function that input the ID of a variable and returns a symmetry class;
 *                      for each role of the move, ony one of the variable in each class will be considered for the vars in searchZone1
 *                      this makes search faster
 *                      Long.MinValue is considered different to itself
 *                      if you set to None this will not be used at all
 * @param symmetryClassOfVariables2 a function that input the ID of a variable and returns a symmetry class;
 *                      for each role of the move, ony one of the variable in each class will be considered for the vars in searchZone2
 *                      this makes search faster
 *                      Long.MinValue is considered different to itself
 *                      if you set to None this will not be used at all
 * @param hotRestart  if true, the exploration order in case you ar not going for the best
 *                    is a hotRestart for the first swapped variable
 *                    even if you specify a searchZone that is: the exploration starts again
 *                    at the position where it stopped, and consider the indices in increasing order
 *                    if false, consider the exploration range in natural order from the first position.
 **/
case class SwapsNeighborhood(vars:Array[CBLSIntVar],
                             name:String = "SwapsNeighborhood",
                             searchZone1:()=>Iterable[Long] = null,
                             searchZone2:() => (Long,Long)=>Iterable[Long] = null,
                             symmetryCanBeBrokenOnIndices:Boolean = true,
                             symmetryCanBeBrokenOnValue:Boolean = false,
                             selectFirstVariableBehavior:LoopBehavior = First(),
                             selectSecondVariableBehavior:LoopBehavior = First(),
                             symmetryClassOfVariables1:Option[Long => Long] = None,
                             symmetryClassOfVariables2:Option[Long => Long] = None,
                             hotRestart:Boolean = true,
                             adjustIfNotInProperDomain:Boolean = false)
  extends EasyNeighborhoodMultiLevel[SwapMove](name){

  //also used as the indice to start with for the exploration
  var firstVarIndice:Long = 0L
  var firstVar:CBLSIntVar = null

  var secondVarIndice:Long = 0L
  var secondVar:CBLSIntVar = null

  override def exploreNeighborhood(initialObj: Long){

    val firstIterationSchemeZone : Iterable[Long] =
      if (searchZone1 == null) {
        if (hotRestart) {
          if (firstVarIndice >= vars.length) firstVarIndice = 0L
          0L until vars.length startBy firstVarIndice
        } else 0L until vars.length
      } else if (hotRestart) HotRestart(searchZone1(), firstVarIndice) else searchZone1()

    val firstIterationScheme = symmetryClassOfVariables1 match {
      case None => firstIterationSchemeZone
      case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(firstIterationSchemeZone, s)
    }

    val searchZone2ForThisSearch = if (searchZone2 == null) null else searchZone2()

    val (iIterator,notifyFound1) = selectFirstVariableBehavior.toIterator(firstIterationScheme)
    while (iIterator.hasNext) {
      firstVarIndice = iIterator.next()
      firstVar = vars(firstVarIndice)
      val oldValOfFirstVar = firstVar.newValue

      val secondIterationSchemeZone : Iterable[Long] = if (searchZone2ForThisSearch == null)0L until vars.length else searchZone2ForThisSearch(firstVarIndice,oldValOfFirstVar)

      val secondIterationScheme = symmetryClassOfVariables2 match {
        case None => secondIterationSchemeZone
        case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(secondIterationSchemeZone, s)
      }

      val (jIterator,notifyFound2) = selectSecondVariableBehavior.toIterator(secondIterationScheme)
      while (jIterator.hasNext) {

        secondVarIndice = jIterator.next()
        secondVar = vars(secondVarIndice)
        val oldValOfSecondVar = secondVar.newValue

        if ((!symmetryCanBeBrokenOnIndices || firstVarIndice < secondVarIndice) //we break symmetry on variables
          && firstVarIndice != secondVarIndice
          && (!symmetryCanBeBrokenOnValue || oldValOfFirstVar < oldValOfSecondVar) //we break symmetry on values
          && oldValOfFirstVar != oldValOfSecondVar){


          if (adjustIfNotInProperDomain) {

            val adjustedValueToSecondVar:Long = secondVar.domain.adjust(oldValOfFirstVar)
            val adjustedValueToFirstVar:Long = firstVar.domain.adjust(oldValOfSecondVar)

            if(adjustedValueToSecondVar != adjustedValueToFirstVar) {
              if (evaluateCurrentMoveObjTrueIfSomethingFound(obj.assignVal(
                List(
                  (firstVar, adjustedValueToFirstVar),
                  (secondVar, adjustedValueToSecondVar))))) {
                notifyFound1()
                notifyFound2()
              }
            }

          } else {
            if (secondVar.domain.contains(oldValOfFirstVar)
              && firstVar.domain.contains(oldValOfSecondVar)
              && oldValOfFirstVar != oldValOfSecondVar) {
              if (evaluateCurrentMoveObjTrueIfSomethingFound(obj.swapVal(firstVar, secondVar))) {
                notifyFound1()
                notifyFound2()
              }
            }
          }
        }
      }
    }
    firstVarIndice = firstVarIndice +1L
  }

  override def instantiateCurrentMove(newObj: Long) =
    SwapMove(firstVar, secondVar, firstVarIndice, secondVarIndice, adjustIfNotInProperDomain, newObj, name)

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    firstVarIndice = 0L
  }
}


/** standard move that swaps the value of two CBLSIntVar
  *
  * @param i the variable
  * @param j the other variable
  * @param objAfter the objective after this assignation will be performed
  * @param neighborhoodName a string describing the neighborhood hat found the move (for debug purposes)
  * @author renaud.delandtsheer@cetic.be
  */
case class SwapMove(i:CBLSIntVar,j:CBLSIntVar, idI:Int, idJ:Int, adjustIfNotInProperDomain:Boolean, override val objAfter:Long, override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName){

  override def commit() {
    if(adjustIfNotInProperDomain) {      val adjustedJValue:Long = i.domain.adjust(j.value)
      val adjustedIValue:Long = j.domain.adjust(i.value)
      i := adjustedJValue
      j := adjustedIValue

    }else{
      i :=: j
    }
  }

  override def toString: String  = {
    neighborhoodNameToString + "SwapMove(" + i + " swapped with " + j + ( if (adjustIfNotInProperDomain) " with adjust" else "") + objToString + ")"
  }

  override def touchedVariables: List[Variable] = List(i,j)
}
