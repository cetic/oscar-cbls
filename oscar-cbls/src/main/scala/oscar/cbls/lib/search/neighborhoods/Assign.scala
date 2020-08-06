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

import oscar.cbls.algo.search.{HotRestart, IdenticalAggregator}
import oscar.cbls.core.computation.{CBLSIntVar, Variable}
import oscar.cbls.core.search.{EasyNeighborhoodMultiLevel, First, LoopBehavior, Move}

/**
  * will find a variable in the array, and find a value from its range that improves the objective function
  *
  * @param vars an array of [[oscar.cbls.core.computation.CBLSIntVar]] defining the search space
  * @param name the name of the neighborhood
  * @param selectIndiceBehavior how should it iterate on the variables?
  * @param selectValueBehavior how should it iterate over the possible values to affect to the variable?
  * @param searchZone a subset of the indices of vars to consider.
  *                   If none is provided, all the array will be considered each time
  * @param symmetryClassOfVariables a function that input the ID of a variable and returns a symmetry class;
  *                      ony one of the variable in each class will be considered to make search faster
  *                      Int.MinValue is considered different to itself
  *                      if you set to None this will not be used at all
  *                      variables of the same class with different values will not be considered as symmetrical
  * @param symmetryClassOfValues a function that inputs the ID of a variable and a possible value for this variable,
  *                              and returns a symmetry class for this variable and value
  *                              only values belonging to different symmetry classes will be tested
  *                             Int.MinValue is considered different to itself
  *                             (this is only useful if your model is awfully expensive to evaluate)
  * @param domain a function that receives a variable and its Id in the vars array
  *               and returns the domain that is searched for the variable
  *               by default, the domain of the variable is explored
  * @param hotRestart  if true, the exploration order in case you ar not going for the best is a hotRestart
  *                    even if you specify a searchZone that is: the exploration starts again
  *                    at the position where it stopped, and consider the indices in increasing order
  *                    if false, consider the exploration range in natural order from the first position.
  */
case class AssignNeighborhood(vars:Array[CBLSIntVar],
                              name:String = "AssignNeighborhood",
                              selectIndiceBehavior:LoopBehavior = First(),
                              selectValueBehavior:LoopBehavior = First(),
                              searchZone:() => Iterable[Int] = null,
                              symmetryClassOfVariables:Option[Int => Int] = None,
                              symmetryClassOfValues:Option[Int => Int => Int] = None,
                              domain:(CBLSIntVar,Int) => Iterable[Int] = (v,_) => v.minInt to v.maxInt,
                              hotRestart:Boolean = true)
  extends EasyNeighborhoodMultiLevel[AssignMove](name){
  //the indice to start with for the exploration
  var startIndice:Int = 0

  var currentVar:CBLSIntVar = _ //null
  var currentIndice:Int = 0
  var newVal:Long = 0L

  override def exploreNeighborhood(initialObj: Long): Unit ={

    val iterationZone : Iterable[Int] =
      if (searchZone == null) vars.indices
      else searchZone()

    val iterationSchemeOnZone =
      if (hotRestart) HotRestart(iterationZone, startIndice)
      else iterationZone

    val iterationSchemeOnSymmetryFreeZone = symmetryClassOfVariables match {
      case None => iterationSchemeOnZone
      case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(iterationSchemeOnZone, (index :Int) => (s(index),vars(index).value))
    }

    //iterating over the variables to consider
    val (indicesIterator,notifyFound1) = selectIndiceBehavior.toIterator(iterationSchemeOnSymmetryFreeZone)
    while(indicesIterator.hasNext){
      currentIndice = indicesIterator.next()
      currentVar = vars(currentIndice)
      //now we have the current variable

      val oldVal = currentVar.value

      val domainIterationScheme = symmetryClassOfValues match {
        case None => domain(currentVar, currentIndice)
        case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(domain(currentVar, currentIndice), s(currentIndice))
      }

      //iterating over the values of the variable
      val (domainIterationSchemeIterator,notifyFound2) = selectValueBehavior.toIterator(domainIterationScheme)
      while(domainIterationSchemeIterator.hasNext){
        newVal = domainIterationSchemeIterator.next()
        if (newVal != oldVal){
          //testing newValue
          val newObj = obj.assignVal(currentVar,newVal)

          if (evaluateCurrentMoveObjTrueIfSomethingFound(newObj)) {
            notifyFound1()
            notifyFound2()
          }
        }
      }
    }

    startIndice = currentIndice + 1
  }

  override def instantiateCurrentMove(newObj:Long) =
    AssignMove(currentVar, newVal, currentIndice, newObj, name)

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}


/**
 * will find a variable in the array, and find a value from its range that improves the objective function
 *
 * @param vars an array of [[oscar.cbls.core.computation.CBLSIntVar]] defining the search space
 * @param name the name of the neighborhood
 * @param selectIndiceBehavior how should it iterate on the variables?
 * @param selectValueBehavior how should it iterate over the possible values to affect to the variable?
 * @param searchZone a subset of the indices of vars to consider.
 *                   If none is provided, all the array will be considered each time
 * @param symmetryClassOfVariables a function that input the ID of a variable and returns a symmetry class;
 *                      ony one of the variable in each class will be considered to make search faster
 *                      Int.MinValue is considered different to itself
 *                      if you set to None this will not be used at all
 *                      variables of the same class with different values will not be considered as symmetrical
 * @param symmetryClassOfValues a function that inputs the ID of a variable and a possible value for this variable,
 *                              and returns a symmetry class for this variable and value
 *                              only values belonging to different symmetry classes will be tested
 *                             Int.MinValue is considered different to itself
 *                             (this is only useful if your model is awfully expensive to evaluate)
 * @param domain a function that receives a variable and its Id in the vars array
 *               and returns the domain that is searched for the variable
 *               by default, the domain of the variable is explored
 * @param hotRestart  if true, the exploration order in case you ar not going for the best is a hotRestart
 *                    even if you specify a searchZone that is: the exploration starts again
 *                    at the position where it stopped, and consider the indices in increasing order
 *                    if false, consider the exploration range in natural order from the first position.
 */
case class NumericAssignNeighborhood(vars:Array[CBLSIntVar],
                                     name:String = "NumericAssignNeighborhood",
                                     selectIndiceBehavior:LoopBehavior = First(),
                                     selectValueBehavior:LoopBehavior = First(),
                                     searchZone:() => Iterable[Int] = null,
                                     symmetryClassOfVariables:Option[Int => Int] = None,
                                     symmetryClassOfValues:Option[Int => Int => Int] = None,
                                     domain:(CBLSIntVar,Int) => Iterable[Int] = (v,_) => v.minInt to v.maxInt,
                                     domainExplorer: () => (Int,Long) => LinearOptimizer,
                                     hotRestart:Boolean = true)
  extends EasyNeighborhoodMultiLevel[AssignMove](name){
  //the indice to start with for the exploration
  var startIndice:Int = 0

  var currentVar:CBLSIntVar = _ //null
  var currentIndice:Int = 0
  var newVal:Long = 0L

  override def exploreNeighborhood(initialObj: Long): Unit ={
    val iterationZone =
      if (searchZone == null) vars.indices
      else searchZone()

    val iterationSchemeOnZone =
      if (hotRestart) HotRestart(iterationZone, startIndice)
      else iterationZone

    val iterationSchemeOnSymmetryFreeZone = symmetryClassOfVariables match {
      case None => iterationSchemeOnZone
      case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(iterationSchemeOnZone, (index : Int) => (s(index),vars(index).value))
    }

    val searchZoneForVar = domainExplorer()

    //iterating over the variables to consider
    val (indicesIterator,notifyFound1) = selectIndiceBehavior.toIterator(iterationSchemeOnSymmetryFreeZone)

    while(indicesIterator.hasNext){
      currentIndice = indicesIterator.next()
      currentVar = vars(currentIndice)
      //now we have the current variable
//TODO: skip this var if domain is singleton
      if(currentVar.domain.size > 1) {
        val oldVal = currentVar.value

        val domainIterationScheme = symmetryClassOfValues match {
          case None => domain(currentVar, currentIndice)
          case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(domain(currentVar, currentIndice), s(currentIndice))
        }

        val searchZoneForThisVar = searchZoneForVar(currentIndice, oldVal)

        def eval(value: Long): Long = {
          this.newVal = value
          obj.assignVal(currentVar, value)
        }

        val (newBestVal, bestObj) = searchZoneForThisVar.search(oldVal, initialObj, domainIterationScheme.min, domainIterationScheme.max, eval)
        this.newVal = newBestVal
        if (newBestVal != oldVal && evaluateCurrentMoveObjTrueIfSomethingFound(bestObj)) {
          notifyFound1()
        }
      }
    }

    startIndice = currentIndice + 1
  }

  override def instantiateCurrentMove(newObj:Long) =
    AssignMove(currentVar, newVal, currentIndice, newObj, name)

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}

/** standard move that assigns an Long value to a CBLSIntVar
  *
  * @param i the variable
  * @param value the value to assign
  * @param id an ID that is used by the neighborhood to pass additional information
  * @param objAfter the objective after this assignation will be performed
  * @param neighborhoodName a string describing the neighborhood hat found the move (for debug purposes)
  * @author renaud.delandtsheer@cetic.be
  */
case class AssignMove(i:CBLSIntVar,value:Long, id:Int, override val objAfter:Long, override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName){

  override def commit(): Unit = {i := value}

  override def toString: String = {
    s"${neighborhoodNameToString}AssignMove($i set to $value$objToString)"
  }

  override def touchedVariables: List[Variable] = List(i)
}
