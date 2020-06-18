package oscar.cbls.lib.search.neighborhoods

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

import oscar.cbls.core.computation.{CBLSIntVar, InvariantHelper}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search._
import oscar.cbls.lib.search.LinearSelectors

import scala.util.Random


/**
 * will randomize the array, by performing shuffle on a subset of the variables
 * This will not consider the objective function, even if it includes some strong constraints
 *
 * @param vars an array of [[oscar.cbls.core.computation.CBLSIntVar]] defining the search space
 * @param indicesToConsider the positions to consider in the shuffle, all positions if not specified
 * @param numberOfShuffledPositions the number of positions to shuffle, taken in indicesToConsider.
 * @param name the name of the neighborhood
 * @param checkNoMoveFound checks that the variables to shuffle have different values, return NoMoveFound if this is not the case
 */
case class ShuffleNeighborhood(vars:Array[CBLSIntVar],
                               indicesToConsider:()=>Iterable[Int] = null,
                               numberOfShuffledPositions:() => Int = () => Int.MaxValue,
                               name:String = "ShuffleNeighborhood",
                               checkNoMoveFound:Boolean = true)
  extends Neighborhood(name) with LinearSelectors{

  override def getMove(obj: Objective,
                       initialObj:Long,
                       acceptanceCriteria: (Long, Long) => Boolean = null): SearchResult = {
    if(printExploredNeighborhoods) println("applying " + name)

    val (realIndicesToConsider:List[Int],numberOfIndicesToConsider:Int) =
      if(indicesToConsider == null) (vars.indices.toList,vars.length)
      else { val tmp = indicesToConsider(); (tmp.toList,tmp.size) }

    if(checkNoMoveFound) {
      val (minValue, maxValue) = InvariantHelper.getMinMaxBoundsInt(realIndicesToConsider.map(vars(_).value))
      if (minValue == maxValue) return NoMoveFound
    }

    val numberOfShuffledPositionsThisTime:Int = numberOfShuffledPositions()
    val subsetOfIndicesToConsider:List[Int] = if(numberOfShuffledPositionsThisTime >= numberOfIndicesToConsider){
      realIndicesToConsider
    }else{
      //shuffle only a subset; select it randomly
      Random.shuffle(realIndicesToConsider).takeRight(numberOfShuffledPositionsThisTime)
    }

    //shuffle everything
    val values = subsetOfIndicesToConsider.map(vars(_).value)
    val newValues = Random.shuffle(values)

    val moves:List[AssignMove] = subsetOfIndicesToConsider.zip(newValues).
      map({case (indice,newValue) => AssignMove(vars(indice),newValue,indice,Int.MaxValue)})

    if(printExploredNeighborhoods) println(name + ": move found")
    CompositeMove(moves, Long.MaxValue, name)
  }
}
