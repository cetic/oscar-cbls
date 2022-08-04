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

import oscar.cbls.core.computation.CBLSIntVar
import oscar.cbls.core.search.{CompositeMove, Move, Neighborhood, NoMoveFound, Profiler, SearchResult}
import oscar.cbls.core.objective.Objective
import oscar.cbls.lib.search.LinearSelectors

import scala.collection.immutable.SortedSet


/**
 * Will randomize the array, typically to get out of a local minimal
 * This will not consider the objective function, even if it includes some strong constraints
 *
 * @param vars an array of CBLSIntVar defining the search space
 * @param degree the number of variables to change randomly
 * @param searchZone a subset of the indices of vars to consider.
 *                   If none is provided, all the array will be considered each time
 * @param valuesToConsider: the set of values to consider for the given variable
 * @param name the name of the neighborhood
 */
case class RandomizeNeighborhood(vars:Array[CBLSIntVar],
                                 degree:() => Int = ()=>1,
                                 name:String = "RandomizeNeighborhood",
                                 searchZone:() => SortedSet[Int] = null,
                                 valuesToConsider:(CBLSIntVar,Long) => Iterable[Long] = (variable,_) => variable.domain.values,
                                 acceptanceChecking:Option[Int] = None)
  extends Neighborhood(name) with LinearSelectors{

  override val profiler: Profiler = new Profiler(name)

  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean = null): SearchResult = {
    if(printExploredNeighborhoods) println("applying " + name)

    val degreeNow = degree()

    var (nbAttempts:Int,checkAcceptation:Boolean) = acceptanceChecking match{
      case None => (-1,false)
      case Some(a) => (a,true)
    }

    //TODO: we should check acceptation for all parts of the move. That could be implemented using Atomic(swap(behavior:Random))
    while(nbAttempts != 0) {
      nbAttempts -= 1
      var toReturn: List[AssignMove] = List.empty

      if (searchZone != null && searchZone().size <= degreeNow) {
        //We move everything
        for (i <- searchZone()) {
          toReturn = AssignMove(vars(i), selectFrom(vars(i).domain.values), i, Long.MaxValue) :: toReturn
        }
      } else {
        var touchedVars: Set[Int] = SortedSet.empty
        val searchZoneNow = if(searchZone==null) null else  searchZone()
        for (r <- 1 to degreeNow) {
          val i = selectFrom(vars.indices, (j: Int) => (searchZone == null || searchZoneNow.contains(j)) && !touchedVars.contains(j))
          touchedVars = touchedVars + i
          val oldVal = vars(i).value
          val valuesToConsiderForThisVar = valuesToConsider(vars(i), i)
          val selectedValue = selectFrom(valuesToConsiderForThisVar , (_: Long) != oldVal)
          toReturn = AssignMove(vars(i), selectedValue, i, Long.MaxValue) :: toReturn
        }
      }

      if (!checkAcceptation
        || acceptanceCriteria(initialObj, obj.assignVal(toReturn.map(x => (x.i, x.value))))) {
        if (printExploredNeighborhoods) println(name + ": move found")
        return CompositeMove(toReturn, Long.MaxValue, name)
      }
      if(printExploredNeighbors) println(name + ": move explored and not accepted")
    }
    NoMoveFound
  }
}

/**git s
 * will randomize the array, by performing swaps only.
 * This will not consider the objective function, even if it includes some strong constraints
 *
 * @param vars an array of [[oscar.cbls.core.computation.CBLSIntVar]] defining the search space
 * @param degree the number of variables to change randomly
 * @param searchZone a subset of the indices of vars to consider.
 *                   If none is provided, all the array will be considered each time
 * @param name the name of the neighborhood
 */
case class RandomSwapNeighborhood(vars:Array[CBLSIntVar],
                                  degree:() => Int = () => 1,
                                  name:String = "RandomSwapNeighborhood",
                                  searchZone:() => SortedSet[Int] = null)  //TODO: search zone does not work!
  extends Neighborhood(name) with LinearSelectors{

  override val profiler: Profiler = new Profiler(name)

  override def getMove(obj: Objective,
                       initialObj:Long,
                       acceptanceCriteria: (Long, Long) => Boolean = null): SearchResult = {
    if(printExploredNeighborhoods) println("applying " + name)

    var toReturn:List[Move] = List.empty
    val degreeNow:Int = degree()
    var touchedVars:Set[Int] = SortedSet.empty
    val varsToMove = if (searchZone == null) vars.length else searchZone().size
    for(r <- 1 to degreeNow if varsToMove - touchedVars.size >= 2L){
      val i = selectFrom(vars.indices,(i:Int) => (searchZone == null || searchZone().contains(i)) && !touchedVars.contains(i))
      touchedVars = touchedVars + i
      val j = selectFrom(vars.indices,(j:Int) => (searchZone == null || searchZone().contains(j)) && !touchedVars.contains(j))
      touchedVars = touchedVars + j
      toReturn = SwapMove(vars(i), vars(j), i,j,false, Long.MaxValue) :: toReturn
    }

    if(printExploredNeighborhoods) println(name + ": move found")
    CompositeMove(toReturn, Long.MaxValue, name)
  }
}
