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
/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

package oscar.cbls.algo.conflict

/** a class that proposes several conflict search algorithms.
  * given a set of items I, and a condition over subsets of I that is monotonic (forall X, Y subset of I, conflict(X) => conflict(X U Y)
  * a minimal conflict C is a subset of I such that conflict(C) and if any part of C is removed, C is not in conflict anymore.
  * @author renaud.delandtsheer@cetic.be
  * */
object ConflictSearch {

  /**Computes a minimal conflict over a list of thinks.
   *Implements the famous quickXplain algorithm in a generic way
   *[Ulrich Junker and F Valbonne, QuickXPlain: Conflict Detection for Arbitrary Constraint Propagation Algorithms, 2001L]
   * and proposes a faster implementation in case that additional operations can be performed on the state
   *
   * @param init the initial S, typically empty
   * @param toInject a list of C among which the minimal conflict must be searched
   * @param inject the procedure to inject a C into the S, going towards conflict
   * @param isConflict the procedure to check whether or not the S is in conflict
   * @return a minimal subset of toInject such that, when injected into init, they cause a conflict
   */
  def apply[S,C](init:S,
                 toInject:List[C],
                 inject:(S, C)=>S,
                 isConflict:S=>Boolean):List[C] = {
    search[S,C](init,  List.empty, toInject, inject, isConflict)
  }

  /**init includes items*/
  private def search[S,C](init:S,
                          items:List[C],
                          toInject:Iterable[C],
                          inject:(S, C)=>S,
                          isConflict:S=>Boolean):List[C] = {

    if (isConflict(init)) return items

    var accumulator = init
    var ListA:List[C] = List.empty
    var ListB:List[C] = List.empty
    var fillA = true
    var remaining = toInject

    while(true){
      if(remaining.isEmpty) throw new Exception("no conflict")
      val item = remaining.head
      remaining = remaining.tail
      accumulator = inject(accumulator,item)

      if (isConflict(accumulator)){
        val initwithitem = inject(init,item)
        var toreturn = item :: items
        
        if (ListB.nonEmpty){
          val initwithItemAndA:S = ListA.foldLeft(initwithitem)(inject)
          toreturn = search(initwithItemAndA, toreturn, ListB, inject, isConflict)
        }

        if (ListA.nonEmpty){
          val initwithItemAndB:S = ListB.foldLeft(initwithitem)(inject)
          toreturn = search(initwithItemAndB, toreturn, ListA, inject, isConflict)
        }
        return toreturn

      }else{
        if (fillA) ListA = item :: ListA
        else ListB = item :: ListB
        fillA = !fillA
      }
    }
    List.empty //never reached
  }

  /**Computes a minimal conflict over a list of thinks.
   * This version is faster : O(n) because it benefits from a remove operation
   * @param init the initial S, typically empty
   * @param toInject a list of C among which the minimal conflict must be searched
   * @param inject the procedure to inject a C into the S, going towards conflict
   * @param remove the procedure to remove a C from the S, possibly removing the conflict
   * @param isConflict the procedure to check whether or not the S is in conflict
   * @return a minimal subset of toInject such that, when injected into init, they cause a conflict
   */
  def apply[S,C](init:S,
                 toInject:List[C],
                 inject:(S,C)=>S,
                 remove:(S,C) => S,
                 isConflict:S=>Boolean):List[C] = {
    if (isConflict(init)) return List.empty

    var accumulatorList:List[C] = List.empty
    var accumulatorState = init
    var remaining = toInject
    while(!isConflict(accumulatorState)){
      if(remaining.isEmpty) throw new Exception("No conflict")
      val item = remaining.head
      accumulatorState = inject(accumulatorState,item)
      remaining = remaining.tail
      accumulatorList = item :: accumulatorList
    }

    var toreturn:List[C] = List.empty
    
    for (item <- accumulatorList){
      val testState = remove(accumulatorState,item)
      if (!isConflict(testState)){
        toreturn = item :: toreturn
      }else{
        accumulatorState = testState
      }
    }
    toreturn
  }
}
