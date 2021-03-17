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

package oscar.cbls.algo.tarjan

import scala.collection.immutable.{SortedMap, SortedSet}

/** The Tarjan algorithm for detecting SCC (strongly connected components) in graphs
 * @author renaud.delandtsheer@cetic.be
 * @param A
 * @tparam T
 */
class Tarjan[T]( implicit A:Ordering[T]){ // <: Ordered[T]]{

  def getStronglyConnexComponents(Nodes:Iterable[T],
                                  GetSucceedingNodes:T => Iterable[T]):List[SortedSet[T]] = {
    var Index: SortedMap[T,Int] = SortedMap.empty
    var LowLink: SortedMap[T,Int] = SortedMap.empty
    var index:Int=0
    var Stack:List[T]=List.empty
    var StackSet:SortedSet[T] = SortedSet.empty
    var Components:List[SortedSet[T]]= List.empty
    var InOneComponent:SortedSet[T] = SortedSet.empty

    def visit(v:T): Unit = {
      Index += ((v,index))
      LowLink+=((v,index))
      index +=1
      Stack = v::Stack
      StackSet +=v
      // Consider successors of v
      for(w <- GetSucceedingNodes(v)){
        if(!Index.contains(w)){
          // Successor w has not yet been visited; recurse on it
          visit(w)
          LowLink+=(( v, LowLink(v).min(LowLink(w)) ))
        }else if(StackSet.contains(w)){
          // Successor w is in stack S and hence in the current SCC
          LowLink+=(( v,LowLink(v).min(Index(w)) ))
        }
      }

      // If v is a root node, pop the stack and generate an SCC
      if (LowLink(v) == Index(v)){
        //start a new strongly connected component
        var SCC:SortedSet[T] = SortedSet.empty[T]
        var finished:Boolean = false
        while(!finished){
          val node = Stack.head
          Stack = Stack.tail
          StackSet -= node
          SCC +=node
          InOneComponent += node
          finished = A.compare(node,v) == 0
        }
        Components = SCC :: Components
      }
    }

    for(n <- Nodes) {if(!Index.contains(n)) visit(n)}

//    for(n <- Nodes){if (!InOneComponent.contains(n)){
//      Components = SortedSet(n) :: Components
//    }}

    Components
  }
}
