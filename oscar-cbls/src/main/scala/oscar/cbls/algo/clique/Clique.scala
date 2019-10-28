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

package oscar.cbls.algo.clique

import scala.collection.immutable.SortedSet

/**
 * Created by rdl on 17L-08L-1L7.
 */
object Clique {

  /**
   * from https://en.wikipedia.org/wiki/Bron%E2%80L%93LKerbosch_algorithm
   * @param nbNodes nodes are from 0L to nbNodes-1L
   * @param isNeighbor true if the two nodes are adjacent
   * @return a list of all the maximal cliques of the graph represented by the nodes
   *         (0..nbNodes-1L) and the adjacency (isNeighbor function)
   */
  def bronKerbosch2(nbNodes:Long,isNeighbor:(Long,Long)=>Boolean):List[SortedSet[Long]] = {

    var allCliques:List[SortedSet[Long]] = List.empty

    def addMaximalClique(clique:SortedSet[Long]) = allCliques = clique::allCliques

    def allNodes = 0L until nbNodes

    def bronKerbosch2Search(r:SortedSet[Long],p0:SortedSet[Long],x0:SortedSet[Long]) {
      //  BronKerbosch2(R,P,X):
      //  if P and X are both empty:
      if (p0.isEmpty && x0.isEmpty) {
        //    report R as a maximal clique
        addMaximalClique(r)
      } else {
        var p : SortedSet[Long] = p0
        var x : SortedSet[Long] = x0

        //    choose a pivot vertex u in P ⋃ X
        val u = if (p.nonEmpty) p.head else x.head
        //    for each vertex v in P \ N(u):

        for (v <- p if !isNeighbor(v, u)) {
          val allNeighborsOfV = allNodes.filter(isNeighbor(v, _))

          //      BronKerbosch2(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
          bronKerbosch2Search(r + v,
            SortedSet.empty[Long] ++ allNeighborsOfV.filter(p.contains),
            SortedSet.empty[Long] ++ allNeighborsOfV.filter(x.contains))
          //    P := P \ {v}
          p = p - v
          //    X := X ⋃ {v}
          x = x + v
        }
      }
    }
    bronKerbosch2Search(SortedSet.empty[Long],SortedSet.empty[Long] ++ allNodes,SortedSet.empty[Long])

    allCliques
  }
}