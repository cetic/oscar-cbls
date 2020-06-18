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
 * Created by rdl on 17-08-17.
 */
object Clique {

  /**
   * from https://en.wikipedia.org/wiki/Bron%E2%80L%93LKerbosch_algorithm
   * @param nbNodes nodes are from 0 to nbNodes-1L
   * @param isNeighbor true if the two nodes are adjacent
   * @return a list of all the maximal cliques of the graph represented by the nodes
   *         (0..nbNodes-1L) and the adjacency (isNeighbor function)
   */
  def bronKerbosch2(nbNodes:Int,
                    isNeighbor:(Int,Int)=>Boolean): List[SortedSet[Int]] = {

    var allCliques:List[SortedSet[Int]] = List.empty

    def addMaximalClique(clique:SortedSet[Int]): Unit = allCliques = clique::allCliques

    def allNodes: Range = 0 until nbNodes

    def bronKerbosch2Search(r:SortedSet[Int],
                            p0:SortedSet[Int],
                            x0:SortedSet[Int]): Unit = {
      //  BronKerbosch2(R,P,X):
      //  if P and X are both empty:
      if (p0.isEmpty && x0.isEmpty) {
        //    report R as a maximal clique
        addMaximalClique(r)
      } else {
        var p : SortedSet[Int] = p0
        var x : SortedSet[Int] = x0

        //    choose a pivot vertex u in P ⋃ X
        val u = if (p.nonEmpty) p.head else x.head
        //    for each vertex v in P \ N(u):

        for (v <- p if !isNeighbor(v, u)) {
          val allNeighborsOfV = allNodes.filter(isNeighbor(v, _))

          //      BronKerbosch2(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
          bronKerbosch2Search(r + v,
            SortedSet.empty[Int] ++ allNeighborsOfV.filter(p.contains),
            SortedSet.empty[Int] ++ allNeighborsOfV.filter(x.contains))
          //    P := P \ {v}
          p = p - v
          //    X := X ⋃ {v}
          x = x + v
        }
      }
    }
    bronKerbosch2Search(SortedSet.empty[Int],SortedSet.empty[Int] ++ allNodes,SortedSet.empty[Int])

    allCliques
  }
}
