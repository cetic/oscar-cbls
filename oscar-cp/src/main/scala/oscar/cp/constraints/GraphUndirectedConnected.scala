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


package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core._
import oscar.cp.core.variables.{CPGraphVar, CPVar}

/**
 * @author Andrew Lambert andrew.lambert@student.uclouvain.be
 * Defines the constraint Connected(G) for an undirected graph
 */

class GraphUndirectedConnected(val g : CPGraphVar) extends Constraint(g.s, "Undirected Connected") {

  val cc = new oscar.algo.DisjointSets[Int](0,g.nNodes)

  override def associatedVars(): Iterable[CPVar] = Array(g)

	override def setup(l: CPPropagStrength): Unit = {
	  // add filter when domain changes
	  g.callPropagateWhenDomainChanges(this)
	  propagate()
	}
	
	override def propagate(): Unit = {
	  // #1 If required D(G) != empty and possible D(G) contains more than one connected component, 
	  //	   remove all but the connected component containing required D(G). 
	  // #2 Then all cutnodes and bridges on a path between two nodes of required D(G) are included in required D(G)

	  val reqNodes : List[Int] = g.requiredNodes
	  val possNodes : List[Int] = g.possibleNodes
	  if (!reqNodes.isEmpty){
	    // we have to compute connected components

      cc.reset()
      for (e <- g.E.possibleSet()) {
        val (a,b) = g.edge(e)
        cc.union(a,b)
      }


	    val connectedComponents : Array[List[Int]] = weaklyConnectedComponents(possNodes).toArray


	    
	    // #1
	    if (connectedComponents.size > 1) {
	      // check whether if two required nodes belongs to two different components -> Failure



        val componentNumber : Int = connectedComponents.indexWhere(l => l.contains(reqNodes.head))
	      for ( n <- reqNodes.tail)
	        if (! connectedComponents(componentNumber).contains(n)) throw Inconsistency
	      // remove all but the connected component containing required D(G)
	      for (n <- possNodes)
	        if (! connectedComponents(componentNumber).contains(n))
	          g.removeNodeFromGraph(n)
	    }
	    
	    // #2
	  	// add in required D(G) all cutnodes and bridges on a path between two nodes of required D(G)
	    val newPossNodes = g.possibleNodes
	    val possNotReqNodes : List[Int] = newPossNodes.filter(!reqNodes.contains(_))
	    var cutnodes : List[Int] = List()
	  	for (n1 <- reqNodes; n2 <- reqNodes; if n1!=n2) {
	  	  // check for all possible nodes not in reqNodes nor already cutnots that its removal would disconnect n1 from n2
	  	  val nodesToCheck = newPossNodes.filter(!cutnodes.contains(_))
	  	  for (n <- nodesToCheck; if ! existPath(n1, n2, n)){
	  	    // n is a cutnode
	  	    //cutnodes = n :: cutnodes
	  	    g.addNodeToGraph(n)
	  	  }
	  	}
	    // check for bridge : if an edge removal would disconnect two required nodes, its a bridge
	    val possEdges = newPossNodes.flatMap(g.possibleOutEdges(_)).map(g.edge(_))
	    val newReqNodes : List[Int] = g.requiredNodes
	    for (n1 <- newReqNodes; n2 <- newReqNodes; if n1!=n2){
	      for (e <- possEdges; if !existPath(n1, n2, e)){
	        g.addEdgeToGraph(e._1,e._2)
	        g.addEdgeToGraph(e._2,e._1)
	      }      
	    }
	  }
	}
	
	/* ---------------------------------- 
     * Useful functions to compute the constraint
     * --------------------------------- */
	/**
     * @return nodes neighbors to the node with index nodeId
     *   one of the two edges between the two nodes (in both direction) should be available
     */
    private def weaklyConnectedNeighbors(nodeId : Int) : List[Int] = 
      g.possibleNodes.filter(n => (nodeId != n) && 
        (!g.possibleEdges(nodeId).map(idx => g.edge(idx)).filter(e => e._2 == n).isEmpty || 
         !g.possibleEdges(n).map(idx => g.edge(idx)).filter(e => e._2 == nodeId).isEmpty) )
     
    /**
     * @param : Nodes to check
     * @return a list of list of integer, each inner list contains one connected component
     */
    private def weaklyConnectedComponents(possNodes : List[Int]) : List[List[Int]] = {
      def connectedNodesList(neighborsNotVisited : List[Int], connectedNodesAcc : List[Int]) : List[Int] = {
        neighborsNotVisited match {
          case Nil => connectedNodesAcc.sorted
          case node :: t => {
            val neighbors : List[Int] = weaklyConnectedNeighbors(node) 
            val newNeighbors : List[Int] = neighbors.filter(elem => !(node :: connectedNodesAcc).contains(elem) )
            connectedNodesList((t ++ newNeighbors).distinct, node :: connectedNodesAcc)
          }
        }
      }
      def possibleConnectedComponents(possNodes : List[Int]) : List[List[Int]] = {
        var visitedNodes : List[Int] = List()
        var componentsList : List[List[Int]] = List()
        for (n <- possNodes) {
          if (!visitedNodes.contains(n)) {
            val connectedNodes : List[Int] = connectedNodesList(List(n),List())
            visitedNodes = visitedNodes ++ connectedNodes
            componentsList = connectedNodes :: componentsList
          }
        }
        componentsList
      }
      possibleConnectedComponents(possNodes)
    }

  // return true if there exists at least one path from s to d without going through n
  private def existPath(s: Int, d: Int, n: Int): Boolean = {
    def possibleNeighborsList(nodeId: Int): List[Int] = g.possibleOutEdges(nodeId).map(g.edge(_)._2) ++ g.possibleInEdges(nodeId).map(g.edge(_)._1)
    // Search to find a path from s to d
    if (s == d) return true
    var toVisit: List[Int] = List(s)
    var visited: List[Int] = List(n) // we will not visit n anymore
    while (!toVisit.isEmpty) {
      val node: Int = toVisit.head
      val possNList = possibleNeighborsList(node)
      if (possNList.contains(d)) return true
      visited = node :: visited
      val neighb = possNList.filter(!visited.contains(_))
      toVisit = toVisit.tail ++ neighb
    }
    return false
  }

  // return true if there exists at least one path from s to d without going through edge e
  private def existPath(s: Int, d: Int, e: (Int, Int)): Boolean = {
    val (e1, e2) = e

    def possibleNeighborsWithoutE(nodeId: Int): List[Int] = {
      if (nodeId == e1) (g.possibleOutEdges(nodeId).map(g.edge(_)._2) ++ g.possibleInEdges(nodeId).map(g.edge(_)._1)).filter(_ != e2)
      else if (nodeId == e2) (g.possibleOutEdges(nodeId).map(g.edge(_)._2) ++ g.possibleInEdges(nodeId).map(g.edge(_)._1)).filter(_ != e1)
      else g.possibleOutEdges(nodeId).map(g.edge(_)._2) ++ g.possibleInEdges(nodeId).map(g.edge(_)._1)
    }
    // Search to find a path from s to d
    if (s == d) return true
    var toVisit: List[Int] = List(s)
    var visited: List[Int] = List()
    while (!toVisit.isEmpty) {
      val node: Int = toVisit.head
      val possNList = possibleNeighborsWithoutE(node)
      if (possNList.contains(d)) return true
      visited = node :: visited
      val neighb = possNList.filter(!visited.contains(_))
      toVisit = toVisit.tail ++ neighb
    }
    return false
  }
}