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
package oscar.cp.core.variables

import oscar.algo.Inconsistency
import oscar.cp.constraints._
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint

/**
 * Build a graph CPVar
 * @author Andrew Lambert andrew.lambert@student.uclouvain.be
 * @param 	nNodes : number of nodes of the maximal graph, nodes are indexes as : [0,1,...,nNodes-1]
 * 			inputEdges : list of tuple/pair (source, destination) representing the edges of the maximal graph
 *               if an tuple has values out of range (e.g. node value bigger than nNodes-1), this edge is ignored
 */
class CPGraphVar(val s: CPStore, val nNodes: Int, inputEdges: List[(Int,Int)], val name: String = "") extends CPVar {
  
  def store = s

  
  // check if the edge is in range of nodes, otherwise ignore it
  private val r = 0 to nNodes
  private val correctInputEdges : List[(Int,Int)] = inputEdges.filter(x => (r.contains(x._1)) && (r.contains(x._2)))
  private val nEdges = correctInputEdges.length
  
  // N and E will hold current graph interval
  val N = new CPSetVar(store,0,nNodes-1)
  val E = new CPSetVar(store,0,nEdges-1)
  
  // define some useful inner class
  case class Edge(index : Int, src: Int, dest : Int)
  case class Node(index : Int, inEdges : List[Int], outEdges : List[Int])
  
  // the following structures (edges, nodes) will be used to access easily to the hidden graph
  // edges and nodes are IndexedSeq to be immutable
  // create edges directly
  private val edges : IndexedSeq[Edge] = Array.tabulate(nEdges)(i => new Edge(i,correctInputEdges(i)._1,correctInputEdges(i)._2 ) )
  // fill temporary arrays with inEdges and outEdges
  private var inEdgesNodesArray : Array[List[Int]] = Array.tabulate(nNodes)(i => List())
  private var outEdgesNodesArray : Array[List[Int]] = Array.tabulate(nNodes)(i => List())
  for(i <- 0 to (nEdges-1)){
    val e = edges(i)
    inEdgesNodesArray(e.dest) = e.index :: inEdgesNodesArray(e.dest)
    outEdgesNodesArray(e.src) = e.index :: outEdgesNodesArray(e.src)
  }
  // create nodes using temporary arrays
  private val nodes : IndexedSeq[Node] = Array.tabulate(nNodes)( i => new Node(i,inEdgesNodesArray(i),outEdgesNodesArray(i)) )
  
   /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever the domain of the variable changes
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callPropagateWhenDomainChanges(c: Constraint): Unit = {
    // call propagator of the two CPVarSet
    N.callPropagateWhenDomainChanges(c)
    E.callPropagateWhenDomainChanges(c)
  }
  
  /**
   * Level L1 registration called if an edge becomes required -> Call the CPVarSet method
   */
  def callValRequiredWhenRequiredValue(c: Constraint): Unit = {
    E.callValRequiredWhenRequiredValue(c)
  }

  
  /* ---------------------------------- 
   * The following methods modify the domain of graphs 
   *     and launch constraint's propagate() method after changing domain (if there is a constraint associated in the CSP)
   * They can be used in a CSP as : 
   *     add(g addNode n)
   * --------------------------------- */
  
  def isBound: Boolean = N.isBound && E.isBound
  
  
  /**
   * Add a node into required nodes from graph interval.
   * 	Launch propagation of associated constraint if there is such
   * @param node Id
   */
  def addNode(n: Int) = new RequiresNode(CPGraphVar.this, n)
  
  /**
   * Remove a node from possible nodes from graph interval and connected edges to this node.
   * 	Launch propagation of associated constraint if there is such
   * @param node Id
   */
  def removeNode(n: Int) = new ExcludesNode(CPGraphVar.this, n)
  
  /**
   * Add an edge into required edges from graph interval, also add in required nodes connected nodes to that edge
   * 	Launch propagation of associated constraint if there is such
   * @param source and destination of the edge
   */
  def addEdge(src: Int, dest: Int) = new RequiresEdge(CPGraphVar.this, src, dest)
  
    /**
   * Remove an edge from possible edges from graph interval
   * 	Launch propagation of associated constraint if there is such
   * @param source and destination of the edge
   */
  def removeEdge(src: Int, dest: Int) = new ExcludesEdge(CPGraphVar.this, src, dest)
  
  
  /* ---------------------------------- 
   * The following methods are simple getters of graphs representation
   * They don't modify the graph
   * --------------------------------- */	

   /**
   * @param node Id
   * @return Return a list with the index of all required outgoing edges from the node
   */
  def requiredOutEdges(nodeId: Int) : List[Int] = nodes(nodeId).outEdges.filter( E isRequired _ )
  
  /**
   * @param node Id
   * @return Return a list with the index of all required incoming edges from the node
   */
  def requiredInEdges(nodeId: Int) : List[Int] = nodes(nodeId).inEdges.filter( E isRequired _ )

  /**
   * @param node Id
   * @return Return a list with the index of all required edges from the node
   */
  def requiredEdges(nodeId: Int) : List[Int] = requiredInEdges(nodeId) ++ requiredOutEdges(nodeId)
  
  /**
   * @param node Id
   * @return Return a list with the index of all possible outgoing edges from the node
   */
  def possibleOutEdges(nodeId: Int) : List[Int] = nodes(nodeId).outEdges.filter( E isPossible _ )
  
  /**
   * @param node Id
   * @return Return a list with the index of all possible incoming edges from the node
   */
  def possibleInEdges(nodeId: Int) : List[Int] = nodes(nodeId).inEdges.filter( E isPossible _ )

  /**
   * @param node Id
   * @return Return a list with the index of all possible edges from the node
   */
  def possibleEdges(nodeId: Int) : List[Int] = possibleInEdges(nodeId) ++ possibleOutEdges(nodeId)
  
  /**
   * @return Return a list with the index of all required nodes
   */
  def requiredNodes() : List[Int] = N.requiredSet.toList
  
  /**
   * @return Return a list with the index of all possible nodes
   */
  def possibleNodes() : List[Int] = N.possibleSet.toList
  
  /**
   * @return the edge corresponding to index idx
   */
  def edge(idx : Int) : (Int,Int) = (edges(idx).src, edges(idx).dest)
  
  /**
   * @return the number of possible edges
   */
  def nbPossibleEdges() : Int = E.possibleSize.intValue()

  
  
  /* ---------------------------------- 
   * The following methods only modify the domain of graphs
   * They should not be called directly in a CSP
   *      because they don't launch constraint's propagate() method after changing domain
   * --------------------------------- */
  
  /**
   * Add a node into required nodes from graph interval
   * @param node Id
   */
  def addNodeToGraph(nodeId: Int) : Unit =  N.requires(nodeId)
  
  /**
   * Remove a node from possible nodes from graph interval and connected edges to this node
   * @param node Id
   */
  def removeNodeFromGraph(nodeId: Int) : Unit = {
    N.excludes(nodeId)

    for (e <- possibleEdges(nodeId))
      E.excludes(e)
  }
  
  /**
   * Add an edge into required edges from graph interval, also add in required nodes connected nodes to that edge
   * @param src and dest of the edge
   */
  def addEdgeToGraph(src: Int, dest: Int) : Unit = {
    val index = indexOfEdge(src, dest)
    addEdgeToGraph(index)
  }

  def addEdgeToGraph(edgeId : Int) : Unit = {
    if (! E.isPossible(edgeId))
      throw Inconsistency

    E.requires(edgeId)
    N.requires(edges(edgeId).src)
    N.requires(edges(edgeId).dest)
  }
  
  /**
   * Remove an edge from possible edges from graph interval
   * @param source and destination of the edge
   */
  def removeEdgeFromGraph(src: Int, dest: Int) : Unit = E.excludes(indexOfEdge(src, dest))
  def removeEdgeFromGraph(edgeId: Int) : Unit = E.excludes(edgeId)
  
  /**
   * @return the index of the edge (src,dest)
   * 			-1 if the edge is not in the graph interval
   */
  private def indexOfEdge(src: Int, dest: Int) : Int = {
    val edge = edges.filter(e => (e.src == src && e.dest == dest))
    if (edge.isEmpty) -1
    else edge(0).index
  } 
}

// Companion Object
object CPGraphVar {
  // Build CPVargraph giving number of nodes and list(edges) in parameter
  def apply(nNodes: Int, inputEdges: List[(Int,Int)])(implicit store: CPStore): CPGraphVar = {
    new CPGraphVar(store, nNodes, inputEdges)
  }
  
  @deprecated("use apply(nNodes: Int, inputEdges: List[(Int,Int)])(implicit store: CPStore) instead", "1.0")
  def apply(s: CPStore, nNodes: Int, inputEdges: List[(Int,Int)]): CPGraphVar = 
    new CPGraphVar(s, nNodes, inputEdges)
  
  // Build CPVargraph giving only number of nodes in parameter (edges will be complete graph)
  def apply(nNodes: Int)(implicit store: CPStore): CPGraphVar = {
    val len = nNodes -1
    var newlist = List((-1,-1))
    for (i <- 0 to len; j <- i+1 to len)
      newlist = (i,j) :: (j,i) :: newlist
    // inputEdges will be edges of the complete graph
    val inputEdges: List[(Int,Int)] = newlist.sorted.tail
    new CPGraphVar(store, nNodes, inputEdges)
  }
  
  @deprecated("use apply(nNodes: Int)(implicit store: CPStore) instead", "1.0")
  def apply(s: CPStore, nNodes: Int): CPGraphVar = {
    val len = nNodes -1
    var newlist = List((-1,-1))
    for (i <- 0 to len; j <- i+1 to len)
      newlist = (i,j) :: (j,i) :: newlist
    val inputEdges: List[(Int,Int)] = newlist.sorted.tail
    new CPGraphVar(s, nNodes, inputEdges)
  }
}
