// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.algo.tarjan

import scala.annotation.tailrec

/** This class allows the computation of the strongly connected components for any graph where the
  * nodes are of type T, using the Tarjan Algorithm
  * @tparam T
  *   the type of values associated to the nodes in the graph.
  */
class Tarjan[T] {

  /** This class encapsulates the state of the Tarjan algorithm for one node in the graph
    */
  private class TarjanNodeState {
    // Index for this node in the DFS (Depth-First Search)
    var index: Int = -1
    // Smallest index of any node on the stack known to be reachable from v through v's DFS subtree
    var lowLink: Int = -1
    // Indicator whether this node is on the current stack
    var isOnStack: Boolean = false
  }

  // The following variables represent the state of the Tarjan algorithm:
  // Number of treated nodes
  private var index: Int = _
  // Stack of nodes representing the current exploring branch of the DFS
  private var stack: List[T] = _
  // List of currently computed Strongly Connected Components
  private var scComponents: List[List[T]] = _
  // Mapping from nodes to their internal state
  private var internalStorage: Map[T, TarjanNodeState] = _

  /** Initializes the state variables
    * @param nodes
    *   the nodes of the graph, as an [[Iterable]] collection
    */
  private def initState(nodes: Iterable[T]): Unit = {
    @tailrec
    def initMap(ns: Iterable[T]): Unit = {
      if (ns.nonEmpty) {
        val n = ns.head
        internalStorage += (n -> new TarjanNodeState)
        initMap(ns.tail)
      }
    }
    /////////////////////////
    index = 0
    stack = Nil
    scComponents = Nil
    internalStorage = Map()
    initMap(nodes)
  }

  /** Pops the stack until the node v is found. The node v should be in the stack
    * @param v
    *   the node to find in the stack
    * @param sccAcc
    *   a list of cumulated elements already popped from the stack
    * @return
    *   the list of elements popped from the stack concatenated with sccAcc, including v
    */
  @tailrec
  private def unstackUntil(v: T, sccAcc: List[T]): List[T] = {
    stack match {
      case Nil => sccAcc
      case n :: _ =>
        val storageForNode = internalStorage(n)
        stack = stack.tail
        storageForNode.isOnStack = false
        if (n == v) {
          n :: sccAcc
        } else {
          unstackUntil(v, n :: sccAcc)
        }
    }
  }

  /** Computes the Strongly Connected Components of a graph using the Tarjan Algorithm
    * @param nodes
    *   the collection of all the nodes in the graph
    * @param adjacencies
    *   the adjacency list of the graph, represented as a function from a node to a collection of nodes
    * @return
    *   the list of Strongly Connected Components of the graph represented by nodes and adjacencies
    */
  def computeSCC(nodes: Iterable[T], adjacencies: T => Iterable[T]): List[List[T]] = {
    def visit(v: T): Unit = {
      val storageForV = internalStorage(v)
      storageForV.index = index
      storageForV.lowLink = index
      storageForV.isOnStack = true
      index += 1
      stack ::= v
      // Consider successors of v
      adjacencies(v).foreach { w =>
        val storageForW = internalStorage(w)
        if (storageForW.index == -1) {
          // Successor w has not yet been visited; recurse on it
          visit(w)
          storageForV.lowLink = storageForV.lowLink min storageForW.lowLink
        } else if (storageForW.isOnStack) {
          // Successor w is in stack S and hence in the current SCC
          storageForV.lowLink = storageForV.lowLink min storageForW.index
        }
      }
      // If v is a root node, pop the stack to generate a SCC
      if (storageForV.lowLink == storageForV.index) {
        val scc = unstackUntil(v, Nil)
        scComponents = scc :: scComponents
      }
    }
    /////////////////////////
    initState(nodes)
    nodes.foreach { n =>
      if (internalStorage(n).index == -1) visit(n)
    }
    scComponents
  }
}
