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

package oscar.cbls.algo.dag

import oscar.cbls.algo.heap.BinaryHeap
import oscar.cbls.util.exceptions.DAGException

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

/** This trait describes the basic structure of a DAG node to support incremental topological
  * sorting.
  *
  * Each node is also expected to maintain sets of predecessors and successors to reflect the
  * structure of the graph.
  */
trait DAGNode extends Ordered[DAGNode] {

  /** The position of the DAGNode in the topological sort */
  var position: Int = 0

  /** Flag used by the algorithms to avoid visiting two times the same node. Supposed to be false
    * between each pass of the algorithm.
    */
  var visited: Boolean = false

  /** The unique ID of the DAGNode.
    *
    * The uniqueIDs are expected to start at 0L and increase continuously.
    *
    * There is ONE exception related to propagation: if one element is not mentioned in the
    * propagation structure its uniqueID is set to -1. For instance, constant values are not in the
    * propagation structure but they are mentioned in the dependencies of registered propagation
    * elements.
    */
  private var _uniqueID: Int = -1

  /** Set the unique id of the DAGNode
 *
    * @throws DAGException
    *   A unique ID has already been set
    */
  def setUniqueId(uniqueID: Int): Unit = {
    if (_uniqueID != -1)
      throw DAGException.uniqueIDAlreadySet(
        s"Trying to change the uniqueID from ${_uniqueID} to $uniqueID"
      )
    else _uniqueID = uniqueID
  }

  /** Returns the uniqueID of the DAGNode */
  def uniqueID: Int = _uniqueID

  /** Returns the predecessors of the DAGNode */
  protected[dag] def getDAGPredecessors: Iterable[DAGNode]

  /** Returns the successors of the DAGNode */
  protected[dag] def getDAGSuccessors: Iterable[DAGNode]
}

/** This trait represents a generic DAG that supports dynamic topological sort. This is performed by
  * altering the position of a [[DAGNode]], if necessary, so that the position value is increasing
  * with respect to the topological order.
  *
  * Sorting can either be done from scratch or maintained incrementally.
  *
  * Note: this trait does not directly express methods to alter the node set or the edge set.
  *
  * @author
  *   renaud.delandtsheer@cetic.be
  */
trait DAG {

  /** Incremental sort is on (true) or off (false) */
  private var _incrementalSort: Boolean = false

  /** Returns the nodes of the DAG */
  def nodes: Iterable[DAGNode]

  /** Performs a self-check on the ordering, used for testing */
  def checkSort(): Unit = {
    for (to <- nodes) {
      for (from <- to.getDAGPredecessors) {
        assert(from.position < to.position, "topological sort is wrong at " + from + "->" + to)
      }
    }
    for (from <- nodes) {
      for (to <- from.getDAGSuccessors) {
        assert(from.position < to.position, "topological sort is wrong at " + from + "->" + to)
      }
    }
  }

  /** Checks that the nodes are maintaining the correct references to each other. Nodes are expected
    * to know their successors and predecessors, and these sets should be consistent among all
    * nodes.
    *
    * @throws DAGException
   *   Some graph incoherence was detected
   * @throws NoSuchFieldError
    *   Some graph incoherence was detected
    */
  def checkGraph(): Unit = {
    nodes.foreach(n => {
      n.getDAGPredecessors.foreach(p => {
        if (!p.getDAGSuccessors.exists(s => s == n)) {
          throw DAGException.graphIncoherence("at nodes [" + p + "] -> [" + n + "]")
        }
      })
      n.getDAGSuccessors.foreach(s => {
        if (!s.getDAGPredecessors.exists(p => p == n)) {
          throw DAGException.graphIncoherence("at nodes [" + n + "] -> [" + s + "]")
        }
      })
    })
  }

  /** Turns the incremental sort on or off.
    *
    * If the incremental sort is activated:
    *   - Incremental sort is then applied at each notification of edge insertion.
    *   - If a cycle is detected, it does not switch on incremental-sort mode, but throws an
    *     exception.
    *
    * WARNING: addition or deletion of nodes when the incremental sort is active will likely cause
    * breakage.
    */
  def incrementalSort_=(mIncrementalSort: Boolean): Unit = {
    // Activating incremental sort
    if (mIncrementalSort && !_incrementalSort) {
      doDAGSort()
      // For testing purpose
      assert({ checkSort(); checkGraph(); true })
      _incrementalSort = true
    } else if (_incrementalSort && !mIncrementalSort) {
      // Deactivating incremental sort
      _incrementalSort = false
    }
  }

  /** @return the incremental-sort status */
  def incrementalSort: Boolean = _incrementalSort

  /** Notifies that an edge has been added between two nodes.
    *
    * This triggers a re-ordering of the nodes in the topological sort, if incremental sort is
    * active. The reordering might lead to an exception in case there is a cycle in the graph.
    * Notice that you do not need to notify edge deletion.
    *
    * WARNING: This trait doesn't support directly adding or removing an edge. The caller of this
    * function has to make sure that the corresponding edge has been actually added to the
    * associated predecessor and successor sets before calling the function.
    *
    * @param from
    *   The node starting the edge
    * @param to
    *   The node ending the edge
    */
  def notifyAddEdge(from: DAGNode, to: DAGNode): Unit = {

    if (_incrementalSort && (from.position > to.position)) {
      // Successors of to having a position greater than from
      val sortedForwardRegion = findSortedForwardRegion(to, from.position)
      // Predecessors of from having a position lesser than to
      val sortedBackwardsRegion = findSortedBackwardRegion(from, to.position)

      // Sorting positions incrementally
      val freePositionsToDistribute: List[Int] =
        extractSortedPositions(sortedForwardRegion, sortedBackwardsRegion)

      // Reallocation starting with backward region (which is followed by forward region)
      val freePositionsForForwardRegion =
        reallocatePositions(sortedBackwardsRegion, freePositionsToDistribute)

      reallocatePositions(sortedForwardRegion, freePositionsForForwardRegion)

      assert({ checkSort(); checkGraph(); true })
    }
  }

  /** Returns a cycle that is expected to be present in the DAG.
    *
    * It uses depth first search to explore the DAG from a starting point. Each node is inserted in
    * a list. If the node doesn't lead to a cycle, it's removed from the list and tagged as visited.
    * If we reach a node whose uniqueID is already in the list, we have found a cycle, which is made
    * up of the nodes of the list between the two elements with that uniqueID.
    * @param start
    *   If known, the starting node of the cycle otherwise None
    * @return
    *   The found cycle else an empty List
    */
  def getCycle(start: Option[DAGNode] = None): List[DAGNode] = {

    // Used to build the cycle. Once found it contains all its nodes
    var currentExploredNodes: List[DAGNode] = List.empty
    // Contains the uniqueID of all the nodes within currentExploredNodes
    var exploredUniqueID: SortedSet[Long] = SortedSet.empty

    def DFS(n: DAGNode): Boolean = {
      if (n.visited) false // Prevent exploration of already explored nodes
      else if (exploredUniqueID.contains(n.uniqueID)) {
        // Known uniqueID, cycle found
        currentExploredNodes = (n :: currentExploredNodes).reverse
        n.visited = true
        // Only n is tagged as visited in exploredUniqueID
        currentExploredNodes.dropWhile(x => !x.visited)
        nodes.foreach(p => p.visited = false)
        true
      } else {
        // Unknown uniqueID, keep looking
        exploredUniqueID += n.uniqueID
        currentExploredNodes = n :: currentExploredNodes
        n.getDAGSuccessors.foreach(p => if (DFS(p)) return true)
        n.visited = true
        exploredUniqueID -= n.uniqueID
        currentExploredNodes = currentExploredNodes.tail
        false
      }
    }

    start match {
      case Some(startingNode) =>
        // We know one of the node of the cycle
        if (DFS(startingNode)) currentExploredNodes else List.empty
      case None =>
        // We don't know any node of the cycle, need to test them all
        if (nodes.exists(n => !n.visited && DFS(n))) {
          currentExploredNodes
        } else {
          nodes.foreach(p => p.visited = false)
          List.empty
        }
    }
  }

  /** Sorts the DAG nodes according to dependencies.
    *
    * First position is set to zero.
    *
    * @throws DAGException
    *   A cycle has been detected
    */
  def doDAGSort(): Unit = {
    @tailrec
    def sortByPrecedingNodes(
      remainingNodes: List[DAGNode],
      frontNodes: List[DAGNode] = List.empty
    ): List[DAGNode] = {
      remainingNodes match {
        case Nil => frontNodes
        case head :: tail =>
          val nbPredecessors = head.getDAGPredecessors.size
          head.position = -nbPredecessors
          sortByPrecedingNodes(tail, if (nbPredecessors == 0) head :: frontNodes else frontNodes)
      }
    }

    @tailrec
    def loop(front: List[DAGNode], position: Int = 0): Int = {
      front match {
        case Nil => position
        case head :: tail =>
          head.position = position
          val successors = head.getDAGSuccessors.toList
          val addToFront = successors.filter(node => {
            node.position += 1
            node.position == 0
          })
          loop(addToFront ::: tail, position + 1)
      }
    }

    val startFront: List[DAGNode] = sortByPrecedingNodes(nodes.toList)
    if (loop(startFront) != nodes.size) {
      throw DAGException.cycle(
        "Cycle in topological sort: \n " + getCycle().mkString("\n ") + "\n"
      )
    }
  }

  /** Returns all the successors of startNode whose positions are lower than ceilPosition.
    *
    * @throws DAGException
    *   A cycle has been detected
    */
  @tailrec
  private def findSortedForwardRegion(
    startNode: DAGNode,
    ceilPosition: Long,
    heap: BinaryHeap[DAGNode] = new BinaryHeap[DAGNode]((n: DAGNode) => n.position, nodes.size),
    sortedRegion: List[DAGNode] = List.empty
  ): List[DAGNode] = {
    // First call
    if (sortedRegion.isEmpty) {
      heap.insert(startNode)
      startNode.visited = true
    }

    if (heap.isEmpty) {
      sortedRegion.reverse
    } else {
      val first = heap.popFirst().get
      first.getDAGSuccessors.foreach(s => {
        if (s.position == ceilPosition) {
          sortedRegion.foreach(_.visited = false)
          heap.foreach(_.visited = false)
          throw DAGException.cycle(
            "Cycle in topological sort: \n " + getCycle(Some(s)).mkString("\n ") + "\n"
          )
        } else if (!s.visited && s.position < ceilPosition) {
          heap.insert(s)
          s.visited = true
        }
      })
      findSortedForwardRegion(startNode, ceilPosition, heap, first :: sortedRegion)
    }
  }

  /** Returns all the predecessors of startNode whose positions are greater than floorPosition.
    */
  @tailrec
  private def findSortedBackwardRegion(
    startNode: DAGNode,
    floorPosition: Long,
    heap: BinaryHeap[DAGNode] = new BinaryHeap[DAGNode]((n: DAGNode) => -n.position, nodes.size),
    sortedRegion: List[DAGNode] = List.empty
  ): List[DAGNode] = {
    // First call
    if (sortedRegion.isEmpty) {
      heap.insert(startNode)
      startNode.visited = true
    }

    if (heap.isEmpty) {
      sortedRegion
    } else {
      val first = heap.popFirst().get
      first.getDAGPredecessors.foreach(p => {
        if (!p.visited && p.position > floorPosition) {
          heap.insert(p)
          p.visited = true
        }
      })
      findSortedBackwardRegion(startNode, floorPosition, heap, first :: sortedRegion)
    }
  }

  /** Extracts a list of sorted positions from two distinct lists of sorted DAGNodes. */
  @tailrec
  private def extractSortedPositions(
    firstList: List[DAGNode],
    secondList: List[DAGNode],
    sortedPositions: List[Int] = List.empty
  ): List[Int] = {
    (firstList, secondList) match {
      case (Nil, Nil) => sortedPositions.reverse
      case (Nil, head2 :: tail2) =>
        extractSortedPositions(firstList, tail2, head2.position :: sortedPositions)
      case (head1 :: tail1, Nil) =>
        extractSortedPositions(tail1, secondList, head1.position :: sortedPositions)
      case (head1 :: tail1, head2 :: tail2) =>
        if (head1.position < head2.position)
          extractSortedPositions(tail1, secondList, head1.position :: sortedPositions)
        else extractSortedPositions(firstList, tail2, head2.position :: sortedPositions)
    }
  }

  /** Changes the positions of the given graph nodes according to a sorted list of free positions.
    *
    * @param orderedNodesForReallocation
    *   The list of sorted DAGNodes
    * @param freePositionsToDistribute
    *   The list of free positions to distribute
    * @return
    *   The remaining positions to distribute
    */
  @tailrec
  private def reallocatePositions(
    orderedNodesForReallocation: List[DAGNode],
    freePositionsToDistribute: List[Int]
  ): List[Int] = {
    if (orderedNodesForReallocation.nonEmpty) {
      orderedNodesForReallocation.head.visited = false
      orderedNodesForReallocation.head.position = freePositionsToDistribute.head
      reallocatePositions(orderedNodesForReallocation.tail, freePositionsToDistribute.tail)
    } else {
      freePositionsToDistribute
    }
  }
}
