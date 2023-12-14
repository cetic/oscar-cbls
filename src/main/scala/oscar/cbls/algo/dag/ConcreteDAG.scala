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

/** A concrete DAGNode
  */
class ConcreteDAGNode(val concreteUniqueID: Int) extends DAGNode {

  setUniqueId(concreteUniqueID)

  private var predecessors: List[DAGNode] = List.empty
  private var successors: List[DAGNode]   = List.empty

  final def compare(that: DAGNode): Int = {
    assert(this.uniqueID != that.uniqueID || this == that)
    this.uniqueID - that.uniqueID
  }

  override def getDAGPredecessors: Iterable[DAGNode] = predecessors
  override def getDAGSuccessors: Iterable[DAGNode]   = successors

  /** Sets the current node as predecessor of the successor so that this -> successor
    * @param successor
    *   reference to the next node
    */
  def setAsANewPredecessorOf(successor: ConcreteDAGNode): Unit = {
    successors = successor :: successors
    successor.predecessors = this :: successor.predecessors
  }

  /** Sets the current node as successor of the predecessor so that predecessor -> this
    * @param predecessor
    *   reference to the predecessor node
    */

  def setAsANewSuccessorOf(predecessor: ConcreteDAGNode): Unit = {
    predecessor.setAsANewPredecessorOf(this)
  }
}

/** A concrete DAG
  *
  * @param Nodes
  *   the nodes of the DAG
  */
class ConcreteDAG(Nodes: Iterable[DAGNode]) extends DAG {
  def nodes: Iterable[DAGNode] = Nodes
}
