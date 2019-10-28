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

package oscar.cbls.algo.dag


/** a concrete DAG  implementing all the abstract methods of DAG
  * @author renaud.delandtsheer@cetic.be
  * @param _UniqueID: an ID to be used as comparison for storage in sorted data structures
  */
class ConcreteDAGNode(val _UniqueID:Int) extends DAGNode{

  uniqueID = _UniqueID

  var PrecedingNodes: List[DAGNode] = List.empty
  var SucceedingNodes:List[DAGNode] = List.empty

  final def compare(that: DAGNode):Int = {
    assert(this.uniqueID != that.uniqueID || this == that)
    this.uniqueID - that.uniqueID
  }

  override def getDAGPrecedingNodes: Iterable[DAGNode] = PrecedingNodes
  override def getDAGSucceedingNodes: Iterable[DAGNode] = SucceedingNodes


  /**
    * Sets the current node as predecessor of the parameter 'successor'
    * so that this -> successor
    * @param successor
    */
  def setAsPrecedingNodeKnownNotYetPreceding(successor:ConcreteDAGNode){
    SucceedingNodes = successor :: SucceedingNodes
    successor.PrecedingNodes = this :: successor.PrecedingNodes
  }

  /**
    * Sets the current node as successor of the parameter 'predecessor'
    * so that predecessor -> this
    * @param predecessor
    */

  def setAsSucceedingNodeKnownNotYetSucceeding(predecessor:ConcreteDAGNode){
    predecessor.setAsPrecedingNodeKnownNotYetPreceding(this)
  }
}

/** a concrete DAG  implementing all the abstract methods of DAG
  * @author renaud.delandtsheer@cetic.be
  * @param Nodes the nodes of the DAG
  */
class ConcreteDAG(Nodes:Iterable[DAGNode]) extends DAG{
  //  var Nodes:List[DAGNode] = List.empty
  def nodes:Iterable[DAGNode] = Nodes
}

