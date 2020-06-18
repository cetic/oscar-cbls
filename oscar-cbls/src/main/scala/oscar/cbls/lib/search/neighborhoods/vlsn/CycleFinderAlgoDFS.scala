/**
  * *****************************************************************************
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
  * ****************************************************************************
  */
package oscar.cbls.lib.search.neighborhoods.vlsn

class CycleFinderAlgoDFS(graph:VLSNGraph,pruneOnReachability:Boolean) extends CycleFinderAlgo{
  private val nodes:Array[Node] = graph.nodes
  private val nbNodes = nodes.length
  private val nbLabels = graph.nbLabels

  override def findCycle(isLiveNode:Array[Boolean]):Option[List[Edge]] = {

    val reachabilityMatrix = if(pruneOnReachability){
      new ReacheabilityFloydWarshall(graph:VLSNGraph).buildRechabilityMatrix()
    }else null

    //MatrixTools.printBoolMatrix(reachabilityMatrix)

    val isNodeReached = Array.fill(nbNodes)(false)
    val isLabelReached = Array.fill(nbLabels)(false)
    val isNodeFullyExplored = Array.fill(nbNodes)(false)
    var rootNode:Node = null
    var rooNodeID:Int = -1

    def dfsExplore(node:Node,summedDelta:Long):Option[List[Edge]] ={
      var outgoingEdges = node.outgoing

      while(outgoingEdges != Nil){
        val currentEdge = outgoingEdges.head
        outgoingEdges = outgoingEdges.tail

        val currentEdgeDelta = currentEdge.deltaObj
        val newSummedDelta = currentEdgeDelta + summedDelta

        val targetNode = currentEdge.to
        val targetNodeID = targetNode.nodeID

        if(isLiveNode(targetNodeID) && !isNodeFullyExplored(targetNodeID)) {
          if (targetNode == rootNode) {
            if (newSummedDelta < 0L) {
              //we just found a negative cycle
              return Some(List(currentEdge))
            } //else e found a cycle, but it is not negative
          } else if (!pruneOnReachability || reachabilityMatrix(targetNodeID)(rooNodeID)) {

            val targetLabel = targetNode.label

            if (!isNodeReached(targetNodeID)
              && !isLabelReached(targetLabel)) {
              //this is a new node, it has a label that has not been reached yet
              //mark and recurse
              isLabelReached(targetLabel) = true
              isNodeReached(targetNodeID) = true
              dfsExplore(targetNode, newSummedDelta) match {
                case None => ;
                case Some(l) => return Some(currentEdge :: l)
              }
              isLabelReached(targetLabel) = false
              isNodeReached(targetNodeID) = false
            }
          }
        }
      }
      None
    }

    for(n <- nodes){
      rootNode = n
      rooNodeID = n.nodeID
      isLabelReached(rootNode.label) = true
      isNodeReached(rooNodeID) = true

      dfsExplore(rootNode,0L) match{
        case None => ;
        case a => return a
      }
      isLabelReached(rootNode.label) = false
      isNodeReached(rooNodeID) = false
      isNodeFullyExplored(rooNodeID) = true
    }
    None
  }
}

class ReacheabilityFloydWarshall(graph:VLSNGraph){
  private val nodes:Array[Node] = graph.nodes
  private val edges:Array[Edge] = graph.edges
  private val nbNodes = nodes.length
  private val nodeRange = 0 until nbNodes

  def buildRechabilityMatrix():Array[Array[Boolean]] = {
    val adjacencyMatrix = buildFloydMatrices()
    saturateAdjacencyMatrixToReacheabilityMatrix(adjacencyMatrix)
    adjacencyMatrix
  }

  private def buildFloydMatrices(): Array[Array[Boolean]] = {
    val adjacencyMatrix:Array[Array[Boolean]] = Array.tabulate(nbNodes)(_ => Array.fill(nbNodes)(false))

    for(node <- nodes.indices){
      adjacencyMatrix(node)(node) = true
    }

    for(edge <- edges){
      adjacencyMatrix(edge.from.nodeID)(edge.to.nodeID) = true
    }

    adjacencyMatrix
  }

  private def saturateAdjacencyMatrixToReacheabilityMatrix(adjacencyMatrix : Array[Array[Boolean]]):Unit = {
    for (k <- nodeRange) {
      for (i <- nodeRange) {
        for (j <- nodeRange) {
          adjacencyMatrix(i)(j) = adjacencyMatrix (i)(j) || (adjacencyMatrix (i)(k) &&  adjacencyMatrix(k)(j))
        }
      }
    }
  }
}
