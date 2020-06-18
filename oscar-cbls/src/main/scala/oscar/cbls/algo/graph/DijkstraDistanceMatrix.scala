package oscar.cbls.algo.graph

import scala.collection.parallel.immutable.ParVector

object DijkstraDistanceMatrix {

  def buildDistanceMatrix(g:ConditionalGraph,
                           isConditionalEdgeOpen:Int => Boolean):Array[Array[Long]] = {


    val distanceMatrix:Array[Array[Long]] =
      Array.fill(g.nbNodes)(null)

    val parNodes = ParVector.tabulate(g.nbNodes){x => x}
    for(fromNodeID <- parNodes) {
      distanceMatrix(fromNodeID) = computeAllDistancesFomNode(g.nodes(fromNodeID),
        g:ConditionalGraph,
        isConditionalEdgeOpen:Int => Boolean)
    }

    distanceMatrix
  }

  def computeAllDistancesFomNode(from:Node,
                                 g:ConditionalGraph,
                                 isConditionalEdgeOpen:Int => Boolean):Array[Long] = {

    def isEdgeOpen(edge: Edge): Boolean =
      edge.conditionID match {
        case None => true
        case Some(condition) => isConditionalEdgeOpen(condition)
      }

    val nodeToDistance = Array.fill(g.nbNodes)(Long.MaxValue)
    nodeToDistance(from.id) = 0

    //TODO: use a fibonacci heap here.
    val toDevelopHeap = new oscar.cbls.algo.heap.BinomialHeapWithMoveLong(
      nodeID => nodeToDistance(nodeID),
      g.nodes.length,
      g.nodes.length - 1)

    toDevelopHeap.insert(from.id)

    while (!toDevelopHeap.isEmpty) {
      val currentNodeId: Int = toDevelopHeap.removeFirst()
      val currentNode = g.nodes(currentNodeId)
      val currentNodeDistance = nodeToDistance(currentNode.id)

      for (outgoingEdge <- currentNode.incidentEdges) {
        if (isEdgeOpen(outgoingEdge)) {
          val otherNode:Node = outgoingEdge.otherNode(currentNode)
          val newDistance:Long = currentNodeDistance + outgoingEdge.length
          val otherNodeID:Int = otherNode.id

          val oldDistance = nodeToDistance(otherNodeID)
          if (newDistance < oldDistance) {
            nodeToDistance(otherNodeID) = newDistance

            if(otherNode.transitAllowed) {
              if (toDevelopHeap.contains(otherNodeID)) {
                toDevelopHeap.notifyChange(otherNodeID)
              } else {
                toDevelopHeap.insert(otherNodeID)
              }
            }
          }
        }
      }
    }
    nodeToDistance
  }
}
