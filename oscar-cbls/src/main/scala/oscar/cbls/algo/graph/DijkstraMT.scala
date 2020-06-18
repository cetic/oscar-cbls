package oscar.cbls.algo.graph

class DijkstraMT(g:ConditionalGraph){

  def search(from:Node,
             targets:Iterable[Node],
             isConditionalEdgeOpen:Int => Boolean
            ):ClosestCentroidLabeling = {

    def isEdgeOpen(edge: Edge): Boolean =
      edge.conditionID match {
        case None => true
        case Some(condition) => isConditionalEdgeOpen(condition)
      }

    val nodeToDistance = Array.fill[Long](g.nodes.length)(Long.MaxValue)

    val isTarget:Array[Boolean] = Array.fill[Boolean](g.nodes.length)(false)
    for(target <- targets){
      isTarget(target.id) = true
    }

    if(isTarget(from.id)){
      return VoronoiZone(from, 0, null)
    }
    //we can only put node with an existing under-approximated distance to the target, this only needs to be checked on the source node, actually
    val toDevelopHeap = new oscar.cbls.algo.heap.BinomialHeapWithMoveLong(
      nodeID => nodeToDistance(nodeID),
      g.nodes.length,
      g.nodes.length - 1)

    nodeToDistance(from.id) = 0
    toDevelopHeap.insert(from.id)

    var toReturn:ClosestCentroidLabeling = Unreachable
    var maxDistance:Long = Long.MaxValue

    while (true) {

      if (toDevelopHeap.isEmpty) return toReturn

      val currentNodeId: Int = toDevelopHeap.removeFirst()
      val currentNode = g.nodes(currentNodeId)
      val currentNodeDistance = nodeToDistance(currentNode.id)

      if (currentNodeDistance > maxDistance) return toReturn

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

          if (isTarget(otherNodeID)) {
            //must be <= because there is also an order on centroid captured in voronoiZones
            if (newDistance <= maxDistance) {
              val newMark = VoronoiZone(otherNode, newDistance, outgoingEdge)
              if (newMark < toReturn) toReturn = newMark
              maxDistance = newDistance
            }
          }
        }
      }
    }
    throw new Error("should not be reached")
  }
}
