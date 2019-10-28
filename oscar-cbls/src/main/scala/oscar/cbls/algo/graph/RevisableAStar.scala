package oscar.cbls.algo.graph

import oscar.cbls.algo.quick.QList

import scala.collection.immutable.SortedSet

abstract sealed class RevisableDistance(from:Node,
                                        to:Node){
  def conditionsForRevisions:Iterable[Int]
  def requiredConditions:SortedSet[Int]
}

case class Distance(from:Node,
                    to:Node,
                    distance:Long,
                    requiredConditions:SortedSet[Int],
                    unlockingConditions:SortedSet[Int],
                    path:Option[List[Edge]]) extends RevisableDistance(from,to){
  override def conditionsForRevisions: Iterable[Int] = requiredConditions ++ unlockingConditions
}

case class NeverConnected(from:Node,to:Node) extends RevisableDistance(from,to){
  override def conditionsForRevisions: Iterable[Int] = Nil

  override def requiredConditions: SortedSet[Int] = SortedSet.empty
}

case class NotConnected(from:Node,
                        to:Node,
                        unlockingConditions:SortedSet[Int]) extends RevisableDistance(from,to){
  override def conditionsForRevisions: Iterable[Int] = unlockingConditions
  override def requiredConditions: SortedSet[Int] = SortedSet.empty
}

/**
  *
  * @param graph
  * @param underApproximatingDistance MaxInt is where thre is no connexion
  */
class RevisableAStar(graph:ConditionalGraph,
                     underApproximatingDistance:(Int,Int) => Long){

  //TODO: speed up if we know, beside the under approximating distance,
  // that the approximation is exact (ie: no conditional edge on the shortest path)
  // then either the search can be stopped andthe distance is just added and returned
  // or the path can be computed faster, given that we know the approximation is exact.

  private val nodeToDistance = Array.fill[Long](graph.nodes.length)(Long.MaxValue)

  def getPath(from:Node,to:Node,isConditionalEdgeOpen:Int => Boolean):Option[List[Edge]] = {
    search(from,
      to,
      isConditionalEdgeOpen,
      includePath = true) match {
      case d: Distance => Some(d.path.get)
      case _ => None
    }
  }

  def search(from:Node,
             to:Node,
             isConditionalEdgeOpen:Int => Boolean,
             includePath:Boolean = false
            ):RevisableDistance = {

    def isEdgeOpen(edge: Edge): Boolean =
      edge.conditionID match {
        case None => true
        case Some(condition) => isConditionalEdgeOpen(condition)
      }

    if (underApproximatingDistance(from.id, to.id) == Long.MaxValue) {
      return NeverConnected(from, to)
    }

    var reachedClosedConditions: SortedSet[Int] = SortedSet.empty

    var reachedNodeIDs:QList[Int] = null

    //we can only put node with an existing under-approximated distance to the target, this only needs to be checked on the source node, actually
    val toDevelopHeap = new oscar.cbls.algo.heap.BinomialHeapWithMoveLong(
      nodeID => nodeToDistance(nodeID) + underApproximatingDistance(nodeID, to.id),
      graph.nodes.length,
      graph.nodes.length - 1)

    val fromNodeID = from.id
    nodeToDistance(fromNodeID) = 0
    reachedNodeIDs = QList(fromNodeID)
    toDevelopHeap.insert(fromNodeID)

    while (true) {

      val currentNodeId: Int = if (toDevelopHeap.isEmpty) -1
      else toDevelopHeap.removeFirst()

      if (currentNodeId == -1 || (nodeToDistance(currentNodeId) >= nodeToDistance(to.id))) {
        //this is the exit code
        val toReturn = extractAnswerFromFinishedSearch(
          from:Node,
          to:Node,
          {
            case None => true
            case Some(c) => isConditionalEdgeOpen(c)
          },
          nodeToDistance:Array[Long],
          pruneReachedClosedConditions(reachedClosedConditions:SortedSet[Int],to.id,nodeToDistance(to.id)),
          includePath)
        resetReachedNodes(reachedNodeIDs)
        return toReturn
      }

      val currentNode = graph.nodes(currentNodeId)
      val currentNodeDistance = nodeToDistance(currentNodeId)
      for (outgoingEdge <- currentNode.incidentEdges) {
        if (isEdgeOpen(outgoingEdge)) {
          val otherNode = outgoingEdge.otherNode(currentNode)
          val otherNodeID = otherNode.id

          val oldDistance = nodeToDistance(otherNodeID)
          val newDistance = currentNodeDistance + outgoingEdge.length
          if (newDistance < oldDistance) {
            nodeToDistance(otherNodeID) = newDistance

            if(otherNode.transitAllowed || otherNode == to) {
              if (toDevelopHeap.contains(otherNodeID)) {
                //Already to explore
                toDevelopHeap.notifyChange(otherNodeID)
              } else {
                reachedNodeIDs = QList(otherNodeID, reachedNodeIDs)
                toDevelopHeap.insert(otherNodeID)
              }
            }else{
              // transit is not allowed, so we'v already updated the distance,
              // ensure the node is to be cleaned upon next call.
              // the only node where this is relevant is the target node.
              if(oldDistance == Long.MaxValue) {
                reachedNodeIDs = QList(otherNodeID, reachedNodeIDs)
              }
            }
          }

        } else {
          //it is closed, but might be open later one
          reachedClosedConditions = reachedClosedConditions + outgoingEdge.conditionID.get
        }
      }
    }
    throw new Error("should not be reached")
  }


  private def pruneReachedClosedConditions(reachedClosedConditions:SortedSet[Int],to:Int,distance:Long):SortedSet[Int] = {
    reachedClosedConditions.filter((conditionID:Int) => {
      val edge = graph.conditionToConditionalEdges(conditionID)

      val nodeAID = edge.nodeIDA
      val distanceA = nodeToDistance(nodeAID)
      val nodeBID = edge.nodeIDB
      val distanceB = nodeToDistance(nodeBID)

      val (minDistance, closestNodeID, farNodeID) = if (distanceA < distanceB) (distanceA, nodeAID, nodeBID) else (distanceB, nodeBID, nodeAID)

      minDistance + edge.length + underApproximatingDistance(farNodeID,to) <= distance
    })
  }


  private def resetReachedNodes(reachedNodes:QList[Int]): Unit ={
    var remainingNodeIDs = reachedNodes
    while(remainingNodeIDs != null){
      nodeToDistance(remainingNodeIDs.head) = Long.MaxValue
      remainingNodeIDs = remainingNodeIDs.tail
    }
  }

  private def extractAnswerFromFinishedSearch(from:Node,
                                              to:Node,
                                              isConditionalEdgeOpen:Option[Int] => Boolean,
                                              nodeToDistance:Array[Long],
                                              reachedClosedEdges: SortedSet[Int],
                                              includePath:Boolean):RevisableDistance = {

    if (nodeToDistance(to.id) == Long.MaxValue) {
      // not reached
      NotConnected(
        from: Node,
        to: Node,
        reachedClosedEdges)
    } else {
      //connected
      Distance(
        from: Node,
        to: Node,
        distance = nodeToDistance(to.id),
        requiredConditions =
          extractRequiredConditions(
            from:Node,
            to:Node,
            isConditionalEdgeOpen:Option[Int] => Boolean,
            nodeToDistance:Array[Long]),
        unlockingConditions = reachedClosedEdges,
        path = if(includePath) Some(extractPath(from:Node,
          to:Node,
          isConditionalEdgeOpen:Option[Int] => Boolean,
          nodeToDistance:Array[Long])) else None
      )
    }
  }

  private def extractRequiredConditions(from:Node,
                                        to:Node,
                                        isConditionalEdgeOpen:Option[Int] => Boolean,
                                        nodeToDistance:Array[Long]):SortedSet[Int] = {
    //we extract the set of conditions found on the actual path
    var toReturn: SortedSet[Int] = SortedSet.empty
    var currentNode: Node = to
    var currentDistance: Long = nodeToDistance(to.id)
    while (currentNode != from) {
      for (incomingEdge <- currentNode.incidentEdges if isConditionalEdgeOpen(incomingEdge.conditionID)) {

        val newNode = incomingEdge.otherNode(currentNode)
        val newDistance = nodeToDistance(newNode.id)

        if (newDistance != Long.MaxValue
          && newDistance + incomingEdge.length == currentDistance
          && newNode.transitAllowed || newNode == from) {

          currentDistance = newDistance
          currentNode = newNode

          incomingEdge.conditionID match {
            case Some(c) => toReturn = toReturn + c
            case _ => ;
          }
        }
      }
    }
    require(currentDistance == 0)
    toReturn
  }

  def extractPath(from:Node,
                  to:Node,
                  isConditionalEdgeOpen:Option[Int] => Boolean,
                  nodeToDistance:Array[Long]):List[Edge] = {
    //we extract the set of conditions found on the actual path
    var toReturn: List[Edge] = List.empty
    var currentNode: Node = to
    var currentDistance: Long = nodeToDistance(to.id)
    while (currentNode != from) {
      for (incomingEdge <- currentNode.incidentEdges if isConditionalEdgeOpen(incomingEdge.conditionID)) {

        val newNode = incomingEdge.otherNode(currentNode)
        val newDistance = nodeToDistance(newNode.id)

        if (newDistance != Long.MaxValue
          && newDistance + incomingEdge.length == currentDistance
          && newNode.transitAllowed || newNode == from) {

          currentDistance = newDistance
          currentNode = newNode

          toReturn = incomingEdge :: toReturn
        }
      }
    }
    toReturn
  }
}



