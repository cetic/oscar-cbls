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
  private val nodeToIncomingNodeArray = Array.fill[Int](graph.nodes.length)(-1)

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
    nodeToIncomingNodeArray(fromNodeID) = -1
    reachedNodeIDs = QList(fromNodeID)
    toDevelopHeap.insert(fromNodeID)
    // println("From: " + from)
    // println("To: " + to)

    while (true) {

      //println("toto1")
      val currentNodeId: Int = if (toDevelopHeap.isEmpty) -1
      else toDevelopHeap.removeFirst()
      //println("toto2")

      if (currentNodeId == -1 || (nodeToDistance(currentNodeId) >= nodeToDistance(to.id))) {
        //this is the exit code
        //println("tata1")
        val toReturn = extractAnswerFromFinishedSearch(
          from:Node,
          to:Node,
          {
            case None => true
            case Some(c) => isConditionalEdgeOpen(c)
          },
          nodeToDistance:Array[Long],
          nodeToIncomingNodeArray:Array[Int],
          pruneReachedClosedConditions(reachedClosedConditions:SortedSet[Int],to.id,nodeToDistance(to.id)),
          includePath)
        //println("tata2")
        resetReachedNodes(reachedNodeIDs)
        return toReturn
      }
      //println("toto3")

      val currentNode = graph.nodes(currentNodeId)
      val currentNodeDistance = nodeToDistance(currentNodeId)
      // println("Current Node:" + currentNode)
      // println("Heap:" + toDevelopHeap)
      // println(nodeToDistance(24).toString() + " + " + underApproximatingDistance(24,to.id))
      // println(nodeToDistance(53).toString() + " + " + underApproximatingDistance(53,to.id))

      //println(Array.tabulate(nodeToDistance.length)(i => (i,nodeToDistance(i))).filter(_._2 != Long.MaxValue).map(i => i._1 + "->" + i._2).mkString("\n"))
      for (outgoingEdge <- currentNode.incidentEdges) {
        // println(outgoingEdge)
        //println("toto4")

        if (isEdgeOpen(outgoingEdge)) {
          val otherNode = outgoingEdge.otherNode(currentNode)
          val otherNodeID = otherNode.id

          val oldDistance = nodeToDistance(otherNodeID)
          val newDistance = currentNodeDistance + outgoingEdge.length
          //println("toto5")


          if (newDistance < oldDistance) {
            nodeToDistance(otherNodeID) = newDistance
            nodeToIncomingNodeArray(otherNodeID) = currentNode.id
            //println("toto6")

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
      //println(Array.tabulate(nodeToDistance.length)(i => (i,nodeToDistance(i))).filter(_._2 != Long.MaxValue).map(i => i._1 + "->" + i._2).mkString("\n"))

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
      nodeToIncomingNodeArray(remainingNodeIDs.head) = -1
      remainingNodeIDs = remainingNodeIDs.tail
    }
  }

  private def extractAnswerFromFinishedSearch(from:Node,
                                              to:Node,
                                              isConditionalEdgeOpen:Option[Int] => Boolean,
                                              nodeToDistance:Array[Long],
                                              nodeToIncomingNode:Array[Int],
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
            nodeToDistance:Array[Long],
            nodeToIncomingNode:Array[Int]),
        unlockingConditions = reachedClosedEdges,
        path = if(includePath) Some(extractPath(from:Node,
          to:Node,
          isConditionalEdgeOpen:Option[Int] => Boolean,
          nodeToDistance:Array[Long],
          nodeToIncomingNode:Array[Int])) else None
      )
    }
  }

  private def extractRequiredConditions(from:Node,
                                        to:Node,
                                        isConditionalEdgeOpen:Option[Int] => Boolean,
                                        nodeToDistance:Array[Long],
                                        nodeToIncomingNode:Array[Int]):SortedSet[Int] = {
    //we extract the set of conditions found on the actual path
    //println("RequiredCondition")
    var toReturn: SortedSet[Int] = SortedSet.empty
    var currentNode: Node = to
    var currentDistance: Long = nodeToDistance(to.id)
    // println(from)
    // println(to)
    var toPrint = true
    while (currentNode != from) {
      // if (toPrint)
      //   println(currentNode)

      for (incomingEdge <- currentNode.incidentEdges if isConditionalEdgeOpen(incomingEdge.conditionID)) {
        // if (toPrint) {
        //   println(incomingEdge)
        //   println(currentDistance)
        //   println(Array.tabulate(nodeToDistance.length)(i => i + " -> " + nodeToDistance(i) + " - " + nodeToIncomingNode(i)).mkString("\n"))
        // }
        val newNode = incomingEdge.otherNode(currentNode)
        val newDistance = nodeToDistance(newNode.id)

        if (newDistance != Long.MaxValue
          && newDistance + incomingEdge.length == currentDistance
          && (nodeToIncomingNode(currentNode.id) == newNode.id)
          && (newNode.transitAllowed || newNode == from)) {

          currentDistance = newDistance
          currentNode = newNode

          incomingEdge.conditionID match {
            case Some(c) => toReturn = toReturn + c
            case _ => ;
          }
        }
      }
      toPrint = false
    }
    //println("End RequiredCondition")
    require(currentDistance == 0)
    toReturn
  }

  def extractPath(from:Node,
                  to:Node,
                  isConditionalEdgeOpen:Option[Int] => Boolean,
                  nodeToDistance:Array[Long],
                  nodeToIncomingNode:Array[Int]):List[Edge] = {
    //we extract the set of conditions found on the actual path
    //println("Extract path")
    var toReturn: List[Edge] = List.empty
    var currentNode: Node = to
    var currentDistance: Long = nodeToDistance(to.id)
    while (currentNode != from) {
      for (incomingEdge <- currentNode.incidentEdges if isConditionalEdgeOpen(incomingEdge.conditionID)) {

        val newNode = incomingEdge.otherNode(currentNode)
        val newDistance = nodeToDistance(newNode.id)

        if (newDistance != Long.MaxValue
          && newDistance + incomingEdge.length == currentDistance
          && (nodeToIncomingNode(currentNode.id) == newNode.id)
          && (newNode.transitAllowed || newNode == from)) {

          currentDistance = newDistance
          currentNode = newNode

          toReturn = incomingEdge :: toReturn
        }
      }
    }
    //println("Extracted")
    toReturn
  }
}
