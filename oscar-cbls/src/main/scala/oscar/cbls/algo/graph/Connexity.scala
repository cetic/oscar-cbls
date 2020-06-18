package oscar.cbls.algo.graph

import scala.collection.immutable.{SortedMap, SortedSet}

object Connexity {
  //TODO: these algo do not consider non transit nodes that constitute a boundary between different components; so far, we consider transit at all nodes.

  def components(graph:ConditionalGraph,
                 isConditionOpen:Int => Boolean):Array[List[Node]] = {
    val consideredEdges = graph.edges.toList.filter(e =>
      e.conditionID match {
        case None => true
        case Some(condition) => isConditionOpen(condition)
      })
    performMerges(graph,consideredEdges).map(_._1)
  }

  def kruskal(graph:ConditionalGraph,
              isConditionOpen:Int => Boolean):Array[(List[Node],List[Edge])] = {
    val consideredEdges = graph.edges.toList.filter(e =>
      e.conditionID match {
        case None => true
        case Some(condition) => isConditionOpen(condition)
      }).sortBy(_.length)

    performMerges(graph,consideredEdges)
  }

  private def performMerges(graph:ConditionalGraph,
                            edgesToConsider:List[Edge]):Array[(List[Node],List[Edge])] = {
    var remainingEdges:List[Edge] = edgesToConsider
    val nodeToComponentHead = Array.tabulate(graph.nbNodes)(nodeID => nodeID) //nodes are their own master at startup

    def getMasterIdUpdateIfNeeded(nodeID: Int): Int = {
      val master = nodeToComponentHead(nodeID)
      if (nodeToComponentHead(master) == master) {
        //no update needed for him
        master
      } else {
        val newMaster = getMasterIdUpdateIfNeeded(master)
        nodeToComponentHead(nodeID) = newMaster
        newMaster
      }
    }

    def setMaster(nodeID: Int, newMaster: Int): Unit = {
      require(nodeToComponentHead(nodeID) == nodeID)
      require(nodeToComponentHead(newMaster) == newMaster)
      nodeToComponentHead(nodeID) = newMaster
    }

    var nbComponents = graph.nbNodes

    var connexityEdges:List[Edge] = Nil

    while (remainingEdges nonEmpty) {
      val currentEdge = remainingEdges.head
      remainingEdges = remainingEdges.tail

      val nodeAMasterId = getMasterIdUpdateIfNeeded(currentEdge.nodeA.id)
      val nodeBMasterId = getMasterIdUpdateIfNeeded(currentEdge.nodeB.id)
      if (nodeAMasterId != nodeBMasterId) {
        //merge
        setMaster(nodeAMasterId, nodeBMasterId)
        nbComponents -= 1
        connexityEdges = currentEdge :: connexityEdges
      }
    }

    //ensuring they are all on their master
    for(nodeID <- 0 until graph.nbNodes){
      getMasterIdUpdateIfNeeded(nodeID)
    }

    val keys = SortedSet.empty[Int] ++ nodeToComponentHead

    require(keys.size == nbComponents)

    val masterToComponentID = SortedMap.empty[Int, Int] ++ keys.toList.zipWithIndex

    val componentsNode=Array.fill(nbComponents)(List.empty[Node])
    val componentsEdges=Array.fill(nbComponents)(List.empty[Edge])
    for(node <- graph.nodes){
      val masterID = getMasterIdUpdateIfNeeded(node.id)
      val componentID = masterToComponentID(masterID)
      componentsNode(componentID) = node :: componentsNode(componentID)
    }
    for(edge <- connexityEdges){
      val masterID = getMasterIdUpdateIfNeeded(edge.nodeA.id)
      val componentID = masterToComponentID(masterID)
      componentsEdges(componentID) = edge :: componentsEdges(componentID)
    }

    Array.tabulate(nbComponents)(component => (componentsNode(component), componentsEdges(component)))
  }

}
