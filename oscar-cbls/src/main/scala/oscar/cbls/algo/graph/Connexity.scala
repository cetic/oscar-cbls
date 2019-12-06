package oscar.cbls.algo.graph

import scala.collection.immutable.{SortedMap, SortedSet}


object Connexity {
  def components(graph:ConditionalGraph,isConditionOpen:Int => Boolean):(Array[List[Node]]) = {
    var consideredEdges = graph.edges.filter(e =>
      e.conditionID match {
        case None => true
        case Some(condition) => isConditionOpen(condition)
      })

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

    def setMaster(nodeID: Int, newMaster: Int) {
      require(nodeToComponentHead(nodeID) == nodeID)
      require(nodeToComponentHead(newMaster) == newMaster)
      nodeToComponentHead(nodeID) = newMaster
    }

    var nbComponents = graph.nbNodes

    while (consideredEdges nonEmpty) {
      val currentEdge = consideredEdges.head
      consideredEdges = consideredEdges.tail

      val nodeAMasterId = getMasterIdUpdateIfNeeded(currentEdge.nodeA.id)
      val nodeBMasterId = getMasterIdUpdateIfNeeded(currentEdge.nodeB.id)
      if (nodeAMasterId != nodeBMasterId) {
        //merge
        setMaster(nodeAMasterId, nodeBMasterId)
        nbComponents -= 1
      }
    }

    //ensuring they are all ontheir master
    for(nodeID <- 0 until graph.nbNodes){
      getMasterIdUpdateIfNeeded(nodeID)
    }

    val keys = SortedSet.empty[Int] ++ nodeToComponentHead

    require(keys.size == nbComponents)

    val masterToComponentID = SortedMap.empty[Int, Int] ++ keys.toList.zipWithIndex

    val components=Array.fill(nbComponents)(List.empty[Node])
    for(node <- graph.nodes){
      val masterID = getMasterIdUpdateIfNeeded(node.id)
      val componentID = masterToComponentID(masterID)
      components(componentID) = node :: components(componentID)
    }
    components
  }


  def kruskal(graph:ConditionalGraph,isConditionOIpen:Int => Boolean):(List[Edge],List[Set[Node]]) = ???

}
