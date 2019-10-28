package oscar.cbls.algo.graph

import oscar.cbls.algo.graph.DijkstraDistanceMatrix.computeAllDistancesFomNode

object Connexity {

  def isGraphConnex(graph:ConditionalGraph,isConditionOpen:Int => Boolean):Boolean = {
    val distanceFromZero = computeAllDistancesFomNode(graph.nodes(0),
      graph,
      isConditionOpen)

    for(node <- graph.nodes.indices){
      if(distanceFromZero(node) == Long.MaxValue){
        println(graph.nodes(node))
        return false
      }
    }
    true
  }

  def getComponents(graph:ConditionalGraph,isConditionOpen:Int => Boolean):List[List[Node]] = {

    val isNodeReached:Array[Boolean] = Array.fill(graph.nbNodes)(false)
    var currentComponent:List[Node] = Nil

    def unmarkNoTransitNodesFromComponent(): Unit ={
      for(node <- currentComponent if !node.transitAllowed){
        isNodeReached(node.id) = false
      }
    }

    def isEdgeOpen(edge:Edge):Boolean = {
      edge.conditionID match{
        case None => true
        case Some(c) => isConditionOpen(c)
      }
    }

    def markComponentAt(node:Node):Unit = {
      for(edge <- node.incidentEdges if isEdgeOpen(edge)){
        val otherNode = edge.otherNode(node)
        if(!isNodeReached(otherNode.id)){
          isNodeReached(otherNode.id) = true
          currentComponent = otherNode :: currentComponent
          if(otherNode.transitAllowed) {
            markComponentAt(otherNode)
          }
        }
      }
    }

    def getComponentAt(node:Node):List[Node] = {
      currentComponent = List(node)
      isNodeReached(node.id) = true
      markComponentAt(node)
      currentComponent
    }

    var components:List[List[Node]] = Nil

    var nodesToExploreForComponent = graph.nodes.toList
    while(true){

      while(nodesToExploreForComponent match {
        case Nil => false
        case h :: t =>
          if (isNodeReached(h.id) || !h.transitAllowed) {
            nodesToExploreForComponent = t // we ignore it
            true
          } else {
            false
          }
      }){}

      nodesToExploreForComponent match{
        case node::t =>
          require(!isNodeReached(node.id))
          nodesToExploreForComponent = t
          components = getComponentAt(node) :: components
          unmarkNoTransitNodesFromComponent()
        case Nil =>
          //we conclude the algo

          //we look for orphan noTransitNodes
          for(nodeID <- graph.nodes.indices) isNodeReached(nodeID) = false
          for(component <- components){
            for(node <- component){
              isNodeReached(node.id) = true
            }
          }
          for(nodeID <- graph.nodes.indices) {
            if(!isNodeReached(nodeID)){
              //this must be a non transit node
              val node = graph.nodes(nodeID)
              require(!node.transitAllowed)
              components = List(node) :: components
            }
          }

          return components
      }
    }


    throw new Exception("should not be reached")
  }
}


