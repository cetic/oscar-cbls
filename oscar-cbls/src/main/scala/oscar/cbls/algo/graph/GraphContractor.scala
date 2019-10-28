package oscar.cbls.algo.graph

import oscar.cbls.algo.dll.{DLLStorageElement, DoublyLinkedList}


/**
  * this algo simplifies graph's by
  *  removing non connected components,
  *  collapsing linear strings into single edge
  *
  *  it returns a ContractedConditionalGraph.
  *  It is a conditional graph, which has a correspondence between the new nodes and the old nodes
  */
object GraphContractor {

  def apply(g:ConditionalGraph, shouldBeKept:Node=>Boolean):ContractedConditionalGraph = {

    /**nodeA is incident to the first edge in origin
      * nodeB is incident to the last ede in origin
      *
      * @param origin
      * @param shouldBeKept
      * @param nodeA
      * @param nodeB
      */
    class TempEdge(val origin:List[Edge],val shouldBeKept:Boolean, val nodeA:TmpNode, val nodeB:TmpNode){

      require(origin.head.isIncidentTo(nodeA.origin))
      require(origin.last.isIncidentTo(nodeB.origin))

      def otherNode(n:TmpNode):TmpNode = {
        if(n == nodeA) nodeB
        else nodeA
      }

      def delete(): Unit ={
        for(k <- keysFoDeletion){
          k.delete()
        }
      }
      var keysFoDeletion:List[DLLStorageElement[TempEdge]] = Nil
      def addKey(k:DLLStorageElement[TempEdge]){
        keysFoDeletion = k :: keysFoDeletion
      }
    }

    def createShortcutEdge(e1:TempEdge,intermediaryNode:TmpNode,e2:TempEdge):TempEdge = {

      val origin1 = if(e1.nodeA == intermediaryNode) e1.origin.reverse else e1.origin
      val origin2 = if(e2.nodeA == intermediaryNode) e2.origin else e2.origin.reverse

      new TempEdge(origin1 ::: origin2, shouldBeKept = false, nodeA = e1.otherNode(intermediaryNode), nodeB = e2.otherNode(intermediaryNode))
    }

    class TmpNode(val origin:Node,
                  val shouldBeKeptNode:Boolean,
                  val incidentEdges:DoublyLinkedList[TempEdge]){
      def degree = incidentEdges.size
      def shouldBeKep = shouldBeKeptNode || incidentEdges.toList.exists(_.shouldBeKept)

      var keyForDeletion:DLLStorageElement[TmpNode] = null

      def delete(): Unit ={
        for(e <- incidentEdges) e.delete()
      }
    }

    val tmpNodes:Array[TmpNode] = Array.fill[TmpNode](g.nbNodes)(null)

    for(n <- 0 until g.nbNodes) {
      val tmp = new TmpNode(
        g.nodes(n),
        shouldBeKept(g.nodes(n)),
        incidentEdges = new DoublyLinkedList[TempEdge])

      tmpNodes(n) = tmp
    }

    val allEdges:DoublyLinkedList[TempEdge] = new DoublyLinkedList[TempEdge]()
    for(edge <- g.edges){
      val tmp = new TempEdge(List(edge), edge.conditionID.isDefined, tmpNodes(edge.nodeIDA), tmpNodes(edge.nodeIDB))
      tmp.addKey(tmpNodes(edge.nodeIDA).incidentEdges.addElem(tmp))
      tmp.addKey(tmpNodes(edge.nodeIDB).incidentEdges.addElem(tmp))
      tmp.addKey(allEdges.addElem(tmp))
    }

    //now, keep track of nodes with degree 1 and 2 that can be deleted
    var nodesWithDegreeMax2:List[TmpNode] = Nil

    for(tmpNode <- tmpNodes){
      if(tmpNode.degree <= 2 && !tmpNode.shouldBeKeptNode){
        nodesWithDegreeMax2 = tmpNode :: nodesWithDegreeMax2
      }
    }


    def checkOtherNode(otherNode:TmpNode,prevDegree:Int){
      if(otherNode.degree <= 2
        && prevDegree >2
        && !otherNode.shouldBeKeptNode){ //en fait c'est idiot, les transit fobidden on peut les supprimer out de suite.
        nodesWithDegreeMax2 = otherNode :: nodesWithDegreeMax2
        //println("enqueue " + otherNode.origin + " degree: " + otherNode.degree)
      }
    }

    while(nodesWithDegreeMax2.nonEmpty){
      val currentNode = nodesWithDegreeMax2.head
      nodesWithDegreeMax2 = nodesWithDegreeMax2.tail

      currentNode.degree match {
        case 0 =>
          //delete it
          currentNode.delete()
          tmpNodes(currentNode.origin.id) = null

          println("delete degree 0:" + currentNode.origin)
        case 1 =>
          //delete it, also the edge

          val otherNode = currentNode.incidentEdges.head.otherNode(currentNode)
          val oldDegree = otherNode.degree

          tmpNodes(currentNode.origin.id) = null
          currentNode.delete()

          checkOtherNode(otherNode,oldDegree)

          println("delete degree 1:" + currentNode.origin)
        case 2 =>
          if (currentNode.origin.transitAllowed) {
            //delete it and add shortcuts (no need to enqueue another node here)
            val List(edge1, edge2) = currentNode.incidentEdges.toList

            tmpNodes(currentNode.origin.id) = null
            currentNode.delete()

            val newEdge = createShortcutEdge(edge1, currentNode, edge2)
            newEdge.addKey(tmpNodes(newEdge.nodeA.origin.id).incidentEdges.addElem(newEdge))
            newEdge.addKey(tmpNodes(newEdge.nodeB.origin.id).incidentEdges.addElem(newEdge))
            newEdge.addKey(allEdges.addElem(newEdge))

            println("contract degree 2 transit allowed, shortcut added: " + currentNode.origin)
          }else{
            //transit is not allowed, so we can delete it without adding more shortcuts, but we might enqueue the neighbors

            val List(edge1, edge2) = currentNode.incidentEdges.toList


            val otherNode1 = edge1.otherNode(currentNode)
            val oldDegree1 = otherNode1.degree

            val otherNode2 = edge2.otherNode(currentNode)
            val oldDegree2 = otherNode2.degree


            tmpNodes(currentNode.origin.id) = null
            currentNode.delete()

            checkOtherNode(otherNode1,oldDegree1)
            checkOtherNode(otherNode2,oldDegree2)

            println("contract degree 2 transit forbidden, check neighbors for deletion: " + currentNode.origin)
          }

        case d => throw new Error("degree " + d + ":" + currentNode.origin)
      }
    }


    //extracting the final nodes
    val nbNodes = tmpNodes.count(_ != null)

    val finalNodes:Array[Node] = Array.fill(nbNodes)(null)
    val originalNodeToNode:Array[Option[Node]] = Array.fill(g.nbNodes)(None)
    val finalNodeToOriginalNode:Array[Node] = Array.fill(nbNodes)(null)

    var newID = 0
    for(node <- tmpNodes.toList if node != null){
      //these are the final nodes
      val finalNode = new Node(newID, node.origin.transitAllowed)
      finalNodes(newID) = finalNode
      originalNodeToNode(node.origin.id) = Some(finalNode)
      finalNodeToOriginalNode(newID) = node.origin
      newID += 1
    }

    //extracting the final edges
    val nbEdges = allEdges.size

    val finalEdges:Array[Edge] = Array.fill(nbEdges)(null)
    val edgeToPathInOriginalGraph:Array[List[Edge]] = Array.fill(nbEdges)(null)

    var newEdgeID = 0
    for(edge <- allEdges.toList){

      val finalEdge = new Edge(
        newEdgeID,
        originalNodeToNode(edge.nodeA.origin.id).get,
        originalNodeToNode(edge.nodeB.origin.id).get,
        length = edge.origin.map(_.length).sum,
        conditionID = if(edge.origin.size == 1) edge.origin.head.conditionID else None)

      finalEdges(newEdgeID) = finalEdge

      edgeToPathInOriginalGraph(newEdgeID) = edge.origin

      newEdgeID += 1
    }

    val tmp = new ContractedConditionalGraph(
      originalGraph = g,
      nodes = finalNodes,
      edges = finalEdges,
      nodeToOriginalNodes = finalNodeToOriginalNode,
      originalNodeToNode = originalNodeToNode,
      edgeToPathInOriginalGraph = edgeToPathInOriginalGraph)

    println(tmp)

    tmp

  }
}

class ContractedConditionalGraph(originalGraph:ConditionalGraph,
                                 nodes:Array[Node],
                                 edges:Array[Edge],
                                 nodeToOriginalNodes:Array[Node],
                                 originalNodeToNode:Array[Option[Node]],
                                 edgeToPathInOriginalGraph:Array[List[Edge]])
  extends ConditionalGraph(nodes,edges,originalGraph.nbConditions) {

  override def toString: String = {
    "Contracted" + super.toString + "\n" +
      "edgeToPathInOriginalGraph:\n\t" + edgeToPathInOriginalGraph.mkString("\n\t")
  }

  def decontractPath(fromNodeInContractedGraph:Node,
                     edgesInContractedGraph:List[Edge],
                     toNodeInContractedGraph:Node):List[Edge] = {

    edgesInContractedGraph match {
      case Nil =>
        require(fromNodeInContractedGraph == toNodeInContractedGraph)
        Nil
      case h :: t =>
        val step = edgeToPathInOriginalGraph(h.id)
        if (h.nodeA == fromNodeInContractedGraph) {
          //do not need to reverse
          step ::: decontractPath(h.nodeB, t, toNodeInContractedGraph)
        } else{
          //reverse
          require(h.nodeA == fromNodeInContractedGraph)
          step.reverse ::: decontractPath(h.nodeA, t, toNodeInContractedGraph)
        }
    }
  }
}
