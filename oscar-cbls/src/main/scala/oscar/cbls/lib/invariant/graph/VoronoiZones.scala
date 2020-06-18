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

package oscar.cbls.lib.invariant.graph

import oscar.cbls.algo.graph._
import oscar.cbls.algo.quick.QList
import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.logic.{Cluster, Filter}
import oscar.cbls.lib.invariant.set.SetMap

import scala.collection.immutable.{SortedMap, SortedSet}

object VoronoiZones{
  def apply(graph:ConditionalGraph,
            graphDiameterOverApprox:Long,
            openConditions:SetValue,
            centroids:SetValue,
            trackedNodes:Iterable[Int],
            m:Store,
            defaultDistanceForUnreachableNodes:Long,
            defaultCentroidForOrphanNodes:Long = -1):VoronoiZones = {

    val trackedNodeToDistanceAndCentroid = SortedMap.empty[Int,(CBLSIntVar,CBLSIntVar)] ++ trackedNodes.map(nodeID =>
      nodeID -> (
        CBLSIntVar(m, 0, Domain(0L,(defaultDistanceForUnreachableNodes max graphDiameterOverApprox)), "distanceToClosestCentroid_Node" + nodeID),
        CBLSIntVar(m, 0, Domain(centroids.min , centroids.max) union defaultCentroidForOrphanNodes, "closestCentroidToNode" + nodeID))
    )

    new VoronoiZones(graph,
      openConditions,
      centroids,
      trackedNodeToDistanceAndCentroid,
      defaultDistanceForUnreachableNodes:Long)
  }

  def orphanNodes(v:VoronoiZones):ChangingSetValue = {
    //TODO: embed this into the VoronoiVone invariant to have better runtime?
    val idToNodeAndCentroid:Array[(Int,IntValue)] = v.trackedNodeToDistanceAndCentroidMap.toList.map({case (id,(_,centroid)) => (id,centroid)}).toArray

    SetMap(
      Filter(
        idToNodeAndCentroid.map(_._2),
        _!= v.defaultCentroidForUnreachableNodes),
      (nodeID:Int) => idToNodeAndCentroid(nodeID)._1,
      Domain(idToNodeAndCentroid.map(_._1:Long)))
  }

  def centroidToTrackedNodeSet(v:VoronoiZones, centroids:Iterable[Int]):SortedMap[Int,SetValue] = {
    val trackedNodesArray:Array[(Int,(_,CBLSIntVar))] = v.trackedNodeToDistanceAndCentroidMap.toArray
    val localIDtoNodeID:Array[Int] = trackedNodesArray.map(_._1)
    val localIDtoToClusterID:Array[IntValue] = trackedNodesArray.map(_._2._2)
    val (minTrackedNodeID,maxTrackedNodeID) = InvariantHelper.getMinMaxBoundsShortInt(localIDtoNodeID)
    val domainsOfTrackedNodeIDs = Domain(minTrackedNodeID,maxTrackedNodeID)
    val centroidsToTmpNodes:SortedMap[Int,CBLSSetVar] = Cluster.makeSparse(localIDtoToClusterID, centroids).clusters

    //SortedMap is actuall ya lazy stuff, so aboid at all cost here!
    SortedMap.empty[Int,SetValue] ++ centroidsToTmpNodes.toList.map({case (a,setOfTmpNodes) => (a,SetMap(setOfTmpNodes,(l:Int) => localIDtoNodeID(l.toInt),domainsOfTrackedNodeIDs))})
  }
}

/**
  *
  * @param graph a graph, this is a constant. it is a conditional graph, so some edges have
  *              a Boolean proposition associated to them
  * @param openConditions the set of conditions such that the edge is considered open
  * @param centroids the centroids
  * @param trackedNodeToDistanceAndCentroidMap this is the output:
  *                                            for the nodes that require it,
  *                                            the distance to the closest centroid
  *                                            and the centroid
  *                                            nodes that are not reacheable by any centroid get the centroid -1
  *
  */
class VoronoiZones(graph:ConditionalGraph,
                   openConditions:SetValue,
                   val centroids:SetValue,
                   val trackedNodeToDistanceAndCentroidMap:SortedMap[Int,(CBLSIntVar,CBLSIntVar)],
                   val defaultDistanceForUnreachableNodes:Long,
                   maxDistanceToCentroid:Long = Long.MaxValue,
                   val defaultCentroidForUnreachableNodes:Int = -1)
  extends Invariant with SetNotificationTarget {

  require(openConditions != centroids, "something absurd in the Voronoï zone declaration")

  //this condition is needed because we use the distance to unmark the voronoi zones whe na conditional edge is closed
  require(graph.conditionToConditionalEdges.forall(_.length >= 0),"all conditional edges should have length >= 0")

  require(maxDistanceToCentroid > 0)

  registerStaticAndDynamicDependency(openConditions)
  registerStaticAndDynamicDependency(centroids)

  finishInitialization()

  private val trackedNodeToDistanceAndCentroid: Array[OutputLabeling] =
    Array.tabulate(graph.nbNodes)(nodeID =>
      trackedNodeToDistanceAndCentroidMap.get(nodeID) match {
        case None => null
        case Some((distanceVar, centroidVar)) =>
          distanceVar.setDefiningInvariant(this)
          centroidVar.setDefiningInvariant(this)
          OutputLabeling(distance=distanceVar,centroid = centroidVar)
      })


  case class OutputLabeling(distance:CBLSIntVar,
                            centroid:CBLSIntVar){

    def set(l:ClosestCentroidLabeling): Unit ={
      l match{
        case Unreachable =>
          setUnreachable()

        case VoronoiZone(centroid,distance,_) =>
          this.centroid := centroid.id
          this.distance := distance
      }
    }

    def setUnreachable(): Unit = {
      this.centroid := defaultCentroidForUnreachableNodes
      this.distance := defaultDistanceForUnreachableNodes
    }

    def checkEqual(l:ClosestCentroidLabeling): Unit = {
      l match{
        case Unreachable =>
          require(this.centroid.value == defaultCentroidForUnreachableNodes)
          require(this.distance.value == defaultDistanceForUnreachableNodes)

        case VoronoiZone(centroid,distance,_) =>
          require(this.centroid.value == centroid.id)
          require(this.distance.value == distance)
      }
    }
  }

  private val isConditionalEdgeOpen: Array[Boolean] = Array.fill(graph.nbConditions)(false)

  for(c <- openConditions.value){
    isConditionalEdgeOpen(c) = true
  }

  private def isEdgeOpen(edge: Edge): Boolean =
    edge.conditionID match {
      case None => true
      case Some(condition) => isConditionalEdgeOpen(condition)
    }

  private val nodeLabeling: Array[ClosestCentroidLabeling] = Array.fill(graph.nbNodes)(Unreachable)

  private def labelNode(nodeID:Int,label:ClosestCentroidLabeling): Unit ={
    nodeLabeling(nodeID) = label
    if(trackedNodeToDistanceAndCentroid(nodeID) != null){
      trackedNodeToDistanceAndCentroid(nodeID).set(label)
    }
  }

  def spanningTree(nodes:QList[Node]):QList[Edge] = {
    require(!isScheduled,"cannot invoke spanning tree when Voronoi is not up to date!")
    var acc:QList[Edge] = null

    var toDevelop = nodes
    while(toDevelop!=null){
      val target = toDevelop.head
      val p = pathToCentroid(target)
      toDevelop = toDevelop.tail

      p match{
        case None =>
        case Some(list) =>
          //println(s"Found a path for ${target.id} -> ${list.toList.mkString(",")}")
          var toEnqueue = list
          while(toEnqueue != null) {
            acc = QList(toEnqueue.head, acc)
            toEnqueue = toEnqueue.tail
          }
      }
    }
    acc
  }

  def pathToCentroid(node:Node):Option[QList[Edge]] = {
    require(!isScheduled,"cannot invoke path to centroid when Voronoï is not up to date!")
    def pathToExistingCentroid(node:Node, targetCentroid:Node):QList[Edge] = {
      nodeLabeling(node.id) match {
        case VoronoiZone(centroid: Node, distance: Long, edge) =>
          //note that we cannot specify the type on edge because it make the match fail when edge == null
          require(centroid == targetCentroid)
          if (edge == null) {
            //z is its own centroïd
            require(distance == 0)
            require(node == targetCentroid)
            null
          } else {
            val otherNode = edge.otherNode(node)
            require(isEdgeOpen(edge))
            QList(edge, pathToExistingCentroid(otherNode, targetCentroid))
          }
        case Unreachable =>
          throw new Error("should not happen:" + nodeLabeling(node.id))
      }
    }

    nodeLabeling(node.id) match {
      case Unreachable =>
        None
      case z:VoronoiZone =>
        Some(pathToExistingCentroid(node,z.centroid))
    }
  }

  override def notifySetChanges(v: ChangingSetValue,
                                d: Int,
                                addedValues: Iterable[Int],
                                removedValues: Iterable[Int],
                                oldValue: SortedSet[Int],
                                newValue: SortedSet[Int]): Unit = {

    if (v == centroids) {
      //println("change on centroids(addedValues:" + addedValues + " removedValues:" + removedValues)
      //We ned to do the remove before the insert because
      for (removed <- removedValues) {
        loadExternalBoundaryIntoHeapMarkInnerZone(graph.nodes(removed))
      }

      for (added <- addedValues) {
        labelNode(added,VoronoiZone(graph.nodes(added),0,null))
        loadOrCorrectNodeIDIntoHeap(added,true) //centroids are force inserted
      }

    } else if (v == openConditions) {
      //opening or closing edges
      //println("changed open conditions(addedValues:" + addedValues + " removedValues:" + removedValues + " oldValue:" + oldValue + " newValue:" + newValue)
      for (added <- addedValues) {
        assert(!isConditionalEdgeOpen(added))
        //if the edge is not reachable, no need to load it.
        isConditionalEdgeOpen(added) = true
        assert(graph.conditionToConditionalEdges(added).conditionID contains  added)
        loadEdgeExtremitiesIntoHeapIfReachable(graph.conditionToConditionalEdges(added))
      }
      for (removed <- removedValues) {
        assert(isConditionalEdgeOpen(removed))
        isConditionalEdgeOpen(removed) = false
        assert(graph.conditionToConditionalEdges(removed).conditionID contains removed)
        loadExternalBoundaryIntoHeapMarkImpactedZone(graph.conditionToConditionalEdges(removed))
      }
    } else {
      require(false, "got notification for not centroid and not openConditions")
    }
    scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    //println("START perform propagation")
    performLabelingFromCurrentHeap()
    //println("END perform propagation")
  }

  //we can only put node with an existing under-approximated distance to the target, this only needs
  // to be checked on the source node, actually
  //We can have a competition between two voronoi zones, tie breaks are on smaller voronoi zone indice; the break takes place later.
  private val nodeIDHeap = new oscar.cbls.algo.heap.BinomialHeapWithMoveLong(
    nodeID => nodeLabeling(nodeID).asInstanceOf[VoronoiZone].distance, graph.nbNodes, graph.nbNodes)

  private def performLabelingFromCurrentHeap(): Unit = {
    while (!nodeIDHeap.isEmpty) {
      val currentNodeId: Int = nodeIDHeap.removeFirst()
      val currentNode = graph.nodes(currentNodeId)
      val currentNodeLabeling = nodeLabeling(currentNodeId).asInstanceOf[VoronoiZone]

      require(currentNodeLabeling.distance <= maxDistanceToCentroid)

      for(edge <- currentNode.incidentEdges){
        if (isEdgeOpen(edge)){
          val otherNode = edge.otherNode(currentNode)
          val otherNodeID = otherNode.id
          val newLabelingForOtherNode = currentNodeLabeling + edge

          if (newLabelingForOtherNode.distance <= maxDistanceToCentroid
            && newLabelingForOtherNode < nodeLabeling(otherNodeID)) { //this performs a tie break on centroid ID
            labelNode(otherNodeID,newLabelingForOtherNode)
            loadOrCorrectNodeIntoHeap(otherNode,false)
          }
        }
      }
    }
  }

  markAllNodesUnreachable()
  loadCentroidsIntoHeap(this.centroids.value)
  performLabelingFromCurrentHeap()

  private def markAllNodesUnreachable(): Unit = {
    for (node <- graph.nodes) {
      markNodeUnreachableAndRemoveFromHeapIfPresent(node:Node)
    }
  }

  private def markNodeUnreachableAndRemoveFromHeapIfPresent(node:Node): Unit ={
    val nodeID = node.id
    labelNode(nodeID,Unreachable)
    nodeIDHeap.deleteIfPresent(nodeID)
  }

  private def loadCentroidsIntoHeap(centroids: Iterable[Int]): Unit = {
    //we also mark them as centroids, actually
    for (centroid <- centroids) {
      labelNode(centroid,VoronoiZone(graph.nodes(centroid),0,null))
      loadOrCorrectNodeIDIntoHeap(centroid,true) //Centroids are force inserted
    }
  }

  private def loadOrCorrectNodeIDIntoHeap(nodeID: Int, alsoLoadTransitNode:Boolean): Unit = {
    if(alsoLoadTransitNode || graph.nodes(nodeID).transitAllowed) {
      if (nodeIDHeap.contains(nodeID)) {
        nodeIDHeap.notifyChange(nodeID)
      } else {
        //not stored yet, we store it
        nodeIDHeap.insert(nodeID)
      }
    }
  }

  private def loadOrCorrectNodeIntoHeap(node:Node,alsoLoadTransitNode:Boolean): Unit ={
    loadOrCorrectNodeIDIntoHeap(node.id,alsoLoadTransitNode)
  }

  private def loadEdgeExtremitiesIntoHeapIfReachable(edge:Edge): Unit ={
    nodeLabeling(edge.nodeIDA) match{
      case v:VoronoiZone => loadOrCorrectNodeIntoHeap(edge.nodeA,v.centroid == edge.nodeA)//we force insert centroid
      case _ => ;
    }

    nodeLabeling(edge.nodeIDB) match{
      case v:VoronoiZone => loadOrCorrectNodeIntoHeap(edge.nodeB,v.centroid == edge.nodeB) //we force insert centroid
      case _ => ;
    }
  }

  /**
    * this method is called when a conditional edge is closed.
    * it explores the potentially orphan side, and
    * @param closedEdge
    */
  private def loadExternalBoundaryIntoHeapMarkImpactedZone(closedEdge:Edge): Unit ={

    val nodeA = closedEdge.nodeA
    val markingA = nodeLabeling(nodeA.id)
    val nodeB = closedEdge.nodeB
    val markingB = nodeLabeling(nodeB.id)

    val orphanNodeOpt = (markingA,markingB) match{
      case (VoronoiZone(centroidA,dA,edgeA),VoronoiZone(centroidB,dB,edgeB)) if centroidA == centroidB =>
        if (edgeB == closedEdge){
          //nodeB is orphan
          require(edgeA != closedEdge)
          Some(nodeB)
        } else if (edgeA == closedEdge){
          //nodeA is orphan
          require(edgeB != closedEdge)
          Some(nodeA)
        }else{
          None
        }
      case _ => None
    }

    orphanNodeOpt match {
      case None => //no passing through centroid, nothing to do
      case Some(orphanNode) =>
        val orphanNodeID = orphanNode.id
        val orphanNodeLabeling = nodeLabeling(orphanNodeID).asInstanceOf[VoronoiZone]
        val minDistance = orphanNodeLabeling.distance
        val centroidThrough = orphanNodeLabeling.centroid

        markNodeUnreachableAndRemoveFromHeapIfPresent(orphanNode)

        //we use an iterative approach here with explicit front
        // because a recursive approach did lead to stack overflow in large graphs.
        var toDevelop: QList[Node] = QList(orphanNode)

        var otherReachedCentroid:SortedSet[Int] = SortedSet.empty

        while (toDevelop != null) {
          val currentNode = toDevelop.head
          toDevelop = toDevelop.tail

          for (edge <- currentNode.incidentEdges if isEdgeOpen(edge)) {
            val otherNode = edge.otherNode(currentNode)
            val otherNodeID = otherNode.id

            nodeLabeling(otherNodeID) match {
              case VoronoiZone(centroid: Node, distance: Long, incomingEdge) =>
                //TODO: this might be improved, we are unmarking too many nodes
                if (centroid == centroidThrough && distance >= minDistance) {
                  //still marking
                  markNodeUnreachableAndRemoveFromHeapIfPresent(otherNode)
                  toDevelop = QList(otherNode, toDevelop)
                }else if(centroid != centroidThrough && distance == 0 && !otherReachedCentroid.contains(centroid.id)){
                  //We are at another centroid.
                  //this one might  be a new centroid, added by another event, so we must pass over it and continue marking
                  otherReachedCentroid = otherReachedCentroid + centroid.id
                  toDevelop = QList(otherNode, toDevelop)
                  loadOrCorrectNodeIDIntoHeap(otherNodeID,true) // centroid are loaded because they must be developed.
                } else {
                  //we are at a node associated to another centroid
                  // or to the same centroid that does not take the closed edge.
                  //if this other node is a centroid, it must be force loaded
                  loadOrCorrectNodeIDIntoHeap(otherNodeID,
                    nodeLabeling(otherNodeID) match{
                      case v:VoronoiZone if v.centroid == otherNode => true;
                      case _ => false})
                }
              case _ => ; //it can be unreachable, no worries
            }
          }
        }
    }
  }

  private def loadExternalBoundaryIntoHeapMarkInnerZone(removedCentroid:Node): Unit ={
    //performed as a DFS, non-redundant exploration, so not very costly
    //TODO: try an explicit tack to replace the recursion since there is a risk of stack overflow in large graphs.
    var reachedNewCentroids:SortedSet[Int] = SortedSet.empty

    def explore(node:Node): Unit ={
      for(edge <- node.incidentEdges if isEdgeOpen(edge)){
        val otherNode = edge.otherNode(node)
        val otherNodeID = otherNode.id

        nodeLabeling(otherNodeID) match{
          case VoronoiZone(centroid:Node,distance:Long, incomingEdge) =>
            //TODO: we are unmarking too many nodes; could prune on incoming edge
            if (centroid == removedCentroid) {

              markNodeUnreachableAndRemoveFromHeapIfPresent(otherNode)

              explore(otherNode)
            }else if(centroid != removedCentroid && distance == 0 && !reachedNewCentroids.contains(centroid.id)){
              //this node was just inserted as a centroid, so we must pass over it and continue unmarking
              //but only one pass over is allowed otherwise, there is an infinite loop
              reachedNewCentroids = reachedNewCentroids + centroid.id
              loadOrCorrectNodeIDIntoHeap(otherNodeID,true) //centroids are inserted by force; they will not be transit node since they are at position zero.
              explore(otherNode)
            } else {
              //we are at anotherNode related to another centroid, so this is the new boundary
              //if this other node is a centroid, it is force inserted
              loadOrCorrectNodeIDIntoHeap(otherNodeID,
                nodeLabeling(otherNodeID) match{
                  case v:VoronoiZone if v.centroid == otherNode => true;
                  case _ => false})
            }
          case Unreachable => ;
          //we can reach an unreacable one in case two path from the removed centroid lead to the same node
        }
      }
    }

    markNodeUnreachableAndRemoveFromHeapIfPresent(removedCentroid)

    explore(removedCentroid)
  }

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  override def checkInternals(c: Checker): Unit ={
    require(nodeIDHeap.isEmpty)

    val centroids:Iterable[Node] = this.centroids.value.toList.map(nodeID => graph.nodes(nodeID))
    val isConditionalEdgeOpen = (conditionID:Int) => this.openConditions.value contains conditionID

    //checking for each node the centroid (this is very costly: nbNodes*Dijkstra)
    for(node <- graph.nodes){

      val fromScratch = new DijkstraMT(this.graph).search(
        node,
        centroids,
        isConditionalEdgeOpen)

      val incremental = nodeLabeling(node.id)

      require(fromScratch equals incremental,
        s"node:$node incremental:$incremental fromScratch:$fromScratch")
    }

    //this is mostly a static check
    for(node <- graph.nodes){
      trackedNodeToDistanceAndCentroidMap.get(node.id) match{
        case None =>
          require(trackedNodeToDistanceAndCentroid(node.id) == null)

        case Some((distanceVar,centroidVar)) =>
          require(trackedNodeToDistanceAndCentroid(node.id).centroid == centroidVar)
          require(trackedNodeToDistanceAndCentroid(node.id).distance == distanceVar)

          //this is the non-static stuff
          trackedNodeToDistanceAndCentroid(node.id).checkEqual(nodeLabeling(node.id))
      }
    }
  }

  def exportGraphToNetworkxInstructions(graph :ConditionalGraphWithIntegerNodeCoordinates, openConditions :List[Long],spanningTree :List[Edge] = List()): String ={

    var toReturn = s"nbNodes = ${graph.nbNodes}\n"

    val nonConditionalEdges = graph.edges.filter(e => e.conditionID.isEmpty).map(e => s"(${e.nodeIDA},${e.nodeIDB})").mkString(",")
    val openEdges =  graph.edges.filter(e => e.conditionID.isDefined && (openConditions(e.conditionID.get) == 1)).map(e => s"(${e.nodeIDA},${e.nodeIDB})").mkString(",")
    val closeEdges = graph.edges.filter(e => e.conditionID.isDefined && (openConditions(e.conditionID.get) == 0)).map(e => s"(${e.nodeIDA},${e.nodeIDB})").mkString(",")
    val nodesPositions = graph.coordinates.zipWithIndex.map({case (e,i) => s"$i : (${e._1},${e._2})"}).mkString(",")
    val spanningTreeString = spanningTree.map(e => s"(${e.nodeIDB},${e.nodeIDA})").mkString(",")

    toReturn = toReturn.concat(s"openEdges = [$openEdges]\n")
    toReturn = toReturn.concat(s"closedEdges = [$closeEdges]\n")
    toReturn = toReturn.concat(s"nonConditionalEdges = [$nonConditionalEdges]\n")
    toReturn = toReturn.concat(s"pos = {$nodesPositions}\n")
    toReturn = toReturn.concat(s"span = [$spanningTreeString]")

    toReturn
  }
}
