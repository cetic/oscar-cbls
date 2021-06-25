package oscar.cbls.lib.search.neighborhoods.vlsn

import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{Move, MoveFound, Neighborhood, NoMoveFound}

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.reflect.ClassTag
import scala.util.Random

class MoveExplorer(v:Int,
                   vehicleToRoutedNodes:Map[Int,Iterable[Int]],
                   unroutedNodesToInsert:Iterable[Int],
                   nodeToRelevantVehicles:Map[Int,Iterable[Int]],

                   targetVehicleNodeToInsertNeighborhood:Int => Int => Neighborhood,
                   targetVehicleNodeToMoveNeighborhood:Int => Int => Neighborhood,
                   nodeToRemoveNeighborhood:Int => Neighborhood,

                   removeAndReInsert:Int => () => Unit,

                   vehicleToObjectives:Array[Objective],
                   unroutedNodesPenalty:Objective,
                   globalObjective:Objective,

                   cache:CachedExplorations,
                   verbose:Boolean,
                   enrichment:EnrichmentParameters,
                   prioritizeMoveNoEject:Boolean = true
                  ) {

  //nodes are all the nodes to consider, ll the vehicles, and a trashNode

  //nodes of the moveGraph are:
  // if the point is routed: removing a node from the vehicle where it is
  // if the point is not routed: routing it if not in the sequence
  //there is a trashNode representing non-routed nodes. only for symbolic purpose.
  //edges are:
  // if from and to are routed: the move that moves the from point to the vehicle of the to point, assuming the to point has been removed from its vehicle
  // if from is routed, and to is a vehicle: moving a node to the vehicle without removing othernode from the vehicle
  // if from is not routed, and to is a vehicle: inserting th node onto the vehicle without removing other node from the vehicle (althoug hthis move is very simple and should not be part of VLSN explration...)
  // if from is routed, and to is not routed: removing the from, assuming the to has been inserted (these one are generated automatically, by just removing points one after the other and evaluating the penalty for unrouting)
  // if the from is not routed and the to is routed: routing the from on the vehicle of the to, assuming the to has been removed from its vehicle
  //there is a noMove edge from each vehicle to TrashNode, and a noMove edge fro trashNode to all unrouted node and all routed nodes

  //label of nodes are:
  // for each routed node and vehicle node: the vehicle of the node
  // For each unrouted node: a different label
  // a different label for the trashNode

  val initialVehicleToObjectives: Array[Long] = vehicleToObjectives.map(_.value)
  var initialUnroutedNodesPenalty: Long = unroutedNodesPenalty.value
  var initialGlobalObjective: Long = globalObjective.value

  val nodesToMove: Iterable[Int] = vehicleToRoutedNodes.flatMap(_._2)

  //label of nodes are:
  // for each routed node and vehicle node: the vehicle of the node
  // For each unrouted node: a different label
  // a different label for the trashNode
  //as labels, we take the vehicles, plus one label per non-routed node
  val nodeBuilder = new VLSNNodeBuilder(nbLabels = v)

  var nodeIDToNode: SortedMap[Int, Node] = SortedMap.empty
  val relevantVehicles: SortedSet[Int] = SortedSet.empty[Int] ++ nodeToRelevantVehicles.flatMap(_._2)
  val vehicleToNode: Array[Node] = Array.fill(v)(null)
  for (vehicle <- relevantVehicles) {
    val node = nodeBuilder.addNode(-vehicle, vehicle, vehicle, VLSNSNodeType.VehicleNode)
    vehicleToNode(vehicle) = node
    nodeIDToNode += ((-vehicle, node))
  }

  //noeud cible pour l'unroutage, label is v
  val trashNode: Node = nodeBuilder.addNode(-1, -1, nodeBuilder.newFreshLabel(), VLSNSNodeType.FictiveNode)

  //noeuds pour les noeud à déplacer
  for ((vehicle, routedNodesOnVehicle) <- vehicleToRoutedNodes) {
    require(vehicle < v)
    for (nodeID <- routedNodesOnVehicle) {
      //require(nodeID >= v, "cannot put vehicle to move :" + nodeID)
      nodeIDToNode += ((nodeID, nodeBuilder.addNode(nodeID, vehicle, vehicle, VLSNSNodeType.RegularNode)))
    }
  }

  //noeuds non routés
  for (unroutedNode <- unroutedNodesToInsert) {
    nodeIDToNode += ((unroutedNode, nodeBuilder.addNode(unroutedNode, v, nodeBuilder.newFreshLabel(), VLSNSNodeType.UnroutedNode)))
  }

  val (nodes:Array[Node],nbLabels:Int) = nodeBuilder.finish()

  val edgeBuilder: VLSNEdgeBuilder = new VLSNEdgeBuilder(nodes, nbLabels, v)

  val nbNodesInVLSNGraph: Int = nodes.length
  def nbEdgesInGraph:Int = edgeBuilder.nbEdges

  val acceptAllButMaxInt: (Long, Long) => Boolean = (_, newObj: Long) => newObj != Long.MaxValue

  // /////////////////////////////////////////////////////////////
  //about incrementality
  val isVehicleDirty:Array[Boolean] = Array.fill(v)(false)
  val isNodeDirty:Array[Boolean] = Array.fill(((nodesToMove ++ unroutedNodesToInsert).max)+1)(false)

  val nodeToNodeRemoveEdge:Array[Edge]= Array.fill(((nodesToMove ++ unroutedNodesToInsert).max)+1)(null)

  var newlyAddedPriorityCycles:List[List[Edge]] = Nil

  // /////////////////////////////////////////////////////////////
  //creating all cheap edges

  addNoMoveEdgesVehiclesToTrashNode()
  addTrashNodeToUnroutedNodes()
  exploreNodeEjection()
  exploreDeletions()

  // ////////////////////////////////////////////////////////////
  // gradual enrichment procedure
  // instantiating all potential edges and bundles
  //these are stored in bundles below, and postponed until performed.

  val maxInsertNoEjectPerVehicleAndPerIteration: Int = v
  val maxMoveNoEjectPerVehiclePerIteration: Int = v

  var allBundlesTmp:List[EdgeToExploreBundle[_]] = Nil
  def registerEdgeBundle(edgeBundle:EdgeToExploreBundle[_]):Unit = {
    allBundlesTmp = edgeBundle :: allBundlesTmp
  }

  generateInsertions()
  generateMoves()

  var allBundlesArray:Array[EdgeToExploreBundle[_]] = Random.shuffle(allBundlesTmp).toArray

  var priorityBundleArray:Array[EdgeToExploreBundle[_]] = if(prioritizeMoveNoEject){
    val tmp = allBundlesArray
    allBundlesArray = allBundlesArray.filter(bundle => !bundle.isInstanceOf[MoveNoEjectBundle] && !bundle.isInstanceOf[InsertNoEjectBundle])
    tmp.filter(bundle => bundle.isInstanceOf[MoveNoEjectBundle] || bundle.isInstanceOf[InsertNoEjectBundle])
  }else{
    Array.ofDim(0)
  }

  var nbBundles:Int = allBundlesArray.length
  var nbPriorityBundles:Int = priorityBundleArray.length

  // ////////////////////////////////////////////////////////////

  def injectAllCache(verbose:Boolean): Unit = {
    var targetPosition = 0
    for(i <- 0 until nbBundles) {
      allBundlesArray(i).loadAllCache()
      if(allBundlesArray(i).isEmpty){
        allBundlesArray(i) = null
      }else{
        allBundlesArray(targetPosition) = allBundlesArray(i)
        targetPosition += 1
      }
    }
    nbBundles = targetPosition

    //priorityBundles are handled separatedly
    targetPosition = 0
    for(i <- 0 until nbPriorityBundles) {
      priorityBundleArray(i).loadAllCache()
      if(priorityBundleArray(i).isEmpty){
        priorityBundleArray(i) = null
      }else{
        priorityBundleArray(targetPosition) = priorityBundleArray(i)
        targetPosition += 1
      }
    }
    nbPriorityBundles = targetPosition
  }

  //the new VLSN graph and a boolean telling if there is more to do or not

  /**
   *
   * @param dirtyNodes
   * @param dirtyVehicles
   * @param verbose
   * @return the enriched graph, true if there is more to load, the number of added edges, and a list of priority edge that can be taken as is (no need for cycle detection)
   */
  def enrichGraph(dirtyNodes:Iterable[Int], dirtyVehicles:Iterable[Int], verbose:Boolean):(VLSNGraph,Boolean,Int, List[List[Edge]]) = {

    newlyAddedPriorityCycles = Nil

    for(node <- dirtyNodes) isNodeDirty(node) = true
    for(vehicle <- dirtyVehicles) {
      isVehicleDirty(vehicle) = true
      initialVehicleToObjectives(vehicle) = vehicleToObjectives(vehicle).value
    }

    var totalExplored = 0

    val nbEdgesAtStart = nbEdgesInGraph

    while((totalExplored <= enrichment.minNbEdgesToExplorePerLevel || (nbEdgesInGraph - nbEdgesAtStart < enrichment.minNbAddedEdgesPerLevel) || nbPriorityBundles <= 1) && nbPriorityBundles > 0){
      //Random selection of next bundle
      val currentPriorityBundleId = if(nbPriorityBundles == 1) 1 else Random.nextInt(nbPriorityBundles-1)
      //TODO: all movesNoEject (thus inserts and moves) should mark related nodes & vehicle as dirty if they have negative delta on obj.
      val nbExplored = priorityBundleArray(currentPriorityBundleId).pruneExplore(targetNbExplores = enrichment.nbEdgesPerBundle/10)
      totalExplored += nbExplored
      if(priorityBundleArray(currentPriorityBundleId).isEmpty){
        if(currentPriorityBundleId == nbPriorityBundles-1){
          priorityBundleArray(currentPriorityBundleId) = null
        }else{
          priorityBundleArray(currentPriorityBundleId) = priorityBundleArray(nbPriorityBundles-1)
        }
        nbPriorityBundles = nbPriorityBundles -1
      }
    }

    while((totalExplored <= enrichment.minNbEdgesToExplorePerLevel || (nbEdgesInGraph - nbEdgesAtStart < enrichment.minNbAddedEdgesPerLevel) || nbBundles <= 1) && nbBundles > 0){
      //Random selection of next bundle
      val currentBundleId = if(nbBundles == 1) 1 else Random.nextInt(nbBundles-1)
      //TODO: we should foster moveNoEject first because they are really fast to explore...
      val nbExplored = allBundlesArray(currentBundleId).pruneExplore(targetNbExplores = enrichment.nbEdgesPerBundle)
      totalExplored += nbExplored
      if(allBundlesArray(currentBundleId).isEmpty){
        if(currentBundleId == nbBundles-1){
          allBundlesArray(currentBundleId) = null
        }else{
          allBundlesArray(currentBundleId) = allBundlesArray(nbBundles-1)
        }
        nbBundles = nbBundles -1
      }
    }

    (edgeBuilder.buildGraph(),nbBundles!=0,totalExplored, newlyAddedPriorityCycles)
  }

  def allMovesExplored:Boolean = {
    require(nbBundles>=0)
    require(nbPriorityBundles >= 0)
    nbBundles == 0 && nbPriorityBundles == 0
  }

  // ////////////////////////////////////////////////////////////

  abstract class EdgeToExploreBundle[E:ClassTag](toNode: Int,
                                                 toVehicle: Int,
                                                 initPotentialEdges: List[E]) {
    //non null are stored from position 0 until size-1
    private var potentialEdges: Array[E] = Random.shuffle(initPotentialEdges).toArray
    var size: Int = potentialEdges.length
    require(potentialEdges.length <= size)

    def isEmpty: Boolean = size == 0

    def exploreEdge(edge: E): Unit

    def loadEdgeFromCache(edge:E):Boolean

    def isEdgeDirty(edge: E): Boolean

    private var allCacheLoaded:Boolean = false

    def loadAllCache(): Unit ={
      if(allCacheLoaded) return
      allCacheLoaded = true
      if(cache == null || size == 0) return
      var targetPosition = 0
      for(i <- 0 until size){
        if (loadEdgeFromCache(potentialEdges(i))){
          potentialEdges(i) = null.asInstanceOf[E]
        }else{
          potentialEdges(targetPosition) = potentialEdges(i)
          targetPosition += 1
        }
      }
      size = targetPosition
      garbageCollectIfNeeded()
    }

    private def garbageCollectIfNeeded():Unit = {
      if (size == 0) {
        potentialEdges = null
      } else if (size <= potentialEdges.length / 100) {
        val tmp = Array.tabulate(size)(i => potentialEdges(i))
        potentialEdges = tmp
      }
    }

    def pruneExplore(targetNbExplores: Int): Int = {
      if(verbose) println(s"exploring $targetNbExplores edges of bundle $this")
      if (targetNbExplores <= 0) {
        0
      } else if (isNodeDirty(toNode) || isVehicleDirty(toVehicle)) {
        potentialEdges = null
        size = 0
        0
      } else {
        require(potentialEdges.length >= size, s"potentialEdges.length:${potentialEdges.length} >= size:$size")

        var toReturn = 0
        var toExplore = targetNbExplores
        while (toExplore != 0 && size != 0 && !isNodeDirty(toNode) && !isVehicleDirty(toVehicle)) {
          if (!isEdgeDirty(potentialEdges(size-1))) {
            if(cache == null || allCacheLoaded || !loadEdgeFromCache(potentialEdges(size-1))) {
              exploreEdge(potentialEdges(size-1))
            }
            toReturn += 1
            toExplore = toExplore - 1
          }
          potentialEdges(size-1) = null.asInstanceOf[E]
          size = size - 1
        }
        garbageCollectIfNeeded()
        toReturn
      }
    }
  }

  // ////////////////////////////////////////////////////////////
  // ////////////////////////////////////////////////////////////

  private def generateInsertions(): Unit = {
    val vehicleAndUnroutedNodes: Iterable[(Int, Int)] =
      unroutedNodesToInsert.flatMap((unroutedNode:Int) =>
        nodeToRelevantVehicles(unroutedNode).map((vehicle:Int) => (vehicle, unroutedNode)))

    val vehicleToUnroutedNodeToInsert = vehicleAndUnroutedNodes.groupBy(_._1).view.mapValues(_.map(_._2))

    for ((targetVehicleForInsertion, unroutedNodesToInsert) <- vehicleToUnroutedNodeToInsert) {
      //without eject
      registerEdgeBundle(
        new InsertNoEjectBundle(
          targetVehicleForInsertion,
          unroutedNodesToInsert.toList))

      //with eject
      for (routingNodeToRemove <- vehicleToRoutedNodes(targetVehicleForInsertion)) {
        registerEdgeBundle(
          new InsertWithEjectBundle(
            toNode = routingNodeToRemove,
            toVehicle = targetVehicleForInsertion,
            nodesToInsert = unroutedNodesToInsert.toList))
      }
    }
  }

  def generateMoves(): Unit = {
    val vehicleAndNodeToMove:Iterable[(Int,Int)] =
      nodesToMove.flatMap(nodeToMove =>
        nodeToRelevantVehicles(nodeToMove).map(vehicle => (vehicle,nodeToMove)))

    val vehicleToNodeToMoveThere = vehicleAndNodeToMove.groupBy(_._1).view.mapValues(_.map(_._2))

    for ((toVehicle, routedNodesToMoveThere) <- vehicleToNodeToMoveThere) {
      //without remove
      registerEdgeBundle(new MoveNoEjectBundle(toVehicle = toVehicle,
        fromNodeVehicle = routedNodesToMoveThere.flatMap(node => {
          val symbolicNodeOfNodeToMove = nodeIDToNode(node)
          val fromVehicle = symbolicNodeOfNodeToMove.vehicle
          if(fromVehicle == toVehicle) Nil
          else Some(NodeVehicle(node,fromVehicle))
        }).toList))

      //moves with removes
      for (nodeIDToEject <- vehicleToRoutedNodes(toVehicle)) {
        registerEdgeBundle(
          new MoveWithEjectBundle(nodeIDToEject,
            toVehicle,
            fromNodeVehicle =
              routedNodesToMoveThere.flatMap(node => {
                val symbolicNodeOfNodeToMove = nodeIDToNode(node)
                val fromVehicle = symbolicNodeOfNodeToMove.vehicle
                if(fromVehicle == toVehicle) Nil
                else Some(NodeVehicle(node, fromVehicle))
              }).toList
          )
        )
      }
    }
  }

  // /////////////////////////////////////////////////////////////////////////////////////////////////////////////

  case class NodeVehicle(node: Int, vehicle: Int)

  class MoveWithEjectBundle(toNode: Int,
                            toVehicle: Int,
                            fromNodeVehicle: List[NodeVehicle])
    extends EdgeToExploreBundle[NodeVehicle](toNode, toVehicle, fromNodeVehicle) {

    override def toString: String = s"MoveWithEjectBundle(size:$size, toNode: $toNode, toVehicle:$toVehicle, fromNodeVehicle:${fromNodeVehicle.mkString(",")}"

    override def isEdgeDirty(edge: NodeVehicle): Boolean = {
      isNodeDirty(edge.node) || isVehicleDirty(edge.vehicle)
    }

    private var nodeToMoveToNeighborhood: Int => Neighborhood = _
    private var reInsert: () => Unit = _

    override def loadEdgeFromCache(edge: NodeVehicle): Boolean = {
      cache.getMoveToVehicleWithRemove(edge.node,edge.vehicle,toVehicle,toNode) match{
        case CachedAtomicMove(move:Move,delta:Long) =>
          val symbolicNodeOfNodeToMove = nodeIDToNode(edge.node)
          val symbolicNodeToEject = nodeIDToNode(toNode)

          edgeBuilder.addEdge(symbolicNodeOfNodeToMove,
            symbolicNodeToEject, delta, move, VLSNMoveType.MoveWithEject)

          true
        case CachedAtomicNoMove => true
        case CacheDirty => false
      }
    }

    override def exploreEdge(edge: NodeVehicle): Unit = {
      //ensureNeighborhoodAndRemove, we do it lazyly because there might be nothing to do, actually
      // (although it is not quite sure that this actually useful at all)
      if (nodeToMoveToNeighborhood == null) {
        reInsert = removeAndReInsert(toNode)
        nodeToMoveToNeighborhood = targetVehicleNodeToMoveNeighborhood(toVehicle)
      }

      val fromNode = edge.node
      nodeToMoveToNeighborhood(fromNode).getMove(
        vehicleToObjectives(toVehicle),
        initialVehicleToObjectives(toVehicle),
        acceptanceCriterion = acceptAllButMaxInt) match {
        case NoMoveFound => ;
        case MoveFound(move) =>
          val delta = move.objAfter - initialVehicleToObjectives(toVehicle)
          val symbolicNodeOfNodeToMove = nodeIDToNode(fromNode)
          val symbolicNodeToEject = nodeIDToNode(toNode)

          edgeBuilder.addEdge(symbolicNodeOfNodeToMove, symbolicNodeToEject, delta, move, VLSNMoveType.MoveWithEject)
      }
    }

    override def pruneExplore(targetNbExplores: Int): Int = {
      val toReturn = super.pruneExplore(targetNbExplores)
      if (reInsert != null) {
        reInsert()
        reInsert = null
        nodeToMoveToNeighborhood = null
      }
      toReturn
    }
  }

  class MoveNoEjectBundle(toVehicle: Int,
                          fromNodeVehicle: List[NodeVehicle])
    extends EdgeToExploreBundle[NodeVehicle](toVehicle, toVehicle, fromNodeVehicle) {

    override def isEdgeDirty(edge: NodeVehicle): Boolean = {
      isNodeDirty(edge.node) || isVehicleDirty(edge.vehicle) || isVehicleDirty(toVehicle)
    }

    private var nodeToMoveToNeighborhood: Int => Neighborhood = null

    override def loadEdgeFromCache(edge: NodeVehicle): Boolean = {
      cache.getMoveToVehicleNoRemove(edge.node, edge.vehicle, toVehicle) match{
        case CachedAtomicMove(move:Move,delta:Long) =>
          edgeBuilder.addEdge(nodeIDToNode(edge.node), vehicleToNode(toVehicle), delta, move, VLSNMoveType.MoveNoEject)
          true
        case CachedAtomicNoMove =>
          true
        case CacheDirty =>
          false
      }
    }

    override def exploreEdge(edge: NodeVehicle): Unit = {

      //ensureNeighborhoodAndRemove, we do it lazyly because there might be nothing to do, actually
      // (although it is not quite sure that this actually useful at all)
      if (nodeToMoveToNeighborhood == null) {
        nodeToMoveToNeighborhood = targetVehicleNodeToMoveNeighborhood(toVehicle)
      }

      require(!isNodeDirty(edge.node))
        require(!isVehicleDirty(toVehicle))
        require(!isVehicleDirty(nodeIDToNode(edge.node).vehicle))

      require(toVehicle != nodeIDToNode(edge.node).vehicle, "moving to same vehicle?!")

      nodeToMoveToNeighborhood(edge.node).getMove(
        vehicleToObjectives(toVehicle),
        initialVehicleToObjectives(toVehicle),
        acceptanceCriterion = acceptAllButMaxInt) match {
        case NoMoveFound => ;
        case MoveFound(move) =>
          val delta = move.objAfter - initialVehicleToObjectives(toVehicle)
          val graphEdge = edgeBuilder.addEdge(nodeIDToNode(edge.node), vehicleToNode(toVehicle), delta, move, VLSNMoveType.MoveNoEject)

          val nodeRemoveEdge = nodeToNodeRemoveEdge(nodeIDToNode(edge.node).nodeID)
          //this prevents moves with same vehicle or node to be explored (would be faster to bypass VLSN & cycle search actually)
          if(prioritizeMoveNoEject && nodeRemoveEdge != null && delta + nodeRemoveEdge.deltaObj < 0){
            isNodeDirty(edge.node) = true
            isVehicleDirty(toVehicle) = true
            isVehicleDirty(nodeIDToNode(edge.node).vehicle) = true

            newlyAddedPriorityCycles = List(nodeRemoveEdge,graphEdge) :: newlyAddedPriorityCycles
          }
      }
    }

    override def pruneExplore(targetNbExplores: Int): Int = {
      val toReturn = super.pruneExplore(targetNbExplores min maxMoveNoEjectPerVehiclePerIteration)
      if (nodeToMoveToNeighborhood != null) {
        nodeToMoveToNeighborhood = null
      }
      toReturn
    }
  }

  // /////////////////////////////////////////////////////////////////////////////////////////////////////////////

  class InsertWithEjectBundle(toNode: Int,
                              toVehicle: Int,
                              nodesToInsert: List[Int])
    extends EdgeToExploreBundle[Int](toNode, toVehicle, nodesToInsert) {

    override def isEdgeDirty(edge: Int): Boolean = {
      isNodeDirty(edge)
    }

    private var nodeToInsertToNeighborhood: Int => Neighborhood = null
    private var reInsert: () => Unit = null

    private var initObjsDefined: Boolean = false
    private var unroutedObjAfterRemove: Long = -1
    private var correctedGlobalInit: Long = -1

    private def ensureNeighborhoodAndReInsert(): Unit = {
      if (nodeToInsertToNeighborhood == null) {
        reInsert = removeAndReInsert(toNode)
        if (!initObjsDefined) {
          unroutedObjAfterRemove = unroutedNodesPenalty.value
          correctedGlobalInit = initialGlobalObjective - initialUnroutedNodesPenalty + unroutedObjAfterRemove
          initObjsDefined = true
        }
        nodeToInsertToNeighborhood = targetVehicleNodeToInsertNeighborhood(toVehicle)
      }
    }

    override def loadEdgeFromCache(edge: Int): Boolean = {
      cache.getInsertOnVehicleWithRemove(edge,
        toVehicle,
        toNode) match{
        case CachedAtomicMove(move:Move,delta:Long) =>
          val symbolicNodeToInsert = nodeIDToNode(edge)
          val symbolicNodeToRemove = nodeIDToNode(toNode)
          edgeBuilder.addEdge(symbolicNodeToInsert, symbolicNodeToRemove, delta, move, VLSNMoveType.InsertWithEject)
          true
        case CachedAtomicNoMove =>
          true
        case CacheDirty =>
          false
      }
    }

    override def exploreEdge(edge: Int): Unit = {
      //ensureNeighborhoodAndRemove, we do it lazily because there might be nothing to do, actually
      // (although it is not quite sure that this actually useful at all)
      ensureNeighborhoodAndReInsert()

      nodeToInsertToNeighborhood(edge).
        getMove(globalObjective, correctedGlobalInit, acceptAllButMaxInt) match {
        case NoMoveFound => ;
        case MoveFound(move) =>
          val delta = move.objAfter - correctedGlobalInit
          val symbolicNodeToInsert = nodeIDToNode(edge)
          val symbolicNodeToRemove = nodeIDToNode(toNode)
          edgeBuilder.addEdge(symbolicNodeToInsert, symbolicNodeToRemove, delta, move, VLSNMoveType.InsertWithEject)
      }
    }

    override def pruneExplore(targetNbExplores: Int): Int = {
      val toReturn = super.pruneExplore(targetNbExplores)
      if (reInsert != null) {
        reInsert()
        reInsert = null
        nodeToInsertToNeighborhood = null
      }
      toReturn
    }
  }

  // /////////////////////////////////////////////////////////////////////////////////////////////////////////////

  class InsertNoEjectBundle(toVehicle: Int,
                            nodesToInsert: List[Int])
    extends EdgeToExploreBundle[Int](toVehicle, toVehicle, nodesToInsert) {
    //the id is the routing node that is moved

    override def toString: String = s"InsertNoEjectBundle(toVehicle:$toVehicle remaining edges:$size nodesToInsert:${nodesToInsert.toList.sorted.mkString(",")}"

    override def isEdgeDirty(edge: Int): Boolean = {
      isNodeDirty(edge) || isVehicleDirty(toVehicle)
    }

    private var nodeToInsertNeighborhood:Int => Neighborhood = null

    private def ensureNeighborhood():Unit = {
      if(nodeToInsertNeighborhood == null) {
        nodeToInsertNeighborhood = targetVehicleNodeToInsertNeighborhood(toVehicle)
      }
    }

    override def loadEdgeFromCache(edge: Int): Boolean = {
      cache.getInsertOnVehicleNoRemove(edge, toVehicle) match{
        case CachedAtomicMove(move:Move,delta:Long) =>
          edgeBuilder.addEdge(
            nodeIDToNode(edge),
            vehicleToNode(toVehicle),
            delta,
            move,
            VLSNMoveType.InsertNoEject)
          true
        case CachedAtomicNoMove =>
          true
        case CacheDirty =>
          false
      }
    }

    override def exploreEdge(edge: Int): Unit = {
      //ensureNeighborhoodAndRemove, we do it lazyly because there might be nothing to do, actually
      // (although it is not quite sure that this actually useful at all)
      ensureNeighborhood()

      require(!isNodeDirty(edge))
      require(!isVehicleDirty(toVehicle))

      nodeToInsertNeighborhood(edge).getMove(
        globalObjective,
        initialGlobalObjective,
        acceptanceCriterion = acceptAllButMaxInt) match {
        case NoMoveFound => ;
        case MoveFound(move) =>
          val delta = move.objAfter - initialGlobalObjective
          val symbolicNodeToInsert = nodeIDToNode(edge)

          val vlsnEdge = edgeBuilder.addEdge(
            symbolicNodeToInsert,
            vehicleToNode(toVehicle),
            delta,
            move,
            VLSNMoveType.InsertNoEject)

          //this prevents moves with same vehicle or node to be explored (would be faster to bypass VLSN & cycle search actually)
          if(delta < 0){
            isNodeDirty(edge) = true  //TODO: pas clair; cest pas juste edge???
            isVehicleDirty(toVehicle) = true
            newlyAddedPriorityCycles = List(vlsnEdge)  :: newlyAddedPriorityCycles
          }
      }
    }

    override def pruneExplore(targetNbExplores: Int): Int = {
      val toReturn = super.pruneExplore(targetNbExplores min maxInsertNoEjectPerVehicleAndPerIteration)
      nodeToInsertNeighborhood = null
      toReturn
    }
  }

  // ////////////////////////////////////////////////////////////////////////////////////////////

  private def exploreDeletions(): Unit = {
    for ((vehicleID, routingNodesToRemove) <- vehicleToRoutedNodes) {
      for (routingNodeToRemove <- routingNodesToRemove) {
        evaluateRemoveOnPenalty(routingNodeToRemove: Int, vehicleID) match {
          case null => ;
          case (move, delta) =>
            val symbolicNodeOfNodeToRemove = nodeIDToNode(routingNodeToRemove)
            edgeBuilder.addEdge(symbolicNodeOfNodeToRemove, trashNode, delta, move, VLSNMoveType.Remove)
        }
      }
    }
  }

  def evaluateRemoveOnPenalty(routingNodeToRemove:Int, fromVehicle:Int):(Move,Long) = {
    nodeToRemoveNeighborhood(routingNodeToRemove)
      .getMove(unroutedNodesPenalty, initialUnroutedNodesPenalty, acceptanceCriterion = (_,newObj) => newObj != Long.MaxValue) match{
      case NoMoveFound => null
      case MoveFound(move) =>
        val delta = move.objAfter - initialUnroutedNodesPenalty
        (move,delta)
    }
  }

  // ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def addNoMoveEdgesVehiclesToTrashNode(): Unit ={
    for(vehicleNode <- vehicleToNode if vehicleNode != null){
      edgeBuilder.addEdge(vehicleNode,trashNode,0L,null,VLSNMoveType.SymbolicVehicleToTrash)
    }
  }

  private def addTrashNodeToUnroutedNodes(): Unit ={
    for(unroutedNode <- unroutedNodesToInsert){
      edgeBuilder.addEdge(trashNode,nodeIDToNode(unroutedNode),0L,null,VLSNMoveType.SymbolicTrashToInsert)
    }
  }

  // ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  //no move edges from trashNode to each routed node wit no move,
  // but with delta equal to impact of removing the node from the route.
  private def exploreNodeEjection(): Unit = {
    for ((vehicleID, routingNodesToRemove) <- vehicleToRoutedNodes ) {
      for (routingNodeToRemove <- routingNodesToRemove) {
        evaluateRemoveOnSourceVehicle(routingNodeToRemove:Int,vehicleID) match{
          case null => ;
          case (move,delta) =>
            val symbolicNodeOfNodeToRemove = nodeIDToNode(routingNodeToRemove)
            val edge = edgeBuilder.addEdge(trashNode, symbolicNodeOfNodeToRemove, delta, null, VLSNMoveType.SymbolicTrashToNodeForEject)
            nodeToNodeRemoveEdge(symbolicNodeOfNodeToRemove.nodeID) = edge
        }
      }
    }
  }

  def evaluateRemoveOnSourceVehicle(routingNodeToRemove:Int,fromVehicle:Int):(Move, Long) = {
    nodeToRemoveNeighborhood(routingNodeToRemove)
      .getMove(vehicleToObjectives(fromVehicle),initialVehicleToObjectives(fromVehicle),
        acceptanceCriterion = (_,newObj) => newObj != Int.MaxValue) match{
      case NoMoveFound => null
      case MoveFound(move) =>
        val delta = move.objAfter - initialVehicleToObjectives(fromVehicle)
        (move,delta) //will negative if triangular inequality
    }
  }
}

