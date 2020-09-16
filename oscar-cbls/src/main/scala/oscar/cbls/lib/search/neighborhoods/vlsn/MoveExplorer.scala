/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cbls.lib.search.neighborhoods.vlsn

import oscar.cbls.core.computation.Store
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{Move, MoveFound, Neighborhood, NoMoveFound}

import scala.collection.immutable.{SortedMap, SortedSet}

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
                   debug:Boolean,
                   gradualEnrichmentSchemeN1V1N2V2P:(Int,Int,Int,Int) => Int) {


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



  val initialVehicleToObjectives = vehicleToObjectives.map(_.value)
  var initialUnroutedNodesPenalty = unroutedNodesPenalty.value
  var initialGlobalObjective = globalObjective.value

  //This is for debug purposes. Through this class we can check that other Obj have not moved
  // when exploring a given move from a givnen vehicel to a given other one.
  class CheckIngObjective(baseObjective:Objective, check:()=>Unit) extends Objective{
    override def detailedString(short: Boolean, indent: Long): String =
      baseObjective.detailedString(short: Boolean, indent: Long)

    override def model: Store = baseObjective.model

    override def value: Long = {
      check()
      baseObjective.value
    }
  }

  def generateCheckerObjForVehicles(evaluatedObj:Objective, changedVehicles:Set[Int], penaltyChanged:Boolean):Objective = {
    new CheckIngObjective(evaluatedObj, () => {
      if (!penaltyChanged){
        val newValue = unroutedNodesPenalty.value
        require(newValue == Long.MaxValue || newValue == initialUnroutedNodesPenalty,
          s"Penalty impacted by current move and should not, can only impact ${changedVehicles.mkString(")")}")
      }
      for (vehicle <- 0 until v){
        if(!(changedVehicles contains vehicle)) {
          val newValue = vehicleToObjectives(vehicle).value
          require(newValue == Long.MaxValue || newValue == initialVehicleToObjectives(vehicle),
            s"vehicle $vehicle impacted by current move and should not; it can only impact {${changedVehicles.mkString(",")}}${if (penaltyChanged) " and penalty " else ""}")
        }
      }
      


      val global = globalObjective.value
      if(global != Long.MaxValue){
        require(global == vehicleToObjectives.map(_.value).sum + unroutedNodesPenalty.value, "global objective not coherent with sum of partial objectives")
      }
    })
  }

  //TODO: find best loop nesting WRT. checkpoint calculation.
  //maybe we should unroute all nodes before doing move exploration since we do not want to waste time on evaluation obj on non targeted vehicle?
  val nodesToMove: Iterable[Int] = vehicleToRoutedNodes.flatMap(_._2)

  require(nodesToMove.isEmpty || nodesToMove.min >=0, "VLSN cannot handle nodes with negative ID's; got" + nodesToMove.min)
  // /////////////////////////////////////////////////////////////
  //building the nodes

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

  val nbNodesInVLSNGraph = nodes.length
  def nbEdgesInGraph:Int = edgeBuilder.nbEdges

  // /////////////////////////////////////////////////////////////
  //the partitioning data

  var partitionLevelDone:Int = -2
  var currentPartitionLevel:Int = 0

  val vehicleIsDirty:Array[Boolean] = Array.fill(v)(false)

  val nodeIsDirty:Array[Boolean] = Array.fill((nodesToMove ++ unroutedNodesToInsert).max+1)(false)

  def isMoveToExplore(fromNode:Int, fromVehicle:Int, toVehicle:Int, toNode:Int = -1):Boolean = {
    if(nodeIsDirty(fromNode)
      || (fromVehicle != -1 && vehicleIsDirty(fromVehicle))
      || (toVehicle != -1 && vehicleIsDirty(toVehicle))
      || (toNode!= -1 && nodeIsDirty(toNode))) return false

    val partitionLevel = gradualEnrichmentSchemeN1V1N2V2P(fromNode, fromVehicle, toNode, toVehicle)

    partitionLevelDone < partitionLevel && partitionLevel <= currentPartitionLevel
  }

  def isInsertToExplore(unroutedNode:Int, toVehicle:Int, removedNode:Int = -1):Boolean = {
    if(nodeIsDirty(unroutedNode)
      || vehicleIsDirty(toVehicle)
      || (removedNode!= -1 && nodeIsDirty(removedNode))) return false

    val partitionLevel = gradualEnrichmentSchemeN1V1N2V2P(unroutedNode, -1, removedNode, toVehicle)
    partitionLevelDone < partitionLevel && partitionLevel <= currentPartitionLevel
  }


  //on peut simler le direct insert via les martiyions.
  // donc on supprimer le diret insert.
  //de tt façons c'est un mécanisme à désactiver.

  //par contre, on a le choix quand on trouve un cycle, on continue à construire le modèle avec ce qu ireste?
  //oui, mais du coup il faut avoir une notion de dirtyNode et dirtyVehicle ici.

  addNoMoveEdgesVehiclesToTrashNode()
  addTrashNodeToUnroutedNodes()
  exploreEjections()

  def injectAllCache(verbose:Boolean): Unit = {}

  var nbExploredMoves = 0
  var nbExploredEdges = 0

  // /////////////////////////////////////////////////////////////
  def enrichGraph(partitioningLevel:Int, dirtyNodes:Set[Int],dirtyVehicles:Set[Int],verbose:Boolean): VLSNGraph = {

    for(node <- dirtyNodes) nodeIsDirty(node) = true
    for(vehicle <- dirtyVehicles) {
      vehicleIsDirty(vehicle) = true
      initialVehicleToObjectives(vehicle) = vehicleToObjectives(vehicle).value
    }

    initialUnroutedNodesPenalty = unroutedNodesPenalty.value
    initialGlobalObjective = globalObjective.value

    require(partitioningLevel > partitionLevelDone, s"partitioningLevel:$partitioningLevel partitionLevelDone:$partitionLevelDone")
    currentPartitionLevel = partitioningLevel

    exploreInsertions()
    exploreNodeMove()
    exploreDeletions() //should be called after insertions

    //these ones are done once and forall at init time of this class
    //exploreEjections() // about moving one node away from a vehicle, without associated insert or move
    //addNoMoveEdgesVehiclesToTrashNode()
    //addTrashNodeToUnroutedNodes()

    if(verbose) println(s"            nbExploredMoves:$nbExploredMoves nbExploredEdges:$nbExploredEdges")

    partitionLevelDone = currentPartitionLevel
    //println("direct inserts:" + directInsertsNodeVehicle)
    edgeBuilder.buildGraph()
  }

  val maxLong = Long.MaxValue
  val acceptAllButMaxInt: (Long, Long) => Boolean = (_, newObj: Long) => newObj != maxLong

  // ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def exploreInsertions(): Unit ={

    val vehicleAndUnroutedNodes: Iterable[(Int, Int)] =
      unroutedNodesToInsert.flatMap((unroutedNode:Int) =>
        if(nodeIsDirty(unroutedNode)) None
        else nodeToRelevantVehicles(unroutedNode).flatMap((vehicle:Int) =>
          if(vehicleIsDirty(vehicle)) None
          else Some((vehicle, unroutedNode))))

    val vehicleToUnroutedNodeToInsert = vehicleAndUnroutedNodes
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2))
      .toMap

    exploreInsertionsNoRemove(vehicleToUnroutedNodeToInsert)
    exploreInsertionsWithRemove(vehicleToUnroutedNodeToInsert)
  }

  private def exploreInsertionsNoRemove(vehicleToUnroutedNodeToInsert: Map[Int, Iterable[Int]]): Unit = {

    //println("vehicleToUnroutedNodeToInsert:" + vehicleToUnroutedNodeToInsert.toList.map({case (ve,nod) => s"sehicle:$ve ${nod.mkString(",")}"}).mkString("\n"))
    for ((targetVehicleForInsertion, unroutedNodesToInsert) <- vehicleToUnroutedNodeToInsert) {

      //en fait, on devrait autoriser un certain nombre de directInsert: chaque véhicule peut avoir v directInserts.

      var allowedDirectInserts = v

      //try inserts without removes
      for (unroutedNodeToInsert <- unroutedNodesToInsert
           if isInsertToExplore(unroutedNode = unroutedNodeToInsert, toVehicle = targetVehicleForInsertion)){
        //insertion without remove

        if (allowedDirectInserts >0) {
          nbExploredEdges += 1
          evaluateInsertOnVehicleNoRemove(
            unroutedNodeToInsert: Int,
            targetVehicleForInsertion: Int,
            cached = true) match {
            case null => ;
            case (move, delta) =>
              val symbolicNodeToInsert = nodeIDToNode(unroutedNodeToInsert)

              require(!vehicleIsDirty(targetVehicleForInsertion))

              val edge = edgeBuilder.addEdge(symbolicNodeToInsert, vehicleToNode(targetVehicleForInsertion), delta, move, VLSNMoveType.InsertNoEject)
              if (delta < 0L) {
                //there is a direct insert
                allowedDirectInserts -= 1
                //println("set direct insert flag vehicle:" + targetVehicleForInsertion + "inserted node:" + unroutedNodeToInsert)
              }
          }
        }
      }
    }
  }


  private var cachedInsertNeighborhoodNoRemove: Option[(Int, Int => Neighborhood)] = None //targetVehicle, node => neighborhood

  @inline
  def evaluateInsertOnVehicleNoRemove(unroutedNodeToInsert: Int,
                                      targetVehicleForInsertion: Int,
                                      cached:Boolean): (Move, Long) = {

    require(!vehicleIsDirty(targetVehicleForInsertion))

    nbExploredMoves += 1
    val nodeToInsertNeighborhood = cachedInsertNeighborhoodNoRemove match {
      case Some((cachedTarget, cachedNeighborhood)) if cachedTarget == targetVehicleForInsertion && cached =>
        cachedNeighborhood
      case _ =>
        val n = targetVehicleNodeToInsertNeighborhood(targetVehicleForInsertion)
        cachedInsertNeighborhoodNoRemove = Some((targetVehicleForInsertion, n))
        n
    }

    val obj = if (debug) {
      generateCheckerObjForVehicles(globalObjective:Objective, Set(targetVehicleForInsertion), penaltyChanged = true)
    }else {
      globalObjective
    }

    val proc = nodeToInsertNeighborhood(unroutedNodeToInsert)

    proc.getMove(obj, initialGlobalObjective, acceptanceCriterion = acceptAllButMaxInt) match {
      case NoMoveFound => null
      case MoveFound(move) =>
        val delta = move.objAfter - initialGlobalObjective
        (move, delta)
    }
  }

  // ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  private def exploreInsertionsWithRemove(vehicleToUnroutedNodeToInsert: Map[Int, Iterable[Int]]): Unit = {

    for ((targetVehicleForInsertion, unroutedNodesToInsert) <- vehicleToUnroutedNodeToInsert){

      //insertion with remove, we remove, and then insert
      //insertion with remove

      for (routingNodeToRemove <- vehicleToRoutedNodes(targetVehicleForInsertion)) {
        val symbolicNodeToRemove = nodeIDToNode(routingNodeToRemove)

        //performing the remove
        val reInsert = removeAndReInsert(routingNodeToRemove)

        val unroutedObjAfterRemove = unroutedNodesPenalty.value
        val correctedGlobalInit = initialGlobalObjective - initialUnroutedNodesPenalty + unroutedObjAfterRemove

        for (unroutedNodeToInsert <- unroutedNodesToInsert
             if (!nodeIsDirty(unroutedNodeToInsert)) && isInsertToExplore(
               unroutedNode = unroutedNodeToInsert,
               toVehicle = targetVehicleForInsertion,
               removedNode = routingNodeToRemove)) {

          //Evaluating the delta
          nbExploredEdges += 1
          evaluateInsertOnVehicleWithRemove(
            unroutedNodeToInsert: Int,
            targetVehicleForInsertion: Int,
            routingNodeToRemove: Int,
            correctedGlobalInit: Long,
            cached = true) match {
            case null => ;
            case (move, delta) =>
              val symbolicNodeToInsert = nodeIDToNode(unroutedNodeToInsert)
              edgeBuilder.addEdge(symbolicNodeToInsert, symbolicNodeToRemove, delta, move, VLSNMoveType.InsertWithEject)
          }
        }
        //re-inserting
        reInsert()
      }
    }
  }

  private var cachedInsertNeighborhoodWithRemove: Option[(Int, Int, Int => Neighborhood)] = None //target,removed,toInsert=>Neighborhood

  @inline
  def evaluateInsertOnVehicleWithRemove(unroutedNodeToInsert: Int,
                                        targetVehicleForInsertion: Int,
                                        removedNode: Int,
                                        correctedGlobalInit: Long,
                                        cached:Boolean): (Move, Long) = {
    require(!vehicleIsDirty(targetVehicleForInsertion))

    nbExploredMoves += 1

    val nodeToInsertToNeighborhood = cachedInsertNeighborhoodWithRemove match {
      case Some((cachedTarget, cachedRemoved, cachedNeighborhood))
        if cached && cachedTarget == targetVehicleForInsertion && cachedRemoved == removedNode =>
        cachedNeighborhood
      case _ =>
        val n = targetVehicleNodeToInsertNeighborhood(targetVehicleForInsertion)
        cachedInsertNeighborhoodWithRemove = Some((targetVehicleForInsertion, removedNode, n))
        n
    }

    val obj = if(debug) {
      generateCheckerObjForVehicles(globalObjective, Set(targetVehicleForInsertion), penaltyChanged = true)
    }else {
      globalObjective
    }

    nodeToInsertToNeighborhood(unroutedNodeToInsert).
      getMove(obj, correctedGlobalInit, acceptanceCriterion = acceptAllButMaxInt) match {
      case NoMoveFound => null
      case MoveFound(move) =>
        val delta = move.objAfter - correctedGlobalInit
        (move, delta)
    }
  }

  // ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private def exploreNodeMove(): Unit = {
    val vehicleAndNodeToMove:Iterable[(Int,Int)] =
      nodesToMove.flatMap(nodeToMove =>
        if(nodeIsDirty(nodeToMove)) None
        else nodeToRelevantVehicles(nodeToMove).flatMap(vehicle =>
          if(vehicleIsDirty(vehicle)) None
          else Some((vehicle,nodeToMove))))

    val vehicleToNodeToMoveThere = vehicleAndNodeToMove
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2))
      .toMap

    exploreNodeMoveNoRemove(vehicleToNodeToMoveThere)
    exploreNodeMoveWithRemove(vehicleToNodeToMoveThere)
  }

  private def exploreNodeMoveNoRemove(vehicleToNodeToMoveThere:Map[Int,Iterable[Int]]): Unit = {

    for ((targetVehicleID, routedNodesToMoveThere) <- vehicleToNodeToMoveThere) {
      val symbolicNodeOfVehicle = vehicleToNode(targetVehicleID)

      //moves without removes
      for (routingNodeToMove <- routedNodesToMoveThere) {
        val symbolicNodeOfNodeToMove = nodeIDToNode(routingNodeToMove)
        val fromVehicle = symbolicNodeOfNodeToMove.vehicle


        if (fromVehicle != targetVehicleID
          && isMoveToExplore(
          fromNode = routingNodeToMove,
          fromVehicle = fromVehicle,
          toVehicle = targetVehicleID)) {  //that's the target vehicle

          nbExploredEdges += 1

          //move without remove
          //     :(Int,Int) => Neighborhood,
          evaluateMoveToVehicleNoRemove(routingNodeToMove: Int, fromVehicle, targetVehicleID: Int, cached = true) match {
            case null => //println("No Accepted Move")
              ;
            case (move, delta) =>
              edgeBuilder.addEdge(symbolicNodeOfNodeToMove, symbolicNodeOfVehicle, delta, move, VLSNMoveType.MoveNoEject)
              // println(symbolicNodeOfNodeToMove.incoming.mkString("\n"))
              // println(s"$move - deltaObj $delta")
            //we cannot consider directMoves here moves because we should also take the impact on the first vehicle into account,
            // and this is not captured into the objective function
          }
        }
        else {
          //println("Not to explore move")
        }
      }
    }
  }

  private var cachedNodeMoveNeighborhoodNoRemove:Option[(Int,Int => Neighborhood)] = None //targetVehicle,node=>Neighborhood


  def evaluateMoveToVehicleNoRemove(routingNodeToMove: Int, fromVehicle: Int, targetVehicleForInsertion: Int, cached:Boolean): (Move, Long) = {

    require(!vehicleIsDirty(fromVehicle))
    require(!vehicleIsDirty(targetVehicleForInsertion))

    nbExploredMoves += 1

    val nodeToMoveToNeighborhood = cachedNodeMoveNeighborhoodNoRemove match {
      case Some((cachedTarget, cachedNeighborhood)) if cachedTarget == targetVehicleForInsertion && cached =>
        cachedNeighborhood
      case _ =>
        val n = targetVehicleNodeToMoveNeighborhood(targetVehicleForInsertion)
        cachedNodeMoveNeighborhoodNoRemove = Some((targetVehicleForInsertion, n))
        n
    }

    val obj = if(debug) {
      generateCheckerObjForVehicles(vehicleToObjectives(targetVehicleForInsertion), Set(fromVehicle, targetVehicleForInsertion), penaltyChanged = false)
    }else {
      vehicleToObjectives(targetVehicleForInsertion)
    }

    val neighborhood = nodeToMoveToNeighborhood(routingNodeToMove)
    //    neighborhood.verbose = 5
    neighborhood.getMove(
      obj,
      initialVehicleToObjectives(targetVehicleForInsertion),
      acceptanceCriterion = acceptAllButMaxInt) match {
      case NoMoveFound => null
      case MoveFound(move) =>
        val delta = move.objAfter - initialVehicleToObjectives(targetVehicleForInsertion)
        (move, delta)
    }
  }

  private def exploreNodeMoveWithRemove(vehicleToNodeToMoveThere:Map[Int,Iterable[Int]]): Unit = {
    for((targetVehicleID,routedNodesToMoveThere) <- vehicleToNodeToMoveThere) {

      //moves with removes
      for(nodeIDToEject <- vehicleToRoutedNodes(targetVehicleID)){
        val symbolicNodeToEject = nodeIDToNode(nodeIDToEject)

        //performing the remove
        val reInsert = removeAndReInsert(nodeIDToEject)

        //Evaluating all moves on this remove
        for(routingNodeToMove <- routedNodesToMoveThere) {
          val symbolicNodeOfNodeToMove = nodeIDToNode(routingNodeToMove)
          val fromVehicle =  symbolicNodeOfNodeToMove.vehicle

          if (symbolicNodeOfNodeToMove.vehicle != targetVehicleID &&
            isMoveToExplore(
              fromNode = routingNodeToMove,
              fromVehicle = fromVehicle,
              toVehicle = targetVehicleID,
              toNode = nodeIDToEject)) {

            nbExploredEdges += 1

            evaluateMoveToVehicleWithRemove(routingNodeToMove, fromVehicle, targetVehicleID, nodeIDToEject, true) match{
              case null => //println("No Accepted Move");
              case (move,delta) =>
                edgeBuilder.addEdge(symbolicNodeOfNodeToMove, symbolicNodeToEject, delta, move, VLSNMoveType.MoveWithEject)
                // println(symbolicNodeOfNodeToMove.incoming.mkString("\n"))
                // println(s"$move $delta")
            }
          }
        }

        //re-inserting
        reInsert()

      }
    }
  }

  private var cachedNodeMoveNeighborhoodWithRemove:Option[(Int,Int,Int => Neighborhood)] = None //targetVehicle,removedNode,node=>Neighborhood


  def evaluateMoveToVehicleWithRemove(routingNodeToMove:Int, fromVehicle:Int, targetVehicleForInsertion:Int, removedNode:Int, cached:Boolean):(Move, Long) = {

    require(!vehicleIsDirty(fromVehicle))
    require(!vehicleIsDirty(targetVehicleForInsertion))

    nbExploredMoves += 1

    val nodeToMoveToNeighborhood = cachedNodeMoveNeighborhoodWithRemove match {
      case Some((cachedTarget, cachedRemoved,cachedNeighborhood))
        if cached && cachedTarget == targetVehicleForInsertion && cachedRemoved == removedNode =>
        cachedNeighborhood
      case _ =>
        val n = targetVehicleNodeToMoveNeighborhood(targetVehicleForInsertion)
        cachedNodeMoveNeighborhoodWithRemove = Some((targetVehicleForInsertion, removedNode, n))
        n
    }

    val obj = if(debug) {
      generateCheckerObjForVehicles(vehicleToObjectives(targetVehicleForInsertion), Set(fromVehicle, targetVehicleForInsertion), penaltyChanged = true) //because node is temporarily removed
    }else {
      vehicleToObjectives(targetVehicleForInsertion)
    }

    nodeToMoveToNeighborhood(routingNodeToMove)
      .getMove(obj, initialVehicleToObjectives(targetVehicleForInsertion), acceptanceCriterion = acceptAllButMaxInt) match {
      case NoMoveFound => null
      case MoveFound(move) =>
        val delta = move.objAfter - initialVehicleToObjectives(targetVehicleForInsertion)
        (move,delta)
    }
  }

  // ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
   * deletions are from deleted node to trashNode
   */
  private def exploreDeletions(): Unit = {
    for ((vehicleID, routingNodesToRemove) <- vehicleToRoutedNodes if !vehicleIsDirty(vehicleID)) {
      for (routingNodeToRemove <- routingNodesToRemove if !nodeIsDirty(routingNodeToRemove)) {
        if (isMoveToExplore(fromVehicle = vehicleID, fromNode = routingNodeToRemove, toVehicle = -1)) {
          nbExploredEdges += 1
          evaluateRemove(routingNodeToRemove: Int, vehicleID) match {
            case null => ;
            case (move, delta) =>
              val symbolicNodeOfNodeToRemove = nodeIDToNode(routingNodeToRemove)
              edgeBuilder.addEdge(symbolicNodeOfNodeToRemove, trashNode, delta, move, VLSNMoveType.Remove)
          }
        }
      }
    }
  }

  def evaluateRemove(routingNodeToRemove:Int,fromVehicle:Int):(Move,Long) = {

    require(!vehicleIsDirty(fromVehicle))
    nbExploredMoves += 1

    val obj = if(debug) {
      generateCheckerObjForVehicles(unroutedNodesPenalty, Set(fromVehicle), penaltyChanged = true)
    }else {
      unroutedNodesPenalty
    }

    nodeToRemoveNeighborhood(routingNodeToRemove)
      .getMove(obj, initialUnroutedNodesPenalty, acceptanceCriterion = (_,newObj) => newObj != Long.MaxValue) match{
      case NoMoveFound => null
      case MoveFound(move) =>
        val delta = move.objAfter - initialUnroutedNodesPenalty
        (move,delta)
    }
  }

  // ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  //should be called after all edges going to vehicle are generated
  private def addNoMoveEdgesVehiclesToTrashNode(): Unit ={
    for(vehicleNode <- vehicleToNode if vehicleNode != null && !vehicleIsDirty(vehicleNode.vehicle)){
      edgeBuilder.addEdge(vehicleNode,trashNode,0L,null,VLSNMoveType.SymbolicVehicleToTrash)
    }
  }

  // ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private def addTrashNodeToUnroutedNodes(): Unit ={
    for(unroutedNode <- unroutedNodesToInsert if !nodeIsDirty(unroutedNode)){
      edgeBuilder.addEdge(trashNode,nodeIDToNode(unroutedNode),0L,null,VLSNMoveType.SymbolicTrashToInsert)
    }
  }

  // ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  //no move edges from trashNode to each routed node wit no move,
  // but with delta equal to impact of removing the node from the route.
  private def exploreEjections(): Unit = {
    for ((vehicleID, routingNodesToRemove) <- vehicleToRoutedNodes if !vehicleIsDirty(vehicleID)) {
      for (routingNodeToRemove <- routingNodesToRemove if !nodeIsDirty(routingNodeToRemove)) {
        nbExploredEdges += 1
        evaluateRemoveOnSourceVehicle(routingNodeToRemove:Int,vehicleID) match{
          case null => ;
          case (move,delta) =>
            val symbolicNodeOfNodeToRemove = nodeIDToNode(routingNodeToRemove)
            edgeBuilder.addEdge(trashNode, symbolicNodeOfNodeToRemove, delta, null, VLSNMoveType.SymbolicTrashToNodeForEject)
        }
      }
    }
  }

  def evaluateRemoveOnSourceVehicle(routingNodeToRemove:Int,fromVehicle:Int):(Move, Long) = {

    require(!vehicleIsDirty(fromVehicle))
    nbExploredMoves += 1

    nodeToRemoveNeighborhood(routingNodeToRemove)
      .getMove(vehicleToObjectives(fromVehicle),initialVehicleToObjectives(fromVehicle),
        acceptanceCriterion = (_,newObj) => newObj != Int.MaxValue) match{
      case NoMoveFound => null
      case MoveFound(move) =>
        val delta = move.objAfter - initialVehicleToObjectives(fromVehicle)
        (move,delta) //will very likely always be negative because of triangular inequality
    }
  }
}
