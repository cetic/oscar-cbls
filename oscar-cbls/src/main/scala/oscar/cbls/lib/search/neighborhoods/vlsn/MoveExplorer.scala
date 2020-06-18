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

class MoveExplorerAlgo(v:Int,
                       vehicleToRoutedNodes:SortedMap[Int,Iterable[Int]],
                       unroutedNodesToInsert:Iterable[Int],
                       nodeToRelevantVehicles:Map[Int,Iterable[Int]],

                       targetVehicleNodeToInsertNeighborhood:Int => Int => Neighborhood,
                       targetVehicleNodeToMoveNeighborhood:Int => Int => Neighborhood,
                       nodeToRemoveNeighborhood:Int => Neighborhood,

                       removeAndReInsert:Int => () => Unit,
                       useDirectInsert:Boolean,

                       vehicleToObjectives:Array[Objective],
                       unroutedNodesPenalty:Objective,
                       globalObjective:Objective,
                       debug:Boolean) {


  val initialVehicleToObjectives = vehicleToObjectives.map(_.value)
  val initialUnroutedNodesPenalty = unroutedNodesPenalty.value
  val initialGlobalObjective = globalObjective.value



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

  var nodes: Array[Node] = null
  var nodeIDToNode: SortedMap[Int, Node] = null
  var relevantVehicles: SortedSet[Int] = null
  var edgeBuilder: VLSNEdgeBuilder = null
  var trashNode: Node = null
  val vehicleToNode: Array[Node] = Array.fill(v)(null)

  var nbLabels: Int = -2

  def buildGraph(): (VLSNGraph,List[Edge]) = {

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
    // For each uinrouted node: a diffeent label
    // a different label for the trashNode

    buildNodes()
    edgeBuilder = new VLSNEdgeBuilder(nodes, nbLabels, v) //the labels are the vehicles
    exploreInsertions()
    exploreNodeMove()
    exploreDeletions() //should be called after insertions
    addNoMoveEdgesVehiclesToTrashNode()
    addTrashNodeToUnroutedNodes()
    exploreEjections() // about moving one node away from a vehicle, without associated insert or move

    //println("direct inserts:" + directInsertsNodeVehicle)
    (edgeBuilder.finish(),directEdges)
  }

  private def buildNodes(): Unit = {
    //label of nodes are:
    // for each routed node and vehicle node: the vehicle of the node
    // For each unrouted node: a different label
    // a different label for the trashNode

    //as labels, we take the vehicles, plus one label per non-routed node
    val builder = new VLSNNodeBuilder(nbLabels = v)
    nodeIDToNode = SortedMap.empty

    //noeuds symboliques pour les véhicules
    relevantVehicles = SortedSet.empty[Int] ++ nodeToRelevantVehicles.flatMap(_._2)

    for (vehicle <- relevantVehicles) {
      val node = builder.addNode(-vehicle, vehicle, vehicle, VLSNSNodeType.VehicleNode)
      vehicleToNode(vehicle) = node
      nodeIDToNode += ((-vehicle, node))
    }

    //noeud cible pour l'unroutage, label is v
    trashNode = builder.addNode(-1, -1, builder.newFreshLabel(), VLSNSNodeType.FictiveNode)

    //noeuds pour les noeud à déplacer
    for ((vehicle, routedNodesOnVehicle) <- vehicleToRoutedNodes) {
      require(vehicle < v)
      for (nodeID <- routedNodesOnVehicle) {
        //require(nodeID >= v, "cannot put vehicle to move :" + nodeID)
        nodeIDToNode += ((nodeID, builder.addNode(nodeID, vehicle, vehicle, VLSNSNodeType.RegularNode)))
      }
    }

    //noeuds non routés
    for (unroutedNode <- unroutedNodesToInsert) {
      //TODO: check that label and vehicles are fine...
      nodeIDToNode += ((unroutedNode, builder.addNode(unroutedNode, v, builder.newFreshLabel(), VLSNSNodeType.UnroutedNode)))
    }

    val x = builder.finish()
    nodes = x._1
    nbLabels = x._2
  }

  val maxLong = Long.MaxValue
  val acceptAllButMaxInt: (Long, Long) => Boolean = (_, newObj: Long) => newObj != maxLong

  private var cachedInsertNeighborhoodNoRemove: Option[(Int, Int => Neighborhood)] = None //targetVehicle, node => neighborhood

  @inline
  def evaluateInsertOnVehicleNoRemove(unroutedNodeToInsert: Int,
                                      targetVehicleForInsertion: Int,
                                      cached:Boolean): (Move, Long) = {

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
//    proc.verbose = 4

    proc.getMove(obj, initialGlobalObjective, acceptanceCriterion = acceptAllButMaxInt) match {
      case NoMoveFound => null
      case MoveFound(move) =>
        val delta = move.objAfter - initialGlobalObjective
        (move, delta)
    }
  }

  private var cachedInsertNeighborhoodWithRemove: Option[(Int, Int, Int => Neighborhood)] = None //target,removed,toInsert=>Neighborhood

  @inline
  def evaluateInsertOnVehicleWithRemove(unroutedNodeToInsert: Int,
                                        targetVehicleForInsertion: Int,
                                        removedNode: Int,
                                        correctedGlobalInit: Long,
                                        cached:Boolean): (Move, Long) = {

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

  private var nodesWithDirectInserts: SortedSet[Int] = SortedSet.empty
  private var vehiclesWithDirectInsertsOrMoves: SortedSet[Int] = SortedSet.empty

  private var directEdges:List[Edge] = List.empty

  private def registerDirectInsert(node: Int, vehicle: Int, edge:Edge): Unit = {
    if (useDirectInsert) {
      nodesWithDirectInserts += node
      vehiclesWithDirectInsertsOrMoves += vehicle
      directEdges = edge :: directEdges
      //println("found direct insert:" + edge + edge.move.shortString)
    }
  }

  private def vehicleHasDirectInsertOrMove(vehicle: Int): Boolean = vehiclesWithDirectInsertsOrMoves contains vehicle

  private def nodeHasDirectInsertOrMove(node: Int): Boolean = nodesWithDirectInserts contains node

  private def exploreInsertionsNoRemove(vehicleToUnroutedNodeToInsert: Map[Int, Iterable[Int]]): Unit = {

    for ((targetVehicleForInsertion, unroutedNodesToInsert) <- vehicleToUnroutedNodeToInsert) {

      var currentVehicleHasDirectInsert: Boolean = false

      //try inserts without removes
      for (unroutedNodeToInsert <- unroutedNodesToInsert if !nodeHasDirectInsertOrMove(unroutedNodeToInsert)) {
        //insertion without remove

        if (!currentVehicleHasDirectInsert) {
          evaluateInsertOnVehicleNoRemove(
            unroutedNodeToInsert: Int,
            targetVehicleForInsertion: Int,
            true) match {
            case null => ;
            case (move, delta) =>
              val symbolicNodeToInsert = nodeIDToNode(unroutedNodeToInsert)
              val edge = edgeBuilder.addEdge(symbolicNodeToInsert, vehicleToNode(targetVehicleForInsertion), delta, move, VLSNMoveType.InsertNoEject)
              if (delta < 0L) {
                //there is a direct insert
                registerDirectInsert(unroutedNodeToInsert, targetVehicleForInsertion,edge)
                currentVehicleHasDirectInsert = true
              }
          }
        }
      }
    }
  }

  private def exploreInsertionsWithRemove(vehicleToUnroutedNodeToInsert: Map[Int, Iterable[Int]]): Unit = {

    for ((targetVehicleForInsertion, unroutedNodesToInsert) <- vehicleToUnroutedNodeToInsert
         if !vehicleHasDirectInsertOrMove(targetVehicleForInsertion)) {

      //insertion with remove, we remove, and then insert
      //insertion with remove
      for (routingNodeToRemove <- vehicleToRoutedNodes(targetVehicleForInsertion)) {
        val symbolicNodeToRemove = nodeIDToNode(routingNodeToRemove)

        //performing the remove
        val reInsert = removeAndReInsert(routingNodeToRemove)

        val unroutedObjAfterRemove = unroutedNodesPenalty.value
        val correctedGlobalInit = initialGlobalObjective - initialUnroutedNodesPenalty + unroutedObjAfterRemove

        for (unroutedNodeToInsert <- unroutedNodesToInsert
             if !nodeHasDirectInsertOrMove(unroutedNodeToInsert)) {

          //Evaluating the delta
          evaluateInsertOnVehicleWithRemove(
            unroutedNodeToInsert: Int,
            targetVehicleForInsertion: Int,
            routingNodeToRemove: Int,
            correctedGlobalInit: Long,
            true) match {
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

  private def exploreInsertions(): Unit ={

    val vehicleAndUnroutedNodes: Iterable[(Int, Int)] =
      unroutedNodesToInsert.flatMap(unroutedNode => nodeToRelevantVehicles(unroutedNode).map(vehicle => (vehicle, unroutedNode)))

    val vehicleToUnroutedNodeToInsert = vehicleAndUnroutedNodes.groupBy(_._1).mapValues(_.map(_._2))

    exploreInsertionsNoRemove(vehicleToUnroutedNodeToInsert)
    exploreInsertionsWithRemove(vehicleToUnroutedNodeToInsert)
  }

  private var cachedNodeMoveNeighborhoodNoRemove:Option[(Int,Int => Neighborhood)] = None //targetVehicle,node=>Neighborhood

  def evaluateMoveToVehicleNoRemove(routingNodeToMove: Int, fromVehicle: Int, targetVehicleForInsertion: Int, cached:Boolean): (Move, Long) = {

    val nodeToMoveToNeighborhood = cachedNodeMoveNeighborhoodNoRemove match {
      case Some((cachedTarget, cachedNeighborhood)) if cachedTarget == targetVehicleForInsertion && cached =>
        cachedNeighborhood
      case _ =>
        val n = targetVehicleNodeToMoveNeighborhood(targetVehicleForInsertion)
        cachedNodeMoveNeighborhoodNoRemove = Some((targetVehicleForInsertion, n))
        n
    }

    val obj = if(debug) {
      generateCheckerObjForVehicles(globalObjective, Set(fromVehicle, targetVehicleForInsertion), penaltyChanged = false)
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

  private var cachedNodeMoveNeighborhoodWithRemove:Option[(Int,Int,Int => Neighborhood)] = None //targetVehicle,removedNode,node=>Neighborhood

  def evaluateMoveToVehicleWithRemove(routingNodeToMove:Int, fromVehicle:Int, targetVehicleForInsertion:Int, removedNode:Int, cached:Boolean):(Move, Long) = {

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
      generateCheckerObjForVehicles(globalObjective, Set(fromVehicle, targetVehicleForInsertion), penaltyChanged = true) //because node is temporarily removed
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

  private def exploreNodeMoveNoRemove(vehicleToNodeToMoveThere:Map[Int,Iterable[Int]]): Unit = {

    for ((targetVehicleID, routedNodesToMoveThere) <- vehicleToNodeToMoveThere if !vehicleHasDirectInsertOrMove(targetVehicleID)) {
      val symbolicNodeOfVehicle = vehicleToNode(targetVehicleID)

      //moves without removes
      for (routingNodeToMove <- routedNodesToMoveThere) {
        val symbolicNodeOfNodeToMove = nodeIDToNode(routingNodeToMove)
        val fromVehicle = symbolicNodeOfNodeToMove.vehicle

        if (fromVehicle != targetVehicleID) {  //that's the target vehicle

          //move without remove
          //     :(Int,Int) => Neighborhood,
          evaluateMoveToVehicleNoRemove(routingNodeToMove: Int, fromVehicle, targetVehicleID: Int, true) match {
            case null => ;
            case (move, delta) =>
              edgeBuilder.addEdge(symbolicNodeOfNodeToMove, symbolicNodeOfVehicle, delta, move, VLSNMoveType.MoveNoEject)
            //we cannot consider directMoves here moves because we should also take the impact on the first vehicle into account,
            // and this is not captured into the objective function
          }
        }
      }
    }
  }

  private def exploreNodeMoveWithRemove(vehicleToNodeToMoveThere:Map[Int,Iterable[Int]]): Unit = {
    for((targetVehicleID,routedNodesToMoveThere) <- vehicleToNodeToMoveThere if !vehicleHasDirectInsertOrMove(targetVehicleID)) {

      //moves with removes
      for(nodeIDToEject <- vehicleToRoutedNodes(targetVehicleID)){
        val symbolicNodeToEject = nodeIDToNode(nodeIDToEject)

        //performing the remove
        val reInsert = removeAndReInsert(nodeIDToEject)

        for(routingNodeToMove <- routedNodesToMoveThere) {
          val symbolicNodeOfNodeToMove = nodeIDToNode(routingNodeToMove)
          val fromVehicle =  symbolicNodeOfNodeToMove.vehicle

          if (symbolicNodeOfNodeToMove.vehicle != targetVehicleID) {
            //Evaluating all moves on this remove
            evaluateMoveToVehicleWithRemove(routingNodeToMove, fromVehicle, targetVehicleID, nodeIDToEject, true) match{
              case null => ;
              case (move,delta) =>
                edgeBuilder.addEdge(symbolicNodeOfNodeToMove, symbolicNodeToEject, delta, move, VLSNMoveType.MoveWithEject)
            }
          }
        }
        //re-inserting
        reInsert()
      }
    }
  }
  private def exploreNodeMove(): Unit = {
    val vehicleAndNodeToMove:Iterable[(Int,Int)] =
      nodesToMove.flatMap(nodeToMove => nodeToRelevantVehicles(nodeToMove).map(vehicle => (vehicle,nodeToMove)))

    val vehicleToNodeToMoveThere = vehicleAndNodeToMove.groupBy(_._1).mapValues(_.map(_._2))

    exploreNodeMoveNoRemove(vehicleToNodeToMoveThere)
    exploreNodeMoveWithRemove(vehicleToNodeToMoveThere)
  }

  def evaluateRemove(routingNodeToRemove:Int,fromVehicle:Int):(Move,Long) = {

    val obj = if(debug) {
      generateCheckerObjForVehicles(globalObjective, Set(fromVehicle), penaltyChanged = true)
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
  /**
   * deletions are from deleted node to trashNode
   */
  private def exploreDeletions(): Unit = {
    for ((vehicleID, routingNodesToRemove) <- vehicleToRoutedNodes if !vehicleHasDirectInsertOrMove(vehicleID)) {
      for (routingNodeToRemove <- routingNodesToRemove) {
        evaluateRemove(routingNodeToRemove:Int,vehicleID) match{
          case null => ;
          case (move,delta) =>
            val symbolicNodeOfNodeToRemove = nodeIDToNode(routingNodeToRemove)
            edgeBuilder.addEdge(symbolicNodeOfNodeToRemove, trashNode, delta, move, VLSNMoveType.Remove)
        }
      }
    }
  }

  //should be called after all edges going to vehicle are generated
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


  def evaluateRemoveOnSourceVehicle(routingNodeToRemove:Int,fromVehicle:Int):(Move, Long) = {
    nodeToRemoveNeighborhood(routingNodeToRemove)
      .getMove(vehicleToObjectives(fromVehicle),initialVehicleToObjectives(fromVehicle),
        acceptanceCriterion = (_,newObj) => newObj != Int.MaxValue) match{
      case NoMoveFound => null
      case MoveFound(move) =>
        val delta = move.objAfter - initialVehicleToObjectives(fromVehicle)
        (move,delta) //will very likely always be negative because of triangular inequality
    }
  }

  //no move edges from trashNode to each routed node wit no move,
  // but with delta equal to impact of removing the node from the route.
  private def exploreEjections(): Unit = {
    for ((vehicleID, routingNodesToRemove) <- vehicleToRoutedNodes if !vehicleHasDirectInsertOrMove(vehicleID)) {
      for (routingNodeToRemove <- routingNodesToRemove) {
        evaluateRemoveOnSourceVehicle(routingNodeToRemove:Int,vehicleID) match{
          case null => ;
          case (move,delta) =>
            val symbolicNodeOfNodeToRemove = nodeIDToNode(routingNodeToRemove)
            edgeBuilder.addEdge(trashNode, symbolicNodeOfNodeToRemove, delta, null, VLSNMoveType.SymbolicTrashToNodeForEject)
        }
      }
    }
  }
}
