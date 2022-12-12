package oscar.cbls.lib.search.neighborhoods.vlsn

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

import oscar.cbls.Objective
import oscar.cbls.core.search._
import oscar.cbls.lib.search.neighborhoods.vlsn.CycleFinderAlgoType.CycleFinderAlgoType
import oscar.cbls.lib.search.neighborhoods.vlsn.VLSNMoveType._

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

case class EnrichmentParameters(injectAllCacheBeforeEnriching:Boolean,
                                minNbEdgesToExplorePerLevel:Int,
                                minNbAddedEdgesPerLevel:Int,
                                nbEdgesPerBundle:Int,
                                nbEdgesPerPriorityBundle:Int)

object VLSN{

  def standardEnrichment(v:Int):EnrichmentParameters =
    EnrichmentParameters(
      injectAllCacheBeforeEnriching = true,
      minNbEdgesToExplorePerLevel = 0,
      minNbAddedEdgesPerLevel = 1000,
      nbEdgesPerBundle= v,
      nbEdgesPerPriorityBundle = 10*v)

  def noEnrichment:EnrichmentParameters =
    EnrichmentParameters(
      injectAllCacheBeforeEnriching = false,
      minNbEdgesToExplorePerLevel = Int.MaxValue,
      minNbAddedEdgesPerLevel =  Int.MaxValue,
      nbEdgesPerBundle =  Int.MaxValue,
      nbEdgesPerPriorityBundle = Int.MaxValue)
}


/**
 * Very Large Scale Neighborhood
 * it searches for a composition of atomic moves that, together improve the objective function, although separatedly, they are not feasible.
 * check @Article{Mouthuy2012,
 *     author="Mouthuy, S{\'e}bastien and Hentenryck, Pascal Van and Deville, Yves",
 *     title="Constraint-based Very Large-Scale Neighborhood search",
 *     journal="Constraints",
 *     year="2012",
 *     month="Apr",
 *     volume="17",
 *     number="2",
 *     pages="87L--122L"}
 * {{{
 *def vlsn(l:Int = Int.MaxValue) = {
 *
 * val lClosestNeighborsByDistance: Array[SortedSet[Int]] = Array.tabulate(n)(node =>
 *   SortedSet.empty[Int] ++ myVRP.kFirst(l, closestRelevantNeighborsByDistance)(node))
 *
 * val nodeToAllVehicles = SortedMap.empty[Int, Iterable[Int]] ++ (v until n).map(node => (node, vehicles))
 *
 * def routeUnroutedPointVLSN(targetVehicle: Int):(Int => Neighborhood) = {
 *   if(vehicletoWorkload(targetVehicle).value + serviceTimePerNode > maxWorkloadPerVehicle){
 *     (_ => NoMoveNeighborhood)
 *   }else {
 *     val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(targetVehicle)
 *
 *     unroutedNodeToInsert:Int => {
 *       val lNearestNodesOfTargetVehicle = nodesOfTargetVehicle.filter(x => lClosestNeighborsByDistance(unroutedNodeToInsert) contains x)
 *       insertPointUnroutedFirst(
 *         () => List(unroutedNodeToInsert),
 *         () => _ => lNearestNodesOfTargetVehicle,
 *         myVRP,
 *         hotRestart = false,
 *         selectInsertionPointBehavior = Best(),
 *         positionIndependentMoves = true //compulsory because we are in VLSN
 *       )
 *     }
 *   }
 * }
 *
 * def movePointVLSN(targetVehicle: Int):(Int => Neighborhood) = {
 *   if(vehicletoWorkload(targetVehicle).value + serviceTimePerNode > maxWorkloadPerVehicle){
 *     (_ => NoMoveNeighborhood)
 *   }else {
 *     val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(targetVehicle)
 *
 *     nodeToMove:Int => {
 *       val lNearestNodesOfTargetVehicle = nodesOfTargetVehicle.filter(x => lClosestNeighborsByDistance(nodeToMove) contains x)
 *       onePointMove(
 *         () => List(nodeToMove),
 *         () => _ => lNearestNodesOfTargetVehicle,
 *         myVRP,
 *         selectDestinationBehavior = Best(),
 *         hotRestart = false,
 *         positionIndependentMoves = true  //compulsory because we are in VLSN
 *       )
 *     }
 *   }
 * }
 *
 * def removePointVLSN(node: Int) =
 *   removePoint(
 *     () => List(node),
 *     myVRP,
 *     positionIndependentMoves = true,
 *     hotRestart = false)
 *
 * def threeOptOnVehicle(vehicle:Int) = {
 *   val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(vehicle)
 *   //insertions points are position where we perform the insert,
 *   // basically the segment will start in plae of the insertion point and the insertion point will be moved upward
 *   val nodesOfTargetVehicleButVehicle = nodesOfTargetVehicle.filter(_ != vehicle)
 *   val insertionPoints = if(vehicle != v-1) vehicle+1 :: nodesOfTargetVehicleButVehicle else nodesOfTargetVehicleButVehicle
 *
 *   threeOpt(() => insertionPoints,
 *     () => _ => nodesOfTargetVehicleButVehicle,
 *     myVRP,
 *     breakSymmetry = false)
 * }
 *
 * def removeAndReInsertVLSN(pointToRemove: Int): (() => Unit) = {
 *   val checkpointBeforeRemove = myVRP.routes.defineCurrentValueAsCheckpoint(true)
 *   require(pointToRemove >= v, "cannot remove vehicle point: " + v)
 *
 *   myVRP.routes.value.positionOfAnyOccurrence(pointToRemove) match {
 *     case None => throw new Error("cannot remove non routed point:" + pointToRemove)
 *     case Some(positionOfPointToRemove) =>
 *       myVRP.routes.remove(positionOfPointToRemove)
 *   }
 *
 *   def restoreAndRelease: (() => Unit) = () => {
 *     myVRP.routes.rollbackToTopCheckpoint(checkpointBeforeRemove)
 *     myVRP.routes.releaseTopCheckpoint()
 *   }
 *
 *   restoreAndRelease
 * }
 *
 * new VLSN(
 *   v,
 *   () => {
 *     SortedMap.empty[Int, SortedSet[Int]] ++ vehicles.map((vehicle: Int) => (vehicle, SortedSet.empty[Int] ++ myVRP.getRouteOfVehicle(vehicle).filter(_ >= v)))
 *   },
 *
 *   () => SortedSet.empty[Int] ++ myVRP.unroutedNodes,
 *   nodeToRelevantVehicles = () => nodeToAllVehicles,
 *
 *   targetVehicleNodeToInsertNeighborhood = routeUnroutedPointVLSN,
 *   targetVehicleNodeToMoveNeighborhood = movePointVLSN,
 *   removePointVLSN,
 *   removeNodeAndReInsert = removeAndReInsertVLSN,
 *
 *   reOptimizeVehicle = Some(vehicle => Some(threeOptOnVehicle(vehicle))),
 *   useDirectInsert = true,
 *
 *   objPerVehicle,
 *   unroutedPenaltyObj,
 *   obj,
 *
 *   cycleFinderAlgoSelection = CycleFinderAlgoType.Mouthuy,
 *
 *   name="VLSN(" + l + ")"
 * )
 *}}}
 *
 * VLSN is a saturating neighborhood, that is: it will run until no more moves can be found, and at this point,
 * it wil return a single move that actually reloads the solution that was reached step-by-step by the VLSN.
 * Typically, you may want to use VLSN MaxMoves 1 because it is useless to call it more than once.
 *
 * @param v the number of vehicles
 * @param initVehicleToRoutedNodesToMove a function that generates a map from vehicle to nodes that are to be moved
 *                                       (or unrouted) by this neighborhood (other nodes present on the vehicles will not be moved)
 * @param initUnroutedNodesToInsert a function generating the nodes that are not routed
 *                                  an that the VLSN should try and route
 * @param nodeToRelevantVehicles a map from node to the vehicles where the node can be routed.
 *                               It must be defined for each node mentioned in initVehicleToRoutedNodesToMove
 *                               and initUnroutedNodesToInsert
 * @param targetVehicleNodeToInsertNeighborhood vehicle to node to a neighborhood that try and insert the node
 *                                              on the vehicle.
 *                                              VLSN guarantees that the node is not routed
 *                                              (it was in initUnroutedNodesToInsert and not routed yet, or was unrouted)
 *                                              You'd better use best for this neighborhood, and no hot restart
 *                                              the moves returned by this neighborhood must be position-independent
 *                                              (check API of your neighborhood for that)
 * @param targetVehicleNodeToMoveNeighborhood vehicle to node to a neighborhood that try and move the node to the vehicle.
 *                                            VLSN guarantees that the node is routed, and reached by another vehicle.
 *                                             You'd better use best for this neighborhood, and no hot restart
 *                                             the moves returned by this neighborhood must be position-independent
 *                                             (check API of your neighborhood for that)
 * @param nodeToRemoveNeighborhood node to a neighborhood that try and remove the node.
 *                                  VLSN guarantees that the node is routed.
 *                                  you do not need any hot restart here
 *                                  the moves returned by this neighborhood must be position-independent (check API of your neighborhood for that)
 * @param removeNodeAndReInsert this is a particular procedure that given a node, which VLSN guarantees to be routed,
 *                              removes teh node from the route, and returns a procedure to re-insert it.
 *                              VLSN guarantees that the remove procedure will be called on a route
 *                              that is restored to its state when the node was removed.
 * @param reOptimizeVehicle an optional procedure to re-optimize the route of a vehicle that VLSN just
 *                          improved by performing some exchange with another vehicle or other atomic move of the VLSN
 *                          given a vehicle,it is expected to return an (optional again) neighborhood that is then exhausted
 *                          the returned neighborhood cannot modify the route of any other vehicle thn the one specified
 * @param vehicleToObjective an array of size v that gives the objective function per vehicle. it must incorporate the strong constraint as well.
 * @param unroutedPenalty the penalty for unrouted nodes
 * @param globalObjective the global objective, which must be a sum of the above objective functions
 *                        (you can of course re-factor so that he strong constraints appear only once)
 * @param cycleFinderAlgoSelection the cycle finder algo to use. Mouthy is the fastest. In some rara case, you might experiment with MouthuyAndThenDFS.
 *                                 DFS is complete, but slower than Mouthuy
 * @param doAfterCycle an additional method that will be regularly called by the VLSN. you can use it to perform some rendering of the search progress,
 *                     but do not modify the current solution; this can only be done through the reOptimizeVehicle method.
 * @param name a name that will be used in pretty printing.
 * @param reoptimizeAtStartUp to automatically perform the re-optimization
 *                            on all vehicle before starting up the VLSN process
 * @param debugNeighborhoodExploration The base neighborhood should only modify the vehicle they are supposed to.
 *                                     if they modify another vehicle, this causes the VLSN to crash for unclear reasons.
 *                                     if this flag is set, the VLSN will check that the base neighborhood
 *                                     only modify vehicles they are supposed to, and report a clear error if they do not.
 *                                     by default, it is not set because there is a (small) time overhead in this flag.
 * @param enrichment with this parameter, you can specify a gradual enrichment of the VLSN graph.
 *                   This provides a speedup on large problems without impacting the completeness of the VLSN.
 *                   by default, incremental enrichment is deactivated.
 * @author renaud.delandtsheer@cetic.be
 */
class VLSN(v:Int,
           initVehicleToRoutedNodesToMove:() => Map[Int,SortedSet[Int]],
           initUnroutedNodesToInsert:() => SortedSet[Int],
           nodeToRelevantVehicles:() => Map[Int,Iterable[Int]],

           targetVehicleNodeToInsertNeighborhood:Int => Int => Neighborhood,
           targetVehicleNodeToMoveNeighborhood:Int => Int => Neighborhood,
           nodeToRemoveNeighborhood:Int => Neighborhood,

           removeNodeAndReInsert:Int => () => Unit,

           reOptimizeVehicle:Option[Int => Option[Neighborhood]],

           vehicleToObjective:Array[Objective],
           unroutedPenalty:Objective,
           globalObjective:Objective,
           cycleFinderAlgoSelection:CycleFinderAlgoType = CycleFinderAlgoType.Mouthuy,
           maxIt : Int = Int.MaxValue,
           doAfterCycle : Option[() => Unit] = None,
           name:String = "VLSN",

           reoptimizeAtStartUp:Boolean = true,
           debugNeighborhoodExploration:Boolean = false,
           enrichment:Option[EnrichmentParameters] = None)
  extends Neighborhood {

  override val profiler: NeighborhoodProfiler = new NeighborhoodProfiler(this)

  def doReoptimize(vehicle:Int): Unit = {
    val reOptimizeNeighborhoodGenerator = reOptimizeVehicle match{
      case None => return
      case Some(reOptimizeNeighborhoodGenerator) => reOptimizeNeighborhoodGenerator
    }

    val oldObjVehicle = vehicleToObjective(vehicle).value
    val oldGlobalObjective = globalObjective.value

    reOptimizeNeighborhoodGenerator(vehicle) match {
      case None => ;
      case Some(n) =>
        n.verbose = 0
        val nbPerformedMoves = n.doAllMoves(obj = globalObjective)
        if ((printTakenMoves && nbPerformedMoves > 0L) || (printExploredNeighborhoods && nbPerformedMoves == 0L)) {
          println(s"   - ?  " + globalObjective.value + s"   $name:ReOptimizeVehicle(vehicle:$vehicle, neighborhood:$n nbMoves:$nbPerformedMoves)")
        }

        val vehicleObjDelta = vehicleToObjective(vehicle).value - oldObjVehicle
        val globalObjDelta = globalObjective.value - oldGlobalObjective

        require(vehicleObjDelta == globalObjDelta,
          "re-optimization of vehicle " + vehicle + " wih" + n + " did impact other vehicle, vehicleObjDelta:" + vehicleObjDelta + " globalObjDelta:" + globalObjDelta)

    }
  }

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriterion: AcceptanceCriterion): SearchResult = {
    profiler.explorationStarted()
    if (printTakenMoves) println("start VLSN")
    val initialSolution = obj.model.solution(true)

    if (reoptimizeAtStartUp) {
      for (vehicle <- 0 until v) {
        doReoptimize(vehicle)
      }
    }

    if (debugNeighborhoodExploration){
      require(globalObjective.value == obj.value, "global objective given to VLSN(" + globalObjective.value + ") not equal to Obj of search procedure (" + obj.value + ")")
      val summedPartialObjective = vehicleToObjective.map(_.value).sum + unroutedPenalty.value
      require(summedPartialObjective == globalObjective.value, "summed partial objectives with unrouted (" + summedPartialObjective + ") not equal to global objective (" + globalObjective.value + ")")
    }

    //first VLSN search
    var (dataForRestartOpt,hotRestart) = doVLSNSearch(
      initVehicleToRoutedNodesToMove(),
      initUnroutedNodesToInsert(),
      None,None)

    val somethingDone = dataForRestartOpt.isDefined

    doAfterCycle match {
      case Some(toDo) => toDo()
      case None => ()
    }

    var remainingIt = maxIt-1

    //we restart as much as possible, there is no data for restart when there is no cycle found at all
    while (dataForRestartOpt.isDefined && remainingIt > 0) {
      remainingIt = remainingIt - 1
      val dataForRestart = dataForRestartOpt.get
       val x = restartVLSNIncrementally(oldGraph = dataForRestart.oldGraph,
        performedMoves = dataForRestart.performedMoves,
        oldVehicleToRoutedNodesToMove = dataForRestart.oldVehicleToRoutedNodesToMove,
        oldUnroutedNodesToInsert = dataForRestart.oldUnroutedNodesToInsert,
        cacheWasBuiltWithIncrementalEnrichment=dataForRestart.cacheWasBuiltWithIncrementalEnrichment,
        Some(hotRestart))
      dataForRestartOpt = x._1
      hotRestart = x._2
      doAfterCycle match {
        case Some(toDo) => toDo()
        case None => ()
      }
      profiler.neighborExplored()
    }

    if (somethingDone) {
      val finalSolution = obj.model.solution(true)
      val finalObj = obj.value

      initialSolution.restoreDecisionVariables()

      if (acceptanceCriterion(initialObj,finalObj)) {
        profiler.explorationEnded(Some(initialObj - finalObj))
        MoveFound(LoadSolutionMove(finalSolution, finalObj, name))
      } else {
        profiler.explorationEnded(None)
        NoMoveFound
      }

    } else {
      profiler.explorationEnded(None)
      NoMoveFound
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private def doVLSNSearch(vehicleToRoutedNodesToMove: Map[Int, Set[Int]],
                           unroutedNodesToInsert: Set[Int],
                           cachedExplorations: Option[CachedExplorations],
                           hotRestart:Option[HotRestartInfo]): (Option[DataForVLSNRestart],HotRestartInfo) = {

    val nodeToRelevantVehiclesNow = nodeToRelevantVehicles()
    val n = nodeToRelevantVehiclesNow.size

    val moveExplorer:MoveExplorer = new MoveExplorer(
      v: Int,
      vehicleToRoutedNodesToMove,
      unroutedNodesToInsert,
      nodeToRelevantVehiclesNow,

      targetVehicleNodeToInsertNeighborhood,
      targetVehicleNodeToMoveNeighborhood,
      nodeToRemoveNeighborhood,
      removeNodeAndReInsert,

      vehicleToObjective,
      unroutedPenalty,
      globalObjective,
      cachedExplorations.orNull,
      hotRestart = hotRestart.orNull,

      verbose = false,
      enrichment = enrichment.getOrElse(
        EnrichmentParameters(injectAllCacheBeforeEnriching = false,
          minNbEdgesToExplorePerLevel = Int.MaxValue,
          minNbAddedEdgesPerLevel =  Int.MaxValue,
          nbEdgesPerBundle =  Int.MaxValue,
          nbEdgesPerPriorityBundle = Int.MaxValue)
      )
    )

    var dirtyNodes:SortedSet[Int] = SortedSet.empty

    val liveNodes = Array.fill(moveExplorer.nbNodesInVLSNGraph)(true)

    var dirtyVehicles:SortedSet[Int] = SortedSet.empty

    def impactedVehicles(cycle: List[Edge]):SortedSet[Int] = SortedSet.empty[Int] ++ cycle.flatMap(edge => {
      var l:List[Int] = List.empty
      val vehicleFrom = edge.from.vehicle
      if (vehicleFrom < v && vehicleFrom >= 0) l = vehicleFrom :: Nil
      val vehicleTo = edge.to.vehicle
      if (vehicleTo < v && vehicleTo >= 0) l = vehicleTo :: Nil
      l
    })

    var acc: List[List[Edge]] = List.empty
    var computedNewObj: Long = globalObjective.value

    def killCycles(edges:List[Edge], vlsnGraph:VLSNGraph): Unit ={
      acc = edges :: acc

      val theImpactedVehicles = impactedVehicles(edges)

      val impactedRoutingNodes = SortedSet.empty[Int] ++ edges.flatMap(edge => {
        val node = edge.from.representedNode; if (node >= 0) Some(node) else None
      })

      //require((dirtyVehicles intersect theImpactedVehicles).isEmpty)

      dirtyVehicles = dirtyVehicles ++ theImpactedVehicles
      dirtyNodes = dirtyNodes ++ edges.flatMap(edge => List(edge.from.representedNode,edge.to.representedNode).filter(_ > v))

      for (vlsnNode <- vlsnGraph.nodes) {
        if ((impactedRoutingNodes contains vlsnNode.representedNode) || (theImpactedVehicles contains vlsnNode.vehicle)) {
          //require(liveNodes(vlsnNode.nodeID))
          liveNodes(vlsnNode.vlsnNodeID) = false
        }
      }
    }

    def performEdgesInCycle(edges:List[Edge]): Unit ={
      val delta = edges.map(_.deltaObj).sum
      require(delta < 0, "delta should be negative, got " + delta)
      computedNewObj += delta

      for(edge <- edges){
        if(edge.move != null){
          edge.move.commit()
        }
      }
    }

    def printCycle(cycle:List[Edge]): Unit ={
      val moves = cycle.flatMap(edge => Option(edge.move))
      val vehicles = impactedVehicles(cycle)
      val moveTypes = "[" +
        cycle
          .flatMap(edge => if(edge.move == null) None else Some(edge.moveType))
          .groupBy((a:VLSNMoveType) => a)
          .toList
          .map({case (moveType,l) => (""  + moveType + "->" + l.size)})
          .mkString(",") + "]"
      val deltaObj = cycle.map(edge => edge.deltaObj).sum
      println("                deltaObj:" + deltaObj+ " size:" + moves.length +
        " vehicles:{" + vehicles.mkString(",") + "} moveTypes:" + moveTypes + " moves:{" + moves.mkString(",") + "}")
    }


    var vlsnGraph:VLSNGraph = null
    //We need this graph after completion of the loop to build the cache of not used moves.
    var nbEdgesAtPreviousIteration = moveExplorer.nbEdgesInGraph

    enrichment match{
      case Some(x) if x.injectAllCacheBeforeEnriching =>
        moveExplorer.injectAllCache(printTakenMoves)
        if (printTakenMoves) {
          println("            " + " loaded " +
            (moveExplorer.nbEdgesInGraph - nbEdgesAtPreviousIteration) + " edges from cache")
        }
        nbEdgesAtPreviousIteration = moveExplorer.nbEdgesInGraph
      case _ =>

    }

    var currentEnrichmentLevel = -1
    while ((!moveExplorer.allMovesExplored) && dirtyVehicles.size < v) {
      currentEnrichmentLevel += 1

      if(printTakenMoves) {
        println("            enriching VLSN graph to level " + currentEnrichmentLevel)
      }

      //require(dirtyVehicles.forall( x => x >= 0 && x < v))
      val (newVlsnGraph,_,_,priorityCycles):(VLSNGraph,Any,Any,List[List[Edge]]) = moveExplorer.enrichGraph(dirtyNodes, dirtyVehicles, printTakenMoves)

      vlsnGraph = newVlsnGraph

      if(printTakenMoves) {
        println("            " + vlsnGraph.statisticsString + " added " +
          (vlsnGraph.nbEdges - nbEdgesAtPreviousIteration) + " edges including " + priorityCycles.size + " priority cycles")
      }

      if(vlsnGraph.nbEdges == nbEdgesAtPreviousIteration && currentEnrichmentLevel !=0){
        if(printTakenMoves) {
          println("            skip cycle search")
        }
      }else {
        nbEdgesAtPreviousIteration = vlsnGraph.nbEdges
        var cycleFound: Boolean = true
        //now, we search for every cycles in this graph

        //bypass the cycle detection algo for the priority cycles
        var priorityCyclesToDigest = priorityCycles
        var nbCycles = 0
        while (cycleFound) {
          val cycle = if(priorityCyclesToDigest.nonEmpty){
            val toReturn = priorityCyclesToDigest.head
            priorityCyclesToDigest = priorityCyclesToDigest.tail
            Some(toReturn)
          }else CycleFinderAlgo(vlsnGraph, cycleFinderAlgoSelection).findCycle(liveNodes)

          cycle match {
            case Some(listOfEdge) =>
              if (printTakenMoves) printCycle(listOfEdge)
              killCycles(listOfEdge, vlsnGraph)
              cycleFound = true
              nbCycles += 1

            case None =>
              //we did not find any move at all on the graph, so it can be enriched now
              cycleFound = false
          }
        }
        if(printTakenMoves) println("                nbCycles:" + nbCycles + " nbDirtyVehicles:" + dirtyVehicles.size)
      }
    }

    //println(vlsnGraph.toDOT(light = true))
    for(cycle<-acc){
      performEdgesInCycle(cycle)
    }
    require(globalObjective.value == computedNewObj, "new global objective differs from computed newObj:" + globalObjective + "!=" + computedNewObj + "\nedges:\n" + acc.flatten.mkString("\n\t") + "\nUnrouted Penlaty:" +  unroutedPenalty.value + " - Obj Per Vehicle:\n" + vehicleToObjective.mkString("\n"))

    if(!moveExplorer.allMovesExplored && dirtyVehicles.size == v && printTakenMoves){
      println("       " + "skipped remaining levels because all vehicles are dirty")
    }
    // Now, all vehicles are dirty or have been fully developed through the graph is exhausted,
    // it might not be complete but all vehicles are dirty
    if(printTakenMoves) {
      println(s"   - ?  $computedNewObj   $name  (nbUnrouted:${unroutedNodesToInsert.size})")
    }
    //println(vlsnGraph.toDOT(acc,false,true))

    //re-optimize
    reOptimizeVehicle match{
      case None => ;
      case Some(_) =>
        //re-optimizing impacted vehicles (optional)
        for(vehicle <- impactedVehicles(acc.flatten)){
          doReoptimize(vehicle)
        }
    }

    val newHotRestart = {
      val hr = hotRestart match{
        case None => HotRestartInfo.apply(v)
        case Some(x) => x}
      for(edge <- acc.flatten){
        hr.update(edge)
      }
      hr
    }

    //there is no possible incremental restart for VLSN
    if (acc.isEmpty) {
      (None,newHotRestart)
    } else {
      (Some(DataForVLSNRestart(
        vlsnGraph,
        acc.flatten,
        vehicleToRoutedNodesToMove,
        unroutedNodesToInsert,
        cacheWasBuiltWithIncrementalEnrichment = enrichment.isDefined,
        allVehiclesDirty = dirtyVehicles.size == v)),newHotRestart)
    }
  }

  // ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  case class DataForVLSNRestart(oldGraph: VLSNGraph,
                                performedMoves: List[Edge],
                                oldVehicleToRoutedNodesToMove: Map[Int, Set[Int]],
                                oldUnroutedNodesToInsert: Set[Int],
                                cacheWasBuiltWithIncrementalEnrichment:Boolean,
                                allVehiclesDirty:Boolean)

  private def restartVLSNIncrementally(oldGraph: VLSNGraph,
                                       performedMoves: List[Edge],
                                       oldVehicleToRoutedNodesToMove: Map[Int, Set[Int]],
                                       oldUnroutedNodesToInsert: Set[Int],
                                       cacheWasBuiltWithIncrementalEnrichment:Boolean,
                                       hotRestart:Option[HotRestartInfo]):(Option[DataForVLSNRestart],HotRestartInfo) = {

    val (updatedVehicleToRoutedNodesToMove, updatedUnroutedNodesToInsert) =
      updateZones(performedMoves,
        oldVehicleToRoutedNodesToMove,
        oldUnroutedNodesToInsert)

    val cachedExplorations: Option[CachedExplorations] =
      CachedExplorations(
        oldGraph,
        performedMoves,
        v,
        cacheWasBuiltWithIncrementalEnrichment)

    doVLSNSearch(updatedVehicleToRoutedNodesToMove,
      updatedUnroutedNodesToInsert,
      cachedExplorations,
      hotRestart)
  }

  @tailrec
  private def updateZones(performedMoves: List[Edge],
                          vehicleToRoutedNodesToMove: Map[Int, Set[Int]],
                          unroutedNodesToInsert: Set[Int]): (Map[Int, Set[Int]], Set[Int]) = {

    performedMoves match {
      case Nil => (vehicleToRoutedNodesToMove, unroutedNodesToInsert)
      case edge :: tail =>

        val fromNode = edge.from
        val toNode = edge.to

        edge.moveType match {
          case InsertNoEject =>
            val targetVehicle = toNode.vehicle
            val insertedNode = fromNode.representedNode

            updateZones(
              tail,
              vehicleToRoutedNodesToMove + (targetVehicle -> (vehicleToRoutedNodesToMove.getOrElse(targetVehicle, SortedSet.empty[Int]) + insertedNode)),
              unroutedNodesToInsert - insertedNode
            )

          case InsertWithEject =>
            val targetVehicle = toNode.vehicle
            val insertedNode = fromNode.representedNode
            val ejectedNode = toNode.representedNode

            updateZones(
              tail,
              vehicleToRoutedNodesToMove + (targetVehicle -> (vehicleToRoutedNodesToMove.getOrElse(targetVehicle, SortedSet.empty[Int]) + insertedNode - ejectedNode)),
              unroutedNodesToInsert - insertedNode
            )

          case MoveNoEject =>
            val fromVehicle = fromNode.vehicle
            val targetVehicle = toNode.vehicle
            val movedNode = fromNode.representedNode

            updateZones(
              tail,
              vehicleToRoutedNodesToMove
                + (targetVehicle -> (vehicleToRoutedNodesToMove.getOrElse(targetVehicle, SortedSet.empty[Int]) + movedNode))
                + (fromVehicle -> (vehicleToRoutedNodesToMove(fromVehicle) - movedNode)),
              unroutedNodesToInsert
            )
          case MoveWithEject =>
            val fromVehicle = fromNode.vehicle
            val targetVehicle = toNode.vehicle
            val movedNode = fromNode.representedNode
            val ejectedNode = toNode.representedNode

            updateZones(
              tail,
              vehicleToRoutedNodesToMove
                + (targetVehicle -> (vehicleToRoutedNodesToMove.getOrElse(targetVehicle, SortedSet.empty[Int]) + movedNode - ejectedNode))
                + (fromVehicle -> (vehicleToRoutedNodesToMove(fromVehicle) - movedNode)),
              unroutedNodesToInsert
            )

          case Remove =>
            val fromVehicle = fromNode.vehicle
            val removedNode = fromNode.representedNode

            updateZones(
              tail,
              vehicleToRoutedNodesToMove
                + (fromVehicle -> (vehicleToRoutedNodesToMove(fromVehicle) - removedNode)),
              unroutedNodesToInsert + removedNode
            )

          case SymbolicTrashToInsert | SymbolicVehicleToTrash | SymbolicTrashToNodeForEject =>
            //nothing to do here
            updateZones(tail, vehicleToRoutedNodesToMove, unroutedNodesToInsert)

        }
    }
  }
}
