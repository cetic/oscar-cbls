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
                                nbEdgesPerBundle:Int)

class BundledVLSN(v:Int,
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
                  debugNeighborhoodExploration:Boolean = false)
                 (enrichment:Option[EnrichmentParameters] =
                  Some(
                    EnrichmentParameters(
                      injectAllCacheBeforeEnriching = false,
                      minNbEdgesToExplorePerLevel = 0,
                      minNbAddedEdgesPerLevel = 1000,
                      nbEdgesPerBundle= v)))
  extends Neighborhood {

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
                       acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {

    if(printTakenMoves) println("start VLSN")
    val initialSolution = obj.model.solution(true)

    var somethingDone: Boolean = false

    if(reoptimizeAtStartUp){
      for(vehicle <- 0 until v){
        doReoptimize(vehicle)
      }
    }

    if(debugNeighborhoodExploration){
      require(globalObjective.value == obj.value, "global objective given to VLSN(" + globalObjective.value + ") not equal to Obj of search procedure (" + obj.value + ")")
      val summedPartialObjective = vehicleToObjective.map(_.value).sum + unroutedPenalty.value
      require(summedPartialObjective == globalObjective.value, "summed partial objectives with unrouted (" + summedPartialObjective + ") not equal to global objective (" + globalObjective.value + ")")
    }

    //first VLSN search
    var dataForRestartOpt = doVLSNSearch(
      initVehicleToRoutedNodesToMove(),
      initUnroutedNodesToInsert(),
      None)

    doAfterCycle match {
      case Some(toDo) => toDo()
      case None => ()
    }

    var remainingIt = maxIt

    //we restart with incremental restart as much as posible
    while (dataForRestartOpt.isDefined && remainingIt > 0) {
      remainingIt = remainingIt - 1
      val dataForRestart = dataForRestartOpt.get
      somethingDone = true
      dataForRestartOpt = restartVLSNIncrementally(oldGraph = dataForRestart.oldGraph,
        performedMoves = dataForRestart.performedMoves,
        oldVehicleToRoutedNodesToMove = dataForRestart.oldVehicleToRoutedNodesToMove,
        oldUnroutedNodesToInsert = dataForRestart.oldUnroutedNodesToInsert,
        cacheWasBuiltWithIncrementalEnrichment=dataForRestart.cacheWasBuiltWithIncrementalEnrichment)
      doAfterCycle match {
        case Some(toDo) => toDo()
        case None => ()
      }
    }

    if (somethingDone) {
      val finalSolution = obj.model.solution(true)
      val finalObj = obj.value

      initialSolution.restoreDecisionVariables()

      if(acceptanceCriterion(initialObj,finalObj)){
        MoveFound(LoadSolutionMove(finalSolution, finalObj, name))
      }else{
        NoMoveFound
      }

    } else {
      NoMoveFound
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private def doVLSNSearch(vehicleToRoutedNodesToMove: Map[Int, Set[Int]],
                           unroutedNodesToInsert: Set[Int],
                           cachedExplorations: Option[CachedExplorations]): Option[DataForVLSNRestart] = {

    val nodeToRelevantVehiclesNow = nodeToRelevantVehicles()
    val n = nodeToRelevantVehiclesNow.size

    val moveExplorer:BundledMoveExplorer = new BundledMoveExplorer(
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
      verbose = false,
      enrichment = enrichment.getOrElse(
        EnrichmentParameters(injectAllCacheBeforeEnriching = false,
          minNbEdgesToExplorePerLevel = Int.MaxValue,
          minNbAddedEdgesPerLevel =  Int.MaxValue,
          nbEdgesPerBundle =  Int.MaxValue)
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

      dirtyVehicles = dirtyVehicles ++ theImpactedVehicles
      dirtyNodes = dirtyNodes ++ edges.flatMap(edge => List(edge.from.representedNode,edge.to.representedNode).filter(_ > v))

      for (vlsnNode <- vlsnGraph.nodes) {
        if ((impactedRoutingNodes contains vlsnNode.representedNode) || (theImpactedVehicles contains vlsnNode.vehicle)) {
          liveNodes(vlsnNode.nodeID) = false
        }
      }
    }

    def performEdgesInCycle(edges:List[Edge]): Unit ={
      val delta = edges.map(edge => edge.deltaObj).sum
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
      vlsnGraph = moveExplorer.enrichGraph(dirtyNodes, dirtyVehicles, printTakenMoves)._1

      if(printTakenMoves) {
        println("            " + vlsnGraph.statisticsString + " added " +
          (vlsnGraph.nbEdges - nbEdgesAtPreviousIteration) + " edges")
      }

      if(vlsnGraph.nbEdges == nbEdgesAtPreviousIteration && currentEnrichmentLevel !=0){
        if(printTakenMoves) {
          println("            skip cycle search")
        }
      }else {
        nbEdgesAtPreviousIteration = vlsnGraph.nbEdges
        var cycleFound: Boolean = true
        //now, we search for every cycles in this graph
        while (cycleFound) {
          CycleFinderAlgo(vlsnGraph, cycleFinderAlgoSelection).findCycle(liveNodes) match {
            case Some(listOfEdge) =>
              if (printTakenMoves) printCycle(listOfEdge)
              killCycles(listOfEdge, vlsnGraph)
              cycleFound = true
            case None =>
              //we did not find any move at all on the graph, so it can be enriched now
              cycleFound = false
          }
        }
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
      println("   - ?  " + computedNewObj + "   " + name + "  (nbUnrouted:" + unroutedNodesToInsert.size + ")")
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

    //there is no possible incremental restart for VLSN
    if (acc.isEmpty) {
      None
    } else {
      Some(DataForVLSNRestart(
        vlsnGraph,
        acc.flatten,
        vehicleToRoutedNodesToMove,
        unroutedNodesToInsert,
        cacheWasBuiltWithIncrementalEnrichment = enrichment.isDefined))
    }
  }

  // ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  case class DataForVLSNRestart(oldGraph: VLSNGraph,
                                performedMoves: List[Edge],
                                oldVehicleToRoutedNodesToMove: Map[Int, Set[Int]],
                                oldUnroutedNodesToInsert: Set[Int],
                                cacheWasBuiltWithIncrementalEnrichment:Boolean)

  private def restartVLSNIncrementally(oldGraph: VLSNGraph,
                                       performedMoves: List[Edge],
                                       oldVehicleToRoutedNodesToMove: Map[Int, Set[Int]],
                                       oldUnroutedNodesToInsert: Set[Int],
                                       cacheWasBuiltWithIncrementalEnrichment:Boolean):Option[DataForVLSNRestart] = {

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
      cachedExplorations)
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
