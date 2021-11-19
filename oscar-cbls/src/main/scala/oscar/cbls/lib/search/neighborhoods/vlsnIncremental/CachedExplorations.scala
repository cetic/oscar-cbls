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

package oscar.cbls.lib.search.neighborhoods.vlsnIncremental

import oscar.cbls.Objective
import oscar.cbls.core.search.{Move, Neighborhood}
import oscar.cbls.lib.search.neighborhoods.vlsn.VLSNMoveType._

import scala.collection.immutable.{SortedMap, SortedSet}

abstract sealed class CachedExploration
case class CachedAtomicMove(move:Move,delta:Long) extends CachedExploration
case object CachedAtomicNoMove extends CachedExploration
case object CacheDirty extends CachedExploration

object CachedAtomicMove{
  def apply(edge:Edge) = new CachedAtomicMove(edge.move,edge.deltaObj)
}

object CachedExplorations{
  def apply(oldGraph:VLSNGraph,
            performedMoves:List[Edge],
            v:Int,
            cacheWasBuiltWithIncrementalEnrichment:Boolean):Option[CachedExplorations] = {

    var dirtyNodes: SortedSet[Int] = SortedSet.empty
    val isDirtyVehicle = Array.fill[Boolean](v)(false)

    for (edge: Edge <- performedMoves) {
      val fromNode = edge.from
      val toNode = edge.to

      edge.moveType match {
        case InsertNoEject | InsertWithEject =>
          dirtyNodes += fromNode.representedNode
          isDirtyVehicle(toNode.vehicle) = true
        case MoveNoEject | MoveWithEject =>
          isDirtyVehicle(fromNode.vehicle) = true
          isDirtyVehicle(toNode.vehicle) = true
        case Remove =>
          isDirtyVehicle(fromNode.vehicle) = true
          dirtyNodes += fromNode.representedNode
        case _ =>
      }
    }

    //println("isDirtyVehicle:" + isDirtyVehicle.indices.map(vehicle => "v_"+vehicle+":"+isDirtyVehicle(vehicle)).mkString(","))
    //println("dirtyNodes:" + dirtyNodes.mkString(","))
    //println(oldGraph.statistics)

    if(isDirtyVehicle.forall(p => p)) None
    else Some(new CachedExplorations(oldGraph: VLSNGraph,
      dirtyNodes:SortedSet[Int],
      isDirtyVehicle: Array[Boolean],
      cacheWasBuiltWithIncrementalEnrichment))
  }
}

class CachedExplorations(oldGraph:VLSNGraph,
                         dirtyNodes:SortedSet[Int], //only for unrouted nodes that were inserted of newly removed
                         isDirtyVehicle:Array[Boolean],
                         cacheWasBuiltWithIncrementalEnrichment:Boolean) {

  def isDirtyNode(node: Int): Boolean = dirtyNodes.contains(node)

  //TODO: use arrays for O(1) access?
  var cachedInsertNoEject: SortedMap[(Int, Int), CachedExploration] = SortedMap.empty //unroute,targetVehicle
  var cachedInsertWithEject: SortedMap[(Int, Int), CachedExploration] = SortedMap.empty //movedNode, ejectedNode
  var cachedMoveNoEject: SortedMap[(Int, Int), CachedExploration] = SortedMap.empty //node,vehicle
  var cachedMoveWithEject: SortedMap[(Int, Int), CachedExploration] = SortedMap.empty
  var cachedRemove: SortedMap[(Int), CachedExploration] = SortedMap.empty

  for (edge <- oldGraph.edges) {

    val fromNode = edge.from
    val toNode = edge.to

    edge.moveType match {
      case InsertNoEject =>
        if (!isDirtyNode(fromNode.representedNode) && !isDirtyVehicle(toNode.vehicle)) {
          cachedInsertNoEject += (fromNode.representedNode, toNode.vehicle) -> CachedAtomicMove(edge)
        }
      case InsertWithEject =>
        if (!isDirtyNode(fromNode.representedNode) && !isDirtyVehicle(toNode.vehicle)) {
          cachedInsertWithEject += (fromNode.representedNode, toNode.representedNode) -> CachedAtomicMove(edge)
        }
      case MoveNoEject =>
        if (!isDirtyNode(fromNode.representedNode) && !isDirtyVehicle(toNode.vehicle)) {
          cachedMoveNoEject += (fromNode.representedNode, toNode.vehicle) -> CachedAtomicMove(edge)
        }
      case MoveWithEject =>
        if (!isDirtyNode(fromNode.representedNode) && !isDirtyVehicle(toNode.vehicle)) {
          cachedMoveWithEject += (fromNode.representedNode, toNode.representedNode) -> CachedAtomicMove(edge)
        }
      case Remove =>
        if (!isDirtyVehicle(fromNode.vehicle)) {
          cachedRemove += fromNode.representedNode -> CachedAtomicMove(edge)
        }
      case _ => ; // non cachable
    }
  }

  if(cacheWasBuiltWithIncrementalEnrichment) {
    for (nonEdge <- oldGraph.nonEdges) {

      val fromNode = nonEdge.from
      val toNode = nonEdge.to

      nonEdge.moveType match {
        case InsertNoEject =>
          if (!isDirtyNode(fromNode.representedNode) && !isDirtyVehicle(toNode.vehicle)) {
            cachedInsertNoEject += (fromNode.representedNode, toNode.vehicle) -> CachedAtomicNoMove
          }
        case InsertWithEject =>
          if (!isDirtyNode(fromNode.representedNode) && !isDirtyVehicle(toNode.vehicle)) {
            cachedInsertWithEject += (fromNode.representedNode, toNode.representedNode) -> CachedAtomicNoMove
          }
        case MoveNoEject =>
          if (!isDirtyNode(fromNode.representedNode) && !isDirtyVehicle(toNode.vehicle)) {
            cachedMoveNoEject += (fromNode.representedNode, toNode.vehicle) -> CachedAtomicNoMove
          }
        case MoveWithEject =>
          if (!isDirtyNode(fromNode.representedNode) && !isDirtyVehicle(toNode.vehicle)) {
            cachedMoveWithEject += (fromNode.representedNode, toNode.representedNode) -> CachedAtomicNoMove
          }
        case Remove =>
          if (!isDirtyVehicle(fromNode.vehicle)) {
            cachedRemove += fromNode.representedNode -> CachedAtomicNoMove
          }
        case _ => ; // non cachable
      }
    }
  }

  //println("cacheSize:" + size)
  //println("dirtyEdges:" + dirtyEdge)

  def getInsertOnVehicleNoRemove(unroutedNodeToInsert: Int,
                                 targetVehicleForInsertion: Int): CachedExploration = {
    if (!isDirtyNode(unroutedNodeToInsert) && !isDirtyVehicle(targetVehicleForInsertion)) {
      cachedInsertNoEject.getOrElse((unroutedNodeToInsert, targetVehicleForInsertion), CachedAtomicNoMove) //TODO: not sure about that if incremental
    } else {
      CacheDirty
    }
  }

  def getInsertOnVehicleWithRemove(unroutedNodeToInsert: Int,
                                   targetVehicleForInsertion: Int,
                                   removedNode: Int): CachedExploration = {
    if (!isDirtyNode(unroutedNodeToInsert) && !isDirtyVehicle(targetVehicleForInsertion)) {
      cachedInsertWithEject.getOrElse((unroutedNodeToInsert, removedNode), CachedAtomicNoMove) //TODO: not sure about that if incremental
    } else {
      CacheDirty
    }
  }

  def getMoveToVehicleNoRemove(routingNodeToMove: Int,
                               fromVehicle: Int,
                               targetVehicle: Int): CachedExploration = {
    //TODO: this is possibly the bottleneck of incremental approach: we forget bout explored and rejected moves
    cachedMoveNoEject.get((routingNodeToMove, targetVehicle)) match{
      case Some(stuff) =>
        stuff
      case None =>
        if (!isDirtyNode(routingNodeToMove) && !isDirtyVehicle(targetVehicle) &&
          (!cacheWasBuiltWithIncrementalEnrichment || !isDirtyVehicle(fromVehicle))) {
          CachedAtomicNoMove
        }else{
          CacheDirty
        }
    }
  }

  def getMoveToVehicleWithRemove(routingNodeToMove: Int,
                                 fromVehicle: Int,
                                 targetVehicle: Int, removedNode: Int): CachedExploration = {
    //TODO: this is possibly the bottleneck of incremental approach: we forget bout explored and rejected moves
    cachedMoveWithEject.get((routingNodeToMove, removedNode)) match{
      case Some(stuff) =>
        stuff
      case None =>
        if (!isDirtyNode(routingNodeToMove) && !isDirtyVehicle(targetVehicle)
          && (!cacheWasBuiltWithIncrementalEnrichment || !isDirtyVehicle(fromVehicle))) {
          CachedAtomicNoMove
        } else {
          CacheDirty
        }
    }
  }

  def getRemoveNode(removedNode: Int, fromVehicle: Int): CachedExploration = {
    if (!isDirtyVehicle(fromVehicle)) {
      cachedRemove.getOrElse(removedNode, CachedAtomicNoMove)
    } else {
      CacheDirty
    }
  }
}

