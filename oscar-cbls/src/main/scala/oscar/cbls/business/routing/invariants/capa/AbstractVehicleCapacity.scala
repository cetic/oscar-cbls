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
package oscar.cbls.business.routing.invariants.capa

import oscar.cbls.algo.rb.RedBlackTreeMap
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.business.routing.model.{ConcreteVehicleLocation, VehicleLocation}
import oscar.cbls.core.computation.{Invariant, SeqUpdateMove}

import scala.annotation.tailrec

/**
  * Maintains the content of vehicles at each node and the starting position of each vehicle
  * @param n The maximum number of nodes
  * @param v The number of vehicles
  */
abstract class AbstractVehicleCapacity(n:Int,
                                       v:Int) extends Invariant{

  /**
    * we only require that zoneStart is <= list.head._2 if exists
    * @param zoneStart
    * @param zoneEnd
    * @param list
    * @return
    * @author renaud.delandtsheer@cetic.be
    */
  protected def smartPrepend(zoneStart: Int, zoneEnd:Int, list:List[(Int,Int)]): List[(Int,Int)] = {
    require(zoneStart >=0)
    require(zoneEnd >=0)
    require(zoneStart <= zoneEnd, s"smartPrepend($zoneStart,$zoneEnd,$list)")
    assert(list.sortWith((lft : (Int, Int), rgt : (Int, Int)) => lft._1 < rgt._1 && lft._2 < rgt._2).equals(list), list + " " + list.sortWith((lft : (Int, Int), rgt : (Int, Int)) => lft._1 < rgt._1 && lft._2 < rgt._2))
    list match {
      case Nil => List((zoneStart, zoneEnd))
      case (oldStart, oldEnd) :: tail =>
        require(zoneStart <= oldEnd, s"zoneStart:$zoneStart oldEnd:$oldEnd")
        if (zoneEnd < oldStart - 1) {
          //the new interval does not touch the old one
          (zoneStart,zoneEnd) :: list
        } else {
          //the new interval touches the old one, there will be some merge
          assert(Math.min(zoneStart, oldStart) <= Math.max(zoneEnd, oldEnd))
          smartPrepend(Math.min(zoneStart, oldStart), Math.max(zoneEnd, oldEnd), tail)
        }
    }
  }

  private def shiftPlusDelta(l:List[(Int,Int)],delta:Int):List[(Int,Int)] = {
    l match {
      case Nil => Nil
      case (a, b) :: tail => (a + delta, b + delta) :: shiftPlusDelta(tail,delta)
    }
  }

  def updateZoneToUpdateAfterInsert(zoneToUpdate: RedBlackTreeMap[List[(Int, Int)]],
                                    posOfInsert:Int,
                                    sequenceBeforeInsert:IntSequence,
                                    vehicleLocationBeforeInsert:VehicleLocation,
                                    vehicleLocationAfterInsert:VehicleLocation):RedBlackTreeMap[List[(Int, Int)]] = {
    if (zoneToUpdate == null) return null
    val vehicleOfInsert = vehicleLocationAfterInsert.vehicleReachingPosition(posOfInsert)
    val startPosOfVehicle = vehicleLocationBeforeInsert.startPosOfVehicle(vehicleOfInsert)
    val relativePosOfInsert = posOfInsert - startPosOfVehicle

    val shouldNextNodeBeIncluded = sequenceBeforeInsert.valueAtPosition(posOfInsert) match {
      case Some(x) if x >= v => true
      case _ => false
    }

    def addInsertionIntoZonesToUpdate(zonesToUpdate:List[(Int,Int)]):List[(Int,Int)] = {
      zonesToUpdate match {
        case Nil =>
          List((relativePosOfInsert, if(shouldNextNodeBeIncluded) relativePosOfInsert+1 else relativePosOfInsert))
        case (startZone, endZone) :: tail =>

          if(relativePosOfInsert < startZone){
            //insert before, the zone
            //shift zonesToUpdate, and smartPrepend (in case the touch)
            smartPrepend(relativePosOfInsert,relativePosOfInsert+1,shiftPlusDelta(zonesToUpdate,1))
          }else{
            if(relativePosOfInsert <= endZone){
              (startZone,endZone+1)::shiftPlusDelta(tail,1)
            }else{
              smartPrepend(startZone,endZone,addInsertionIntoZonesToUpdate(tail))
            }
          }
      }
    }
    val zoneOfVehicleBeforeInsert = zoneToUpdate.getOrElse(vehicleOfInsert, List.empty[(Int, Int)])
    val zoneOFVehicleAfterInsert = addInsertionIntoZonesToUpdate(zoneOfVehicleBeforeInsert)

    zoneToUpdate.insert(vehicleOfInsert, zoneOFVehicleAfterInsert)
  }

  /**
    * this method is for invariant that have other input than the sequence.
    * for instance, a node to node distance based on a conditional distance in a graph.
    * Ths conditional distance in the graph depends on other things than the sequence, hence,
    * some hops might be modified without any changes in the sequence.
    * @param zoneToUpdate the zones to update
    * @param posToRecompute the position where a change has occurred, which must therefore be included in the refresh iterations.
    * @param sequence the sequence (same before and after, because this is for changes that do not affect the sequence)
    * @param vehicleLocation the vehicle location
    * @return the zones to update
    */
  def updateZoneToUpdateWithNewPointToUpdate(zoneToUpdate: RedBlackTreeMap[List[(Int, Int)]],
                                             posToRecompute:Int,
                                             sequence:IntSequence,
                                             vehicleLocation:VehicleLocation):RedBlackTreeMap[List[(Int, Int)]] = {

    if (zoneToUpdate == null) return null
    val vehicle = vehicleLocation.vehicleReachingPosition(posToRecompute)
    val startPosOfVehicle = vehicleLocation.startPosOfVehicle(vehicle)
    val relativePosToRecompute = posToRecompute - startPosOfVehicle

    val shouldNextNodeBeIncluded = sequence.valueAtPosition(posToRecompute) match {
      case Some(x) if x >= v => true
      case _ => false
    }

    def addPositionIntoZonesToUpdate(zonesToUpdate:List[(Int,Int)]):List[(Int,Int)] = {
      zonesToUpdate match {
        case Nil =>
          List((relativePosToRecompute, if(shouldNextNodeBeIncluded) relativePosToRecompute+1 else relativePosToRecompute))
        case (startZone, endZone) :: tail =>

          if(relativePosToRecompute < startZone){
            //insert before, the zone
            //shift zonesToUpdate, and smartPrepend (in case the touch)
            smartPrepend(relativePosToRecompute,relativePosToRecompute+1,zonesToUpdate)
          }else{
            if(relativePosToRecompute <= endZone){
              (startZone,endZone+1)::tail
            }else{
              smartPrepend(startZone,endZone,addPositionIntoZonesToUpdate(tail))
            }
          }
      }
    }
    val zoneOfVehicleBeforeInsert = zoneToUpdate.getOrElse(vehicle, List.empty[(Int, Int)])
    val zoneOFVehicleAfterInsert = addPositionIntoZonesToUpdate(zoneOfVehicleBeforeInsert)

    zoneToUpdate.insert(vehicle, zoneOFVehicleAfterInsert)
  }

  def updateZoneToUpdateAfterRemove(zoneToUpdate: RedBlackTreeMap[List[(Int, Int)]],
                                    posOfRemove:Int,
                                    sequenceBeforeRemove:IntSequence,
                                    vehicleLocationBeforeRemove:VehicleLocation):RedBlackTreeMap[List[(Int, Int)]] = {
    if (zoneToUpdate == null) return null
    val vehicleOfRemove = vehicleLocationBeforeRemove.vehicleReachingPosition(posOfRemove)
    val startPosOfVehicle = vehicleLocationBeforeRemove.startPosOfVehicle(vehicleOfRemove)
    val relativePosOfRemove = posOfRemove - startPosOfVehicle

    val shouldNextNodeBeIncluded = sequenceBeforeRemove.valueAtPosition(posOfRemove + 1) match {
      case Some(x) if x >= v => true
      case _ => false
    }

    val zoneToUpdateOfVehicleOfRemove = zoneToUpdate.getOrElse(vehicleOfRemove, List.empty[(Int, Int)])
    val updatedZoneToUpdateOfVehicleOfRemove = addRemoveIntoZonesToUpdate(relativePosOfRemove, zoneToUpdateOfVehicleOfRemove, shouldNextNodeBeIncluded)
    zoneToUpdate.insert(vehicleOfRemove, updatedZoneToUpdateOfVehicleOfRemove)
  }

  private def addRemoveIntoZonesToUpdate(relativePosOfRemove: Int, zonesToUpdate:List[(Int,Int)], shouldNextNodeBeIncluded:Boolean):List[(Int,Int)] = {
    zonesToUpdate match {
      case Nil =>
        if(shouldNextNodeBeIncluded){
          List((relativePosOfRemove, relativePosOfRemove))
        }else{
          Nil
        }
      case (startZone, endZone) :: tail =>

        if(relativePosOfRemove +1 < startZone){
          //remove happens strictly before the zone
          smartPrepend(relativePosOfRemove , relativePosOfRemove, shiftPlusDelta(zonesToUpdate,-1))
        }else if (relativePosOfRemove +1 == startZone){
          //remove touches startZone
          shiftPlusDelta(zonesToUpdate,-1)
        }else if (relativePosOfRemove < endZone){
          //remove is strictly in the zone (and zone is >1)
          require(startZone != endZone)
          (startZone, endZone - 1) :: shiftPlusDelta(tail, -1)
        }else if(relativePosOfRemove == endZone){
          //remove is on the endZone
          if(shouldNextNodeBeIncluded) {
            //(startZone, endZone) :: shiftPlusDelta(tail, -1)
            smartPrepend(startZone, endZone, shiftPlusDelta(tail, -1))
          }else{
            require(tail.isEmpty,s"deleted last node on route, yet we have more zoneToUpdate after the remove:$tail")
            if(startZone == endZone){
              List.empty
            }else {
              List((startZone, endZone - 1))
            }
          }
        }else if (endZone < relativePosOfRemove){
          //remove is after the zone
          smartPrepend(startZone,endZone, addRemoveIntoZonesToUpdate(relativePosOfRemove, tail, shouldNextNodeBeIncluded))
        }else{
          throw new Error("unexpected case")
        }
    }
  }

  // @Note => O(listToInsert+toInsert)
  private def insertInList(listToInsert: List[(Int, Int)], toInsert: List[(Int, Int)]): List[(Int, Int)] = {
    listToInsert match {
      case Nil => toInsert
      case (start, end) :: _ =>
        toInsert match {
          case Nil => listToInsert
          case (s, e) :: _ =>
            if (s > end) smartPrepend(start, end, insertInList(listToInsert.drop(1), toInsert))
            else if (s == start && end == e) insertInList(listToInsert, toInsert.drop(1))
            else smartPrepend(s, e, insertInList(listToInsert, toInsert.drop(1)))
        }
    }
  }

  /**
    * Updates vehicles starting positions and list zones of position of nodes which content have to be updated after the move
    * @param zonesToUpdate
    * @param m
    * @param sequenceBeforeMove
    * @param vehicleLocationBeforeMove
    * @return
    */
  def updateZoneToUpdateAfterMove(zonesToUpdate: RedBlackTreeMap[List[(Int, Int)]],
                                  m:SeqUpdateMove,
                                  sequenceBeforeMove:IntSequence,
                                  vehicleLocationBeforeMove:VehicleLocation,
                                  vehicleLocationAfterMove:VehicleLocation):RedBlackTreeMap[List[(Int, Int)]] = {
    if (zonesToUpdate == null) return null
    if (m.isNop) return zonesToUpdate

    val sourceVehicle = vehicleLocationBeforeMove.vehicleReachingPosition(m.fromIncluded)
    val startPositionOfSourceVehicle = vehicleLocationBeforeMove.startPosOfVehicle(sourceVehicle)
    val relativeFromIncluded = m.fromIncluded - startPositionOfSourceVehicle
    val relativeToIncluded = m.toIncluded - startPositionOfSourceVehicle

    if (m.isSimpleFlip) {
      //in case of flip, we must to update all nodes in the flip and the node after the flip
      // (this node after might not exist, actually)
      val shouldNextNodeBeIncluded = m.prev.newValue.valueAtPosition(m.toIncluded + 1) match {
        case Some(x) if x >= v => true
        case _ => false
      }
      zonesToUpdate.insert(sourceVehicle,
        insertInList(zonesToUpdate.getOrElse(sourceVehicle, List.empty),
          List((relativeFromIncluded, if (shouldNextNodeBeIncluded) relativeToIncluded + 1 else relativeToIncluded))))
    } else {
      val destinationVehicle = vehicleLocationBeforeMove.vehicleReachingPosition(m.after)
      val relativeAfter = m.after - vehicleLocationBeforeMove.startPosOfVehicle(destinationVehicle)
      val nbPointsInMovedSegment = m.nbPointsInMovedSegment

      val shouldNextNodeBeIncludedInVehicleFrom = m.prev.newValue.valueAtPosition(m.toIncluded + 1) match {
        case Some(x) if x >= v => true
        case _ => false
      }

      val shouldNextNodeBeIncludedInVehicleTo = sequenceBeforeMove.valueAtPosition(m.after+1) match {
        case Some(x) if x >= v => true
        case _ => false
      }

      /**
        * @param listOfZonesForVehicle the list of zones to update on vehicleFrom
        * @return (cleanedList,removedList)
        *         cleanedList: the list of zones where the subsequence of the move has been removed.
        *         An additional zone of size 1 is added after the removed subsequence
        *         removedList: and the list of zones that are within the moved subsequence,
        *         not including the first position in this segment unless covered by zone in the input.
        *         These removed zones are re-computed to be relative to the subsequence start.
        */
      def removeMovedZoneFromZonesToUpdate(listOfZonesForVehicle : List[(Int, Int)]) : (List[(Int, Int)], List[(Int, Int)]) = {
        listOfZonesForVehicle match {
          case Nil =>
            if(shouldNextNodeBeIncludedInVehicleFrom)(List((relativeFromIncluded, relativeFromIncluded)), Nil)
            else (Nil, Nil)
          case (startZone, endZone) :: tail =>
            if (endZone < relativeFromIncluded) {
              // this couple is before the moved subsequence, recurse
              val (updatedZones, removedZones) = removeMovedZoneFromZonesToUpdate(tail)
              (smartPrepend(startZone, endZone, updatedZones), removedZones)
            } else {
              //overlaps start here

              if (shouldNextNodeBeIncludedInVehicleFrom) {
                val (updatedZones, removedZones) = removeMovedZoneFromZonesToUpdateWithin(listOfZonesForVehicle)

                val updatedZoneWithIncludedPoint = updatedZones match{
                  case (a,b) :: updZs if b <= relativeFromIncluded => smartPrepend(a,b,smartPrepend(relativeFromIncluded, relativeFromIncluded, updZs))
                  case _ => smartPrepend(relativeFromIncluded, relativeFromIncluded, updatedZones)
                }
                (updatedZoneWithIncludedPoint, removedZones)
              } else {
                removeMovedZoneFromZonesToUpdateWithin(listOfZonesForVehicle)
              }
            }
        }
      }

      def removeMovedZoneFromZonesToUpdateWithin(listOfZonesForVehicle : List[(Int, Int)]) : (List[(Int, Int)], List[(Int, Int)]) = {
        listOfZonesForVehicle match {
          case Nil => (Nil, Nil)
          case (startZone, endZone) :: tail =>
            if (startZone > relativeToIncluded) {
              //we are after the moved subsequence, so just shift the tail and finish
              (shiftPlusDelta(listOfZonesForVehicle, -nbPointsInMovedSegment), Nil)
            } else {
              // there is an overlap between the moved subsequence and [startZone endZone]
              val (updatedTail, removedZones) = removeMovedZoneFromZonesToUpdateWithin(tail)

              val coupleFromRemovedZone =
                (Math.max(startZone, relativeFromIncluded) - relativeFromIncluded,
                  Math.min(relativeToIncluded, endZone) - relativeFromIncluded)
              val updatedRemovedZone = coupleFromRemovedZone :: removedZones

              //now, we compute what remains of the zone after the segment has been removed.
              val anythingLeftAtStartOfZone = startZone < relativeFromIncluded
              val anythingLeftAtEndOfZone = relativeToIncluded < endZone
              if (anythingLeftAtStartOfZone) {
                if (anythingLeftAtEndOfZone) {
                  (smartPrepend(startZone, endZone - nbPointsInMovedSegment, updatedTail), updatedRemovedZone)
                } else {
                  (smartPrepend(startZone, relativeFromIncluded - 1, updatedTail), updatedRemovedZone)
                }
              } else {
                if (anythingLeftAtEndOfZone) {
                  (smartPrepend(relativeFromIncluded, endZone - nbPointsInMovedSegment, updatedTail), updatedRemovedZone)
                } else {
                  //nothing left of the zone
                  (updatedTail, updatedRemovedZone)
                }
              }
            }
        }
      }

      val (updatedListOfZonesForVehicleFrom, removedZones) = removeMovedZoneFromZonesToUpdate(zonesToUpdate.getOrElse(sourceVehicle, Nil))
      val zonesToUpdateWithVehicleFromUpdated = zonesToUpdate.insert(sourceVehicle, updatedListOfZonesForVehicleFrom)

      //building the sequence to insert back. also adding a compulsory zone at start of this zone (since its pred has changed)
      val relativeZonesToInsert =
        if (m.flip) List((0, nbPointsInMovedSegment - 1))
        else smartPrepend(0, 0, removedZones)

      //println("shouldNextNodeBeIncludedInVehicleTo:" + shouldNextNodeBeIncludedInVehicleTo)
      //println("relativeZonesToInsert:" + relativeZonesToInsert)

      val relativeAfterInNewSequence = m.oldPosToNewPos(m.after).get - vehicleLocationAfterMove.startPosOfVehicle(destinationVehicle)
      val relativeAfterWhenSegmentIsRemoved = relativeAfterInNewSequence

      require(relativeAfterWhenSegmentIsRemoved >=0)

      def insertMovedZones(listOfZonesForVehicle : List[(Int, Int)], zonesToInsert : List[(Int, Int)],insertionPosition:Int = relativeAfterWhenSegmentIsRemoved +1) : List[(Int, Int)] = {

        listOfZonesForVehicle match {
          case Nil =>

            if(shouldNextNodeBeIncludedInVehicleTo){
              insertMovedZonesWithin(zonesToInsert, relativeAfterInNewSequence + 1, List((relativeAfterInNewSequence+1,relativeAfterInNewSequence+1)))
            }else {
              shiftPlusDelta(zonesToInsert, relativeAfterInNewSequence + 1)
            }

          case (startZone, endZone) :: tail =>
            if (endZone < insertionPosition) {
              //startZone endZone startOfFirstZoneToInsertRebased
              //still before the insertion zone
              smartPrepend(startZone, endZone,insertMovedZones(tail, zonesToInsert,insertionPosition))
            } else if (insertionPosition <= startZone) {
              if(shouldNextNodeBeIncludedInVehicleTo){
                insertMovedZonesWithin(zonesToInsert, insertionPosition, smartPrepend(insertionPosition,insertionPosition, listOfZonesForVehicle))
              }else {
                insertMovedZonesWithin(zonesToInsert, insertionPosition, listOfZonesForVehicle)
              }
            }else{
              //startZone < insertionPosition
              //overlap
              smartPrepend(startZone, insertionPosition-1, insertMovedZonesWithin(zonesToInsert, insertionPosition, (insertionPosition,endZone) :: tail))
            }
        }
      }

      def insertMovedZonesWithin(zonesToInsert : List[(Int, Int)], insertionOffset : Int, toReinsertAndShiftAfter : List[(Int, Int)]) : List[(Int, Int)] = {
        zonesToInsert match {
          case Nil => shiftPlusDelta(toReinsertAndShiftAfter, nbPointsInMovedSegment)
          case (start, end) :: tail =>
            smartPrepend(start + insertionOffset, end + insertionOffset, insertMovedZonesWithin(tail, insertionOffset, toReinsertAndShiftAfter))
        }
      }

      //inserting the sequence into the list of zones to update
      //also need to insert a single zone at the end, and perform a relative shift of the sequence.
      val zonesToUpdateTo = zonesToUpdateWithVehicleFromUpdated.getOrElse(destinationVehicle, Nil)
      val updatedZonesTo = insertMovedZones(zonesToUpdateTo, relativeZonesToInsert)
      zonesToUpdateWithVehicleFromUpdated.insert(destinationVehicle, updatedZonesTo)
    }
  }


  //this is a debug procedure
  def checkZonesToUpdate(zonesToUpdate: RedBlackTreeMap[List[(Int, Int)]],
                         sequenceAfterMovesBeforeUpdates:IntSequence): Unit ={

    for (vehicle <- zonesToUpdate.keys) {
      val theZone = zonesToUpdate.get(vehicle).get
      //println("toUpodateZone vehicle:" + vehicle + "  zone:" + theZone)
      require(0 <= vehicle)
      require(vehicle < v)

      val startPosOfVehicle = sequenceAfterMovesBeforeUpdates.positionOfAnyOccurrence(vehicle).get
      val lastPointPosOfVehicle = if (vehicle == v - 1) sequenceAfterMovesBeforeUpdates.size - 1 else sequenceAfterMovesBeforeUpdates.positionOfAnyOccurrence(vehicle + 1).get - 1
      val vehicleRouteLength = lastPointPosOfVehicle - startPosOfVehicle

      if(theZone.nonEmpty) {
        val (start, end) = checkZone(theZone)

        require(start >= 0)
        require(end <= vehicleRouteLength,
          s"end:$end vehicleRouteLength:$vehicleRouteLength theZone:$theZone vehicle:$vehicle routes:$sequenceAfterMovesBeforeUpdates")
      }
    }

    def checkZone(toUpdateZone : List[(Int, Int)]) : (Int, Int) = {
      toUpdateZone match {
        case List((a, b)) =>
          require(a <= b)
          (a, b)
        case (a, b) :: tail =>
          require(a <= b)
          (a, checkZoneLoop(b, tail))
      }
    }

    @tailrec
    def checkZoneLoop(endOfPrev : Int, toUpdateZone : List[(Int, Int)]) : Int = {
      toUpdateZone match {
        case List((a, b)) =>
          require(a <= b)
          require(endOfPrev + 1 < a)
          b
        case (a, b) :: tail =>
          require(a <= b)
          require(endOfPrev + 1 < a)
          checkZoneLoop(b, tail)
      }
    }
  }

  def setNodesUnrouted(unroutedNodes:Iterable[Int]): Unit

  /**
    *
    * @param prevNode
    * @param node
    * @return true if changed, false otherwise
    */
  def setVehicleContentAtNode(prevNode:Int, node: Int):Boolean

  def setVehicleContentAtEnd(vehicle:Int, lastNode:Int): Unit

  /**
    *
    * @param vehicle
    * @return true if changed, false otherwise
    */
  def setVehicleContentAtStart(vehicle:Int):Boolean

  /**
    * updates the output, based on the zones to updata, for all vehicles
    * @param s
    * @param vehiclesToZonesToUpdate
    * @param vehicleLocationInSequence
    */
  protected def updateVehicleContentOnAllVehicle(s:IntSequence,
                                                 vehiclesToZonesToUpdate: RedBlackTreeMap[List[(Int, Int)]],
                                                 vehicleLocationInSequence:VehicleLocation): Unit ={

    var tmpExplorer:Option[IntSequenceExplorer] = None

    //println("updateVehicleContentOnAllVehicle")
    //println("vehiclesToZonesToUpdate:" + vehiclesToZonesToUpdate.content)

    for((vehicle,sortedZonesToUpdateRelativeToVehicleStartPosition) <- vehiclesToZonesToUpdate.content){
      tmpExplorer = updateVehicleContent(s,
        sortedZonesToUpdateRelativeToVehicleStartPosition,
        vehicleLocationInSequence.startPosOfVehicle(vehicle),
        vehicle,
        tmpExplorer)

      tmpExplorer match{
        case Some(e) if e.position+1 == s.size || (vehicle+1 < v && e.position + 1 == vehicleLocationInSequence.startPosOfVehicle(vehicle+1)) =>
          setVehicleContentAtEnd(vehicle,e.value)
        case null =>
          require(v-1 == vehicle)
          setVehicleContentAtEnd(vehicle,s.last)
        case _ => ;
          //we did not reach th end of the vehicle route in the update, yet the last node might have hanged, so we have to update this
          val positionOfEndNodeOfVehicle = if(vehicle == v-1) s.size-1 else vehicleLocationInSequence.startPosOfVehicle(vehicle+1) -1
          val explorerAtEndNodeOfVehicleOpt = s.explorerAtPosition(positionOfEndNodeOfVehicle)
          val explorerAtEndNodeOfVehicle = explorerAtEndNodeOfVehicleOpt.get
          setVehicleContentAtEnd(vehicle,explorerAtEndNodeOfVehicle.value)
          tmpExplorer = explorerAtEndNodeOfVehicleOpt
      }
    }
  }

  /**
    * performs the update of vehicle content on the mentioned vehicle
    * beware, this does not update the contentAtVehicleEnd; you must do it somewhere else.
 *
    * @param s the sequence
    * @param sortedZonesToUpdateRelativeToVehicleStartPosition a sorted lsit of non-overlapping intervals wit hthe relative positions to update
    * @param startPositionOfVehicle the start position o he vehicle to update
    * @param vehicle the vehicle to update
    * @param explorerToLatestUpdatedPosition an explorer to the latest position that was updated
    * @return the last position that was updated
   */
  @tailrec
  private def updateVehicleContent(s:IntSequence,
                                   sortedZonesToUpdateRelativeToVehicleStartPosition: List[(Int, Int)],
                                   startPositionOfVehicle:Int,
                                   vehicle:Int,
                                   explorerToLatestUpdatedPosition:Option[IntSequenceExplorer] = None):Option[IntSequenceExplorer] = {

    //println("updateVehicleContent")
    //println("s:" + s)
    //println("vehicle:" + vehicle)
    //println("sortedZonesToUpdateRelativeToVehicleStartPosition:" + sortedZonesToUpdateRelativeToVehicleStartPosition)

    sortedZonesToUpdateRelativeToVehicleStartPosition match {
      case Nil => explorerToLatestUpdatedPosition
      case (startCompulsoryRelative, endCompulsoryRelative) :: tail =>
        val startCompulsoryAbsolute = startCompulsoryRelative + startPositionOfVehicle
        val endCompulsoryAbsolute = endCompulsoryRelative + startPositionOfVehicle

        //println("startPositionOfVehicle:" + startPositionOfVehicle)
        //println("startCompulsoryAbsolute:" + startCompulsoryAbsolute)
        val positionToStartFromAbsolute = explorerToLatestUpdatedPosition match {
          case Some(e) if e.position > startCompulsoryAbsolute => e.position
          case _ => startCompulsoryAbsolute
        }

        //println("positionToStartFromAbsolute:" + positionToStartFromAbsolute)

        val explorerAfterUpdatingThisIntervalOpt =
          if (positionToStartFromAbsolute > endCompulsoryAbsolute) {
            explorerToLatestUpdatedPosition
          } else {
            val explorerToStartUpdate = explorerToLatestUpdatedPosition match {
              case Some(e) if e.position == startCompulsoryAbsolute => e
              case Some(e) if e.position == startCompulsoryAbsolute - 1 => e.next.get
              case _ => s.explorerAtPosition(positionToStartFromAbsolute) match{
                case Some(e) => e
                case None => return null //we need to start past end of last vehicle. It means that the end node of the last vehicle was removed.
              }
            }

            if (positionToStartFromAbsolute == startPositionOfVehicle) {
              //we need to update the value at vehicle start
              val changedStart = setVehicleContentAtStart(vehicle)
              if(changedStart || endCompulsoryAbsolute > 0) {
                //start iterate from here
                updateUntilAbsolutePositionAndSaturatedOrVehicleEnd(explorerToStartUpdate,
                  endCompulsoryAbsolute, vehicle)
              }else {
                //we do not need to iterate on this because there was no change at this point, and we already reached the end of the compulsory zone
                Some(explorerToStartUpdate)
              }
            } else {
              //we start later than vehicle start
              //so we need to fetch the value at the previous node

              val explorerAtPrev = explorerToStartUpdate.prev.get  //TODO: error here when positionToStartFromAbsolute == 0
              updateUntilAbsolutePositionAndSaturatedOrVehicleEnd(explorerAtPrev,
                endCompulsoryAbsolute, vehicle)
            }
          }
        //and carry on to next interval
        updateVehicleContent(s,
          tail,
          startPositionOfVehicle,
          vehicle,
          explorerAfterUpdatingThisIntervalOpt)
    }
  }

  /**
    * @param previousUpdatedPosition
    * @param endCompulsoryAbsolute
    * @param vehicle the vehicle to which this belongs
    * @return the last position where an update was performed
    */
  @tailrec
  private def updateUntilAbsolutePositionAndSaturatedOrVehicleEnd(previousUpdatedPosition:IntSequenceExplorer,
                                                                  endCompulsoryAbsolute:Int,
                                                                  vehicle:Int):Option[IntSequenceExplorer] = {
    previousUpdatedPosition.next match{
      case None => None //we'v reached the end of the sequence
      case Some(positionOfCurrent) =>
        val currentNode = positionOfCurrent.value
        if(currentNode < v){
          //we'v just reached another vehicle start
          Some(previousUpdatedPosition)
        }else {
          //(startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode

          val changedAtNode = setVehicleContentAtNode(previousUpdatedPosition.value, currentNode)
          if (changedAtNode || positionOfCurrent.position < endCompulsoryAbsolute) {
            //changed or in the compulsory zone
            updateUntilAbsolutePositionAndSaturatedOrVehicleEnd(
              positionOfCurrent,
              endCompulsoryAbsolute, vehicle)
          } else {
            //there is no update, and we are not in the compulsory zone anymore, so we can stop, and return the position
            Some(positionOfCurrent)
          }
        }
    }
  }

  /**
    *Computes content of vehicle and their starting position from scratch
    * @param s the sequence
    * @return (VehicleLocation)
    */
  def computeAndAffectContentAndVehicleStartPositionsFromScratch(s:IntSequence,unrouteAllNodes:Boolean):ConcreteVehicleLocation = {
    val vehicleLocation = Array.fill(v)(0)

    if(unrouteAllNodes) setNodesUnrouted((v until n))

    var previousPosition = s.explorerAtPosition(0).get
    var currentVehicle = 0
    setVehicleContentAtStart(0)

    while(true) {
      previousPosition.next match {
        case None => //we'v reached the end of the sequence
          setVehicleContentAtEnd(currentVehicle,previousPosition.value)
          require(currentVehicle == v-1)
          return VehicleLocation(vehicleLocation)
        case Some(currentPosition) =>
          val currentNode = currentPosition.value
          if (currentNode < v) {
            //we'v reached a new vehicle
            setVehicleContentAtEnd(currentVehicle,previousPosition.value)
            vehicleLocation(currentNode) = currentPosition.position
            setVehicleContentAtStart(currentNode)
            previousPosition = currentPosition
            currentVehicle = currentNode
          } else {
            //carry on the same vehicle
            //(startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
            setVehicleContentAtNode(previousPosition.value,currentNode)
            previousPosition = currentPosition
          }
      }
    }
    throw new Error("should not happen")
  }
}

object AbstractVehicleCapacity{
  /**
    *Computes content of vehicle and their starting position from scratch
    * @param s the sequence
    * @return (nodeToContent,vehicleToContentAtEnd,vehicleLocation)
    */
  def computeNodeToContentAndVehicleContentAtEndAndVehicleStartPositionsFromScratch[T]
  (n:Int,
   v:Int,
   op:(Int,Int,T) => T,  //fomNode,toNode,contentAtFomNode,contentAtToNode
   getContentAtVehicleStart:Int=>T,
   s:IntSequence,
   defaultVehicleContentForUnroutedNodes:T)
  (implicit X: Manifest[T]):(Array[T],Array[T],ConcreteVehicleLocation) = {

    val vehicleLocation = Array.fill(v)(0)
    val vehicleContent = Array.fill[T](n)(defaultVehicleContentForUnroutedNodes)
    val vehicleContentAtEndOfRoute = Array.fill[T](v)(defaultVehicleContentForUnroutedNodes)

    var previousPosition = s.explorerAtPosition(0).get
    var currentVehicle = 0
    var previousContent = getContentAtVehicleStart(0)

    vehicleContent(0) = previousContent

    while(true) {
      previousPosition.next match {
        case None => //we'v reached the end of the sequence
          vehicleContentAtEndOfRoute(currentVehicle) = op(previousPosition.value, currentVehicle, previousContent)
          require(currentVehicle == v-1)
          return (vehicleContent,vehicleContentAtEndOfRoute,VehicleLocation(vehicleLocation))
        case Some(currentPosition) =>
          val currentNode = currentPosition.value
          if (currentNode < v) {
            //we'v reached a new vehicle
            vehicleContentAtEndOfRoute(currentVehicle) = op(previousPosition.value, currentVehicle, previousContent)
            vehicleLocation(currentNode) = currentPosition.position
            previousContent = getContentAtVehicleStart(currentNode)
            vehicleContent(currentNode) = previousContent
            previousPosition = currentPosition
            currentVehicle = currentNode
          } else {
            //carry on the same vehicle
            //(startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
            previousContent = op(previousPosition.value, currentNode, previousContent)
            vehicleContent(currentNode) = previousContent
            previousPosition = currentPosition
          }
      }
    }
    throw new Error("should not happen")
  }
}
