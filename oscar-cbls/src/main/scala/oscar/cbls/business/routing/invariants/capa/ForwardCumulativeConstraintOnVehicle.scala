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

import oscar.cbls._
import oscar.cbls.algo.magicArray.MagicIntArrayStacked
import oscar.cbls.algo.rb.RedBlackTreeMap
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.model.VehicleLocation
import oscar.cbls.core.computation.{CBLSIntVar, ChangingIntValue, ChangingSeqValue, SeqCheckpointedValueStack, SeqNotificationTarget, SeqUpdate, SeqUpdateAssign, SeqUpdateDefineCheckpoint, SeqUpdateInsert, SeqUpdateLastNotified, SeqUpdateMove, SeqUpdateRemove, SeqUpdateRollBackToCheckpoint}
import oscar.cbls.core.propagation.Checker

object ForwardCumulativeConstraintOnVehicle {
  /**
   * the violation maintained by this invariant is the sum over all routed nodes of the overshoot
   * strictly above cMax and the undershoot strictly below 0L of the content of the vehicles
   * @param routes
   * @param n The maximum number of nodes
   * @param v The number of vehicles
   * @param op A function which describes the capacity between two nodes : (startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
   * @param cMax the maximal capacity of all vehicles (it is shared among all vehicles, so if you do not like it, you can use contentAtVehicleStart to make up for this)
   * @param contentAtVehicleStart the content of the vehicle at its start point
   * @param maxCheckpointLevel the maximal level of checkpoints that this should support.
   *                           it consumes O(n) memory per level, so do not overdrive uselessly
   */
  def apply (routes:ChangingSeqValue,
             n:Int,
             v:Int,
             op :(Int,Int,Long)=>Long,
             cMax:Long,
             contentAtVehicleStart:Array[Long],
             maxCheckpointLevel:Int,
             capacityName:String):ChangingIntValue = {

    val violation = new CBLSIntVar(routes.model,0L,(0L,Long.MaxValue), s"violation of capacity $capacityName")

    new ForwardCumulativeConstraintOnVehicle(
      routes,
      n,
      v,
      op,
      cMax,
      contentAtVehicleStart,
      violation,
      maxCheckpointLevel,
      capacityName)

    violation
  }
}

/**
 * the violation maintained by this invariant is the sum over all routed nodes of the overshoot
 * strictly above cMax and the undershoot strictly below 0L of the content of the vehicles
 * @param routes
 * @param n The maximum number of nodes
 * @param v The number of vehicles
 * @param op A function which describes the capacity between two nodes : (startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
 * @param cMax the maximal capacity of all vehicles (it is shared among all vehicles, so if you do not like it, you can use contentAtVehicleStart to make up for this)
 * @param contentAtVehicleStart the content of the vehicle at its start point
 * @param violation the violation that will be controlled by the invariant
 * @param maxCheckpointLevel the maximal level of checkpoints that this should support.
 *                           it consumes O(n) memory per level, so do not overdrive uselessly
 */
class ForwardCumulativeConstraintOnVehicle(routes:ChangingSeqValue,
                                           n:Int,
                                           v:Int,
                                           op :(Int,Int,Long)=>Long,
                                           cMax:Long,
                                           contentAtVehicleStart:Array[Long],
                                           val violation:CBLSIntVar,
                                           maxCheckpointLevel:Int,
                                           capacityName:String = "capacity",
                                           fullDebug:Boolean = false)
  extends AbstractVehicleCapacity(n,v) with SeqNotificationTarget {

  require(contentAtVehicleStart.length==v)
  require(cMax >=0L,"cMax should be >=0L")
  require(contentAtVehicleStart.forall(_ <= cMax),"cannot exceed cMax in initial values")

  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  violation.setDefiningInvariant(this)

  private val contentAtNode = new MagicIntArrayStacked(maxCheckpointLevel, _ => 0L, n)
  private val violationAndVehicleStartStack = new SeqCheckpointedValueStack[(Long,VehicleLocation)]()

  def contentAtNodes:Array[Long] = Array.tabulate(n)(contentAtNode(_))
  def freeSpaceAtNodes: Int => Long = {
    val contentAtNodesNow = contentAtNodes
    (node:Int) => cMax - contentAtNodesNow(node)
  }

  violation := 0L
  for(vehicle <- 0 until v){
    contentAtNode(vehicle) = contentAtVehicleStart(vehicle)
    violation :+= (contentToViolation(contentAtNode(vehicle)) - contentToViolation(0))
  }

  //this also sets the violation, supposing it is at zero before,
  // and it reads content at the start of vehicle  at start, so it must be up to date as well
  private var currentVehicleLocation:VehicleLocation =
    computeAndAffectContentAndVehicleStartPositionsFromScratch(routes.value,false)

  override def setVehicleContentAtNode(prevNode:Int, node: Int):Boolean = {
    val oldContent = contentAtNode(node)
    val newContentAtNode = op(prevNode,node,contentAtNode(prevNode))

    if(newContentAtNode == oldContent) return false
    contentAtNode(node) = newContentAtNode
    violation :+= (contentToViolation(newContentAtNode) - contentToViolation(oldContent))
    true
  }

  override def setNodesUnrouted(unroutedNodes : Iterable[Int]): Unit ={
    for(node <- unroutedNodes){
      violation :-= contentToViolation(contentAtNode(node))
      contentAtNode(node) = 0L
    }
  }

  override def setVehicleContentAtEnd(vehicle : Int, lastNode : Int): Unit = {}

  /**
   * @param vehicle
   * @return true if changed, false otherwise
   */
  override def setVehicleContentAtStart(vehicle : Int) : Boolean = {
    //never changes, so no actual update.
    assert(contentAtNode(vehicle) == contentAtVehicleStart(vehicle))
    false
  }

  @inline
  private def contentToViolation(content:Long):Long = {
    if (content < 0L) - content
    else {
      val overshoot = content - cMax
      if (overshoot > 0L) overshoot
      else 0L
    }
  }

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    val (toUpdateZonesAndVehicleStartAfter,potentiallyRemovedNodes) =
      digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(changes,Some(RedBlackTreeMap.empty[List[(Int,Int)]],currentVehicleLocation),List.empty,v.value)


    setNodesUnrouted(potentiallyRemovedNodes)

    toUpdateZonesAndVehicleStartAfter match{
      case Some((vehiclesToZonesToUpdate,vehicleLocation)) =>
        if(fullDebug) {checkZonesToUpdate(vehiclesToZonesToUpdate, changes.newValue)}
        updateVehicleContentOnAllVehicle(changes.newValue,
          vehiclesToZonesToUpdate,
          vehicleLocation)
        currentVehicleLocation = vehicleLocation
      case None =>
        currentVehicleLocation = computeAndAffectContentAndVehicleStartPositionsFromScratch(changes.newValue,false)
    }
  }

  def digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(changes:SeqUpdate,
                                                                       toUpdateZonesAndVehiceStartOpt:Option[(RedBlackTreeMap[List[(Int,Int)]],VehicleLocation)],
                                                                       potentiallyRemovedPoints:List[Int],
                                                                       previousSequence:IntSequence)
  :(Option[(RedBlackTreeMap[List[(Int,Int)]],VehicleLocation)],List[Int]) = {

    if(fullDebug) {
      toUpdateZonesAndVehiceStartOpt match {
        case Some((list, _)) =>
          checkZonesToUpdate(list, previousSequence : IntSequence)
        case _ => ;
      }
    }

    changes match {
      case s@SeqUpdateInsert(value : Int, posOfInsert : Int, prev : SeqUpdate) =>
        digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev, toUpdateZonesAndVehiceStartOpt, potentiallyRemovedPoints, previousSequence) match {
          case (Some((zonesAfterPrev, vehicleLocationAfterPrev)), potentiallyRemovedPointsAfterPrev) =>
            val vehicleLocationAfterInsert = vehicleLocationAfterPrev.push(s.oldPosToNewPos)
            val updatedZones =
              updateZoneToUpdateAfterInsert(
                zonesAfterPrev,
                posOfInsert,
                prev.newValue,
                vehicleLocationAfterPrev,
                vehicleLocationAfterInsert)

            (Some((updatedZones, vehicleLocationAfterInsert)), potentiallyRemovedPointsAfterPrev)
          case (None,potentiallyRemovedPointsAfterPrev) =>
            (None, potentiallyRemovedPointsAfterPrev)
        }

      case r@SeqUpdateRemove(pos : Int, prev : SeqUpdate) =>
        digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev, toUpdateZonesAndVehiceStartOpt, potentiallyRemovedPoints, previousSequence) match {
          case (Some((zonesAfterPrev, vehicleLocationAfterPrev)), potentiallyRemovedPointsAfterPrev) =>
            val updatedZones =
              updateZoneToUpdateAfterRemove(
                zonesAfterPrev,
                pos : Int,
                prev.newValue, vehicleLocationAfterPrev)

            (Some((updatedZones, vehicleLocationAfterPrev.push(r.oldPosToNewPos))), r.removedValue :: potentiallyRemovedPointsAfterPrev)
          case (None,potentiallyRemovedPointsAfterPrev) =>
            (None, r.removedValue :: potentiallyRemovedPointsAfterPrev)
        }

      case m@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev, toUpdateZonesAndVehiceStartOpt, potentiallyRemovedPoints, previousSequence) match {
          case (Some((zonesAfterPrev, vehicleLocationAfterPrev)), potentiallyRemovedPointsAfterPrev) =>
            val vehicleLocationAfterMove = vehicleLocationAfterPrev.push(m.oldPosToNewPos)
            val updatedZones =
              updateZoneToUpdateAfterMove(
                zonesAfterPrev,
                m,
                prev.newValue,
                vehicleLocationAfterPrev,
                vehicleLocationAfterMove)
            (Some((updatedZones, vehicleLocationAfterMove)), potentiallyRemovedPointsAfterPrev)

          case(None,potentiallyRemovedPointsAfterPrev) =>
            (None, potentiallyRemovedPointsAfterPrev)
        }

      case SeqUpdateAssign(value : IntSequence) =>
        (None, potentiallyRemovedPoints ::: previousSequence.unorderedContentNoDuplicate.filter(_>=v) )

      case SeqUpdateLastNotified(value : IntSequence) =>
        (toUpdateZonesAndVehiceStartOpt, potentiallyRemovedPoints)

      case s@SeqUpdateDefineCheckpoint(prev : SeqUpdate, isStarMode:Boolean, checkpointLevel:Int) =>
        if(checkpointLevel < maxCheckpointLevel) {
          digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev, toUpdateZonesAndVehiceStartOpt, potentiallyRemovedPoints, previousSequence) match {
            //checkpoints are managed about the vehicleLocation exclusively
            case (Some((zonesAfterPrev, vehicleLocationAfterPrev)), removedPointsAfterPrev) =>
              while (contentAtNode.level >= checkpointLevel) {
                //we save the changes, so they are comitted to the lower level
                contentAtNode.popLevel(false)
              }

              val fastVehicleLocationAfterPrev = if(checkpointLevel == 0) vehicleLocationAfterPrev.regularize else vehicleLocationAfterPrev

              setNodesUnrouted(removedPointsAfterPrev)
              updateVehicleContentOnAllVehicle(prev.newValue,
                zonesAfterPrev,
                fastVehicleLocationAfterPrev)
              contentAtNode.pushLevel()
              require(contentAtNode.level == checkpointLevel,
                s"contentAtNode.level:${contentAtNode.level} checkpointLevel:$checkpointLevel")

              violationAndVehicleStartStack.defineCheckpoint(prev.newValue, checkpointLevel, (violation.newValue, fastVehicleLocationAfterPrev))
              currentVehicleLocation = fastVehicleLocationAfterPrev

              (Some(RedBlackTreeMap.empty[List[(Int, Int)]], currentVehicleLocation), List.empty)

            case (None, potentiallyRemovedPointsAfterPrev) =>

              while (contentAtNode.level >= checkpointLevel) {
                //we do not save the changes because we cannot perform incremental computation anyway
                contentAtNode.popLevel(true)
              }
              contentAtNode.pushLevel()
              setNodesUnrouted(v until n) //unroutes all nodes
              violation := 0L
              //we have to set all unrouted nodes to unrouted, since we have lost continuity on routes nodes because of the popLevel(true) hereabove
              currentVehicleLocation = computeAndAffectContentAndVehicleStartPositionsFromScratch(routes.value, false)
              violationAndVehicleStartStack.defineCheckpoint(prev.newValue, checkpointLevel, (violation.newValue, currentVehicleLocation))

              (Some(RedBlackTreeMap.empty[List[(Int, Int)]], currentVehicleLocation), List.empty)
          }
        }else{
          //we are above the maxCheckpoint level
          digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev, toUpdateZonesAndVehiceStartOpt, potentiallyRemovedPoints, previousSequence)
        }
      case u@SeqUpdateRollBackToCheckpoint(checkpoint : IntSequence, level:Int) =>
        if(level <= maxCheckpointLevel){

          while (contentAtNode.level >= level) {
            contentAtNode.popLevel(true)
          }
          contentAtNode.pushLevel()
          require(contentAtNode.level == level)

          val (violation,vehicleLocation) = violationAndVehicleStartStack.rollBackAndOutputValue(checkpoint, level)
          currentVehicleLocation = vehicleLocation
          this.violation := violation
          (Some(RedBlackTreeMap.empty[List[(Int, Int)]], currentVehicleLocation), List.empty)
        }else{
          //We are above the max checkpoint level
          digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(u.howToRollBack, toUpdateZonesAndVehiceStartOpt, potentiallyRemovedPoints, previousSequence)
          //we could save and restore the regularized vehicle start, but his is probably not useful
        }
    }
  }

  override def toString : String = {
    s"""ForwardCumulativeConstraintOnVehicle(routes:${routes.name} n:$n v:$v cMax:$cMax capacityName:$capacityName violation:=${violation.value}){
       |${(0 until v).toList.map((vehicle:Int) =>
    {
      val header = s"\tvehicle$vehicle contentAtVehicleStart:${contentAtVehicleStart(vehicle)}\n"
      var explorerOpt = routes.value.explorerAtAnyOccurrence(vehicle).get.next
      var acc:String = ""

      while(explorerOpt match{
        case None => //at end of last vehicle
          false
        case Some(explorer) if explorer.value < v =>
          //reached another vehicle
          false
        case Some(explorer) if explorer.value >= v =>
          val node = explorer.value
          acc += s"\t\tnode:$node\t content:${contentAtNode(node)}${if (contentAtNode(node) > cMax) " violation:" + (contentAtNode(node) - cMax) else ""}\n"
          explorerOpt = explorer.next
          true
      }){}
      header+acc}).mkString("")}""".stripMargin
  }

  override def checkInternals(c: Checker): Unit = {
    val (nodeToContent,_,vehicleStartPos) = AbstractVehicleCapacity.
      computeNodeToContentAndVehicleContentAtEndAndVehicleStartPositionsFromScratch[Long](n,v,op,((i : Int) => this.contentAtVehicleStart(i)),routes.value, 0L)

    for(node <- routes.value){

      if(node < v){
        c.check(nodeToContent(node) equals contentAtVehicleStart(node),Some("error on initial vehicle content on vehicle " + node))
      }

      c.check(nodeToContent(node) equals contentAtNode(node),
        Some("GenericCumulativeConstraint : Error on content at node(" + node + ") at pos : " +
          routes.newValue.positionsOfValue(node)+ " :=" + contentAtNode(node) + " should be :=" + nodeToContent(node) + " route:" + routes.value))
    }
    val computedViolation = nodeToContent.foldLeft(0L)({case (acc,nodeContent) => acc + contentToViolation(nodeContent)})
    c.check(computedViolation == violation.value, Some("GenericCumulativeConstraint : " + violation + " should be :="+computedViolation))
    for(node <- 0 until n){
      if(routes.value.contains(node)){
        c.check(nodeToContent(node) == contentAtNode(node),Some("Error on content of routed node " + node))
      }else{
        c.check(nodeToContent(node) == 0L ,Some("Error on computed content of unrouted node " + node))
        c.check(contentAtNode(node) == 0L ,Some("Error on content of unrouted node " + node + " is " + contentAtNode(node) + " should be 0L"))
      }
    }
    vehicleStartPos.checkOnSequence(routes.value)
  }
}
