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
package oscar.cbls.business.routing.invariants

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.model.RoutingConventionMethods
import oscar.cbls.core.computation.{CBLSIntVar, ChangingSeqValue, Invariant, SeqNotificationTarget, SeqUpdate, SeqUpdateAssign, SeqUpdateDefineCheckpoint, SeqUpdateInsert, SeqUpdateLastNotified, SeqUpdateMove, SeqUpdateRemove, SeqUpdateRollBackToCheckpoint}
import oscar.cbls.core.propagation.Checker

object VehicleOfNodes{

  /**
    * maintains an array mapping each node to the vehicle reaching it.
    * if the node is unrouted, its corresponding vehicle is set to v
    * @param routes a sequence representing all routed
    * @param v the number of vehicles
    * @return an array that mas each node in 0..route.maxValue to the vehicle reaching it
    */
  def apply(routes:ChangingSeqValue,v:Int):Array[CBLSIntVar] = {
    val model = routes.model
    val domain = routes.domain

    val vehicleOrUnroutedOfNode = Array.tabulate(routes.maxValue+1)((node:Int) =>
      CBLSIntVar(model,
        v,
        domain,
        s"vehicle_or_unrouted_of_node_$node"))

    new VehicleOfNodes(routes, v, vehicleOrUnroutedOfNode)

    vehicleOrUnroutedOfNode
  }
}

class VehicleOfNodes(routes:ChangingSeqValue,
                     v:Int,
                     vehicleOrUnroutedOfNode:Array[CBLSIntVar])
  extends Invariant() with SeqNotificationTarget{

  val n = routes.maxValue + 1

  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  vehicleOrUnroutedOfNode.foreach(_.setDefiningInvariant(this))

  private var savedCheckpoint:IntSequence = null
  //TODO: use magic array here
  private val movedNodesSinceCheckpointArray:Array[Boolean] = Array.fill(n)(false)
  private var movedNodesSinceCheckpointList:QList[Int] = null
  private val vehicleOfNodeAtCheckpointForMovedPoints:Array[Int] = Array.fill(n)(0)

  computeAndAffectValueFromScratch(routes.value)

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    if(!digestUpdates(changes)) {
      dropCheckpoint()
      computeAndAffectValueFromScratch(changes.newValue)
    }
  }

  private def digestUpdates(changes:SeqUpdate):Boolean = {
    val newValue = changes.newValue

    changes match {
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        //on which vehicle did we insert?
        if(!digestUpdates(prev)) return false
        val insertedVehicle = RoutingConventionMethods.searchVehicleReachingPosition(pos,newValue,v)
        vehicleOrUnroutedOfNode(value) := insertedVehicle
        recordMovedPoint(value, v)

        true
      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        if(!digestUpdates(prev)) false
        else if(x.isNop) true
        else if(x.isSimpleFlip){
          true
        }else {
          val oldValue = prev.newValue
          val vehicleOfMovedSegment = RoutingConventionMethods.searchVehicleReachingPosition(fromIncluded,oldValue,v)
          assert(vehicleOfMovedSegment == RoutingConventionMethods.searchVehicleReachingPosition(toIncluded,oldValue,v))
          val targetVehicleOfMove = RoutingConventionMethods.searchVehicleReachingPosition(after,oldValue,v)
          if(vehicleOfMovedSegment != targetVehicleOfMove){
            //we moved all the points to another vehicle
            for(movedValue <- x.movedValuesQList) {
              vehicleOrUnroutedOfNode(movedValue) := targetVehicleOfMove
              recordMovedPoint(movedValue, vehicleOfMovedSegment)
            }
          }
          true
        }

      case x@SeqUpdateRemove(position: Int, prev : SeqUpdate) =>
        //on which vehicle did we remove?
        //on which vehicle did we insert?
        if(!digestUpdates(prev)) return false
        val oldValue = prev.newValue
        val impactedVehicle = RoutingConventionMethods.searchVehicleReachingPosition(position,oldValue,v)
        val removedValue = x.removedValue
        vehicleOrUnroutedOfNode(removedValue) := v
        recordMovedPoint(removedValue, impactedVehicle)
        true
      case SeqUpdateAssign(value : IntSequence) =>
        false //impossible to go incremental
      case SeqUpdateLastNotified(value:IntSequence) =>
        true //we are starting from the previous value
      case SeqUpdateDefineCheckpoint(prev,isStarMode,checkpointLevel) =>
        if(checkpointLevel == 0) {
          if (!digestUpdates(prev)) {
            computeAndAffectValueFromScratch(changes.newValue)
          }
          saveCurrentCheckpoint(prev.newValue)
          true
        }else{
          digestUpdates(prev)
        }
      case r@SeqUpdateRollBackToCheckpoint(checkpoint,checkpointLevel) =>
        if(checkpoint == null) false //it has been dropped following a Set
        else {
          if(checkpointLevel == 0) {
            require(checkpoint quickEquals savedCheckpoint)
            restoreCheckpoint()
            true
          }else{
            digestUpdates(r.howToRollBack)
          }
        }
    }
  }

  private def dropCheckpoint(): Unit ={
    saveCurrentCheckpoint(null)
  }

  private def saveCurrentCheckpoint(s:IntSequence): Unit ={
    savedCheckpoint = s
    while (movedNodesSinceCheckpointList!= null) {
      movedNodesSinceCheckpointArray(movedNodesSinceCheckpointList.head) = false
      movedNodesSinceCheckpointList = movedNodesSinceCheckpointList.tail
    }
  }

  private def restoreCheckpoint(): Unit ={
    while (movedNodesSinceCheckpointList!= null) {
      val node= movedNodesSinceCheckpointList.head
      movedNodesSinceCheckpointArray(movedNodesSinceCheckpointList.head) = false
      movedNodesSinceCheckpointList = movedNodesSinceCheckpointList.tail
      vehicleOrUnroutedOfNode(node) := vehicleOfNodeAtCheckpointForMovedPoints(node)
    }
  }

  private def recordMovedPoint(node: Int, oldVehicle:Int): Unit ={
    if(savedCheckpoint!= null) {
      if (!movedNodesSinceCheckpointArray(node)) {
        movedNodesSinceCheckpointList = QList(node, movedNodesSinceCheckpointList)
        movedNodesSinceCheckpointArray(node) = true
        vehicleOfNodeAtCheckpointForMovedPoints(node) = oldVehicle
      }
    }
  }

  private def computeAndAffectValueFromScratch(s:IntSequence): Unit ={
    vehicleOrUnroutedOfNode.foreach(_:=v) //unrouted

    val it = s.iterator
    var currentVehicle:Int = it.next()
    require(currentVehicle == 0)
    vehicleOrUnroutedOfNode(0) := 0

    while(it.hasNext){
      val node = it.next()
      if(node < v){
        //reaching a new vehicle start
        currentVehicle = node
      }
      //continuing on the same vehicle
      vehicleOrUnroutedOfNode(node) := currentVehicle
    }
  }

  private def computeValueFromScratch(s:IntSequence):Array[Int] = {
    val tmpVehicleOrUnroutedOfNode = Array.fill(n)(v)

    val it = s.iterator
    var currentVehicle:Int = it.next()
    require(currentVehicle == 0)
    tmpVehicleOrUnroutedOfNode(0) = 0

    while(it.hasNext){
      val node = it.next()
      if(node < v){
        //reaching a new vehicle start
        require(node == currentVehicle+1)
        currentVehicle = node
      }
      //continuing on the same vehicle
      tmpVehicleOrUnroutedOfNode(node) = currentVehicle
    }
    tmpVehicleOrUnroutedOfNode
  }

  override def checkInternals(c : Checker) : Unit = {
    val values = computeValueFromScratch(routes.value)
    for (node <- 0 until n){
      c.check(vehicleOrUnroutedOfNode(node).value == values(node),
        Some(s"vehicleOrUnroutedOfNode(node).value=${vehicleOrUnroutedOfNode(node).value} should== valuesFromScratch(node)=${values(node)} node:$node"))
    }

    if(savedCheckpoint != null) {
      val vehicleOfNodeFromScratch = computeValueFromScratch(savedCheckpoint)
      for (node <- 0 until n) {
        if(movedNodesSinceCheckpointArray(node)) {
          c.check(vehicleOfNodeFromScratch(node) == vehicleOfNodeAtCheckpointForMovedPoints(node),
            Some(s"vehicleOfNodeAtCheckpointForMovedPoints(node)=${vehicleOfNodeAtCheckpointForMovedPoints(node)} should== vehicleOfNodeFromScratch(node)=${vehicleOfNodeFromScratch(node)}"))
        }
      }
    }
  }
}
