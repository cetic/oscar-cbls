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
import oscar.cbls.core.computation.{CBLSSetVar, ChangingSeqValue, Invariant, SeqNotificationTarget, SeqUpdate, SeqUpdateAssign, SeqUpdateDefineCheckpoint, SeqUpdateInsert, SeqUpdateLastNotified, SeqUpdateMove, SeqUpdateRemove, SeqUpdateRollBackToCheckpoint}
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.SortedSet

object NodesOfVehicle{
  /**
   * this invariant ensures that nodesOfVehicle(p) is maintained to the nodes reached by vehicle p according to the sequence routes.
   * @param routes a sequence value representing routes
   * @param v the number of vehicles
   * @return an array nodesOfVehicle maintained to the nodes reached y each vehicle
   */
  def apply(routes:ChangingSeqValue,v:Int,includeVehicleNode : Boolean = true):Array[CBLSSetVar] = {
    val model = routes.model
    val emptySet = SortedSet.empty[Int]
    val domain = routes.domain

    val nodesOfVehicle = Array.tabulate(v+1)((vehicle:Int) =>
      CBLSSetVar(model,
        emptySet,
        domain,
        if(vehicle== v) "unrouted nodes" else s"nodes_of_vehicle_$vehicle"))

    new NodesOfVehicle(routes, nodesOfVehicle,includeVehicleNode)

    nodesOfVehicle
  }
}

/**
 * this invariant ensures that nodesOfVehicleOrUnrouted(p) is maintained to the ndes reached vy vehicle p according to the sequence routes.p
 * the size of the array is supposed to equal to v
 * @param routes a sequence value representing routes
 * @param nodesOfVehicleOrUnrouted an array of v CBLSSetVar
 */
class NodesOfVehicle(routes:ChangingSeqValue,
  nodesOfVehicleOrUnrouted:Array[CBLSSetVar],
  includeVehicleNode : Boolean = true)  //there is actually one more vehicle, for unrouted nodes.
  extends Invariant() with SeqNotificationTarget{

  val v = nodesOfVehicleOrUnrouted.length-1
  val n = routes.maxValue+1

  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  nodesOfVehicleOrUnrouted.foreach(_.setDefiningInvariant(this))

  private val savedValues:Array[SortedSet[Int]] = null
  private var savedCheckpoint:IntSequence = null
  //TODO: use magic array here
  private val movedNodesSinceCheckpointArray:Array[Boolean] = Array.fill(n)(false)
  private var movedNodesSinceCheckpointList:QList[Int] = null
  private val vehicleOfNodeAtCheckpointForMovedPoints:Array[Int] = Array.fill(n)(0)
  private val vehicleOfNodeAfterMoveForMovedPoints:Array[Int] = Array.fill(n)(0)

  affect(computeValueFromScratch(routes.value))

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    if(!digestUpdates(changes)) {
      dropCheckpoint()
      affect(computeValueFromScratch(changes.newValue))
    }
  }

  private def digestUpdates(changes:SeqUpdate):Boolean = {
    val newValue = changes.newValue

    changes match {
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        //on which vehicle did we insert?
        if(!digestUpdates(prev)) return false
        val insertedVehicle = RoutingConventionMethods.searchVehicleReachingPosition(pos,newValue,v)
        nodesOfVehicleOrUnrouted(insertedVehicle) :+= value
        nodesOfVehicleOrUnrouted(v) :-= value
        recordMovedPoint(value, v, insertedVehicle)

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
          val targetVehicleOfMove = RoutingConventionMethods.searchVehicleReachingPosition(after,oldValue,v)
          if(vehicleOfMovedSegment != targetVehicleOfMove){
            //we moved all the points to another vehicle
            for(movedValue <- x.movedValuesQList) {
              nodesOfVehicleOrUnrouted(vehicleOfMovedSegment) :-= movedValue
              nodesOfVehicleOrUnrouted(targetVehicleOfMove) :+= movedValue
              recordMovedPoint(movedValue, vehicleOfMovedSegment, targetVehicleOfMove)
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
        nodesOfVehicleOrUnrouted(impactedVehicle) :-= removedValue
        nodesOfVehicleOrUnrouted(v) :+= removedValue
        recordMovedPoint(removedValue, impactedVehicle, v)
        true
      case SeqUpdateAssign(value : IntSequence) =>
        false //impossible to go incremental
      case SeqUpdateLastNotified(value:IntSequence) =>
        true //we are starting from the previous value
      case SeqUpdateDefineCheckpoint(prev,isStarMode,checkpointLevel) =>
        if(checkpointLevel == 0) {
          if (!digestUpdates(prev)) {
            affect(computeValueFromScratch(changes.newValue))
          }
          saveCurrentCheckpoint(prev.newValue)
          true
        }else{
          //we do not handle other checkpoint, so ignore declaration
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
      nodesOfVehicleOrUnrouted(vehicleOfNodeAfterMoveForMovedPoints(node)) :-= node
      nodesOfVehicleOrUnrouted(vehicleOfNodeAtCheckpointForMovedPoints(node)) :+= node
    }
  }

  private def recordMovedPoint(node:Int, oldVehicle:Int, newVehicle:Int): Unit ={
    require(oldVehicle != newVehicle)
    if(savedCheckpoint!= null) {
      if (!movedNodesSinceCheckpointArray(node)) {
        movedNodesSinceCheckpointList = QList(node, movedNodesSinceCheckpointList)
        movedNodesSinceCheckpointArray(node) = true
        vehicleOfNodeAtCheckpointForMovedPoints(node) = oldVehicle
      }
      vehicleOfNodeAfterMoveForMovedPoints(node) = newVehicle
    }
  }

  private def affect(value:Array[SortedSet[Int]]): Unit ={
    var currentV = 0
    while(currentV <= v){
      nodesOfVehicleOrUnrouted(currentV) := value(currentV)
      currentV += 1
    }
  }

  private def computeValueFromScratch(s:IntSequence):Array[SortedSet[Int]] = {
    val toReturn = Array.fill(v+1)(SortedSet.empty[Int])
    toReturn(v) = toReturn(v) ++ (v until n)
    val it = s.iterator
    var currentVehicle:Int = it.next()
    require(currentVehicle == 0)
    if (includeVehicleNode) {
      toReturn(0) = toReturn(0) + (0)
    }

    while(it.hasNext){
      val node = it.next()
      if(node < v){
        //reaching a new vehicle start
        currentVehicle = node
      }
      //continuing on the same vehicle
      if (node >= v || includeVehicleNode) {
        toReturn(currentVehicle) = toReturn(currentVehicle) + node
      }
      toReturn(v) = toReturn(v) - node
    }
    toReturn
  }

  override def checkInternals(c : Checker) : Unit = {
    val values = computeValueFromScratch(routes.value)
    for (vehicle <- 0 to v){
      require(nodesOfVehicleOrUnrouted(vehicle).value equals values(vehicle), Some("error on vehicle " + v + " output-correct:" + (nodesOfVehicleOrUnrouted(vehicle).value.diff(values(vehicle))) + " correct-output:" + (values(vehicle).diff(nodesOfVehicleOrUnrouted(vehicle).value))))
    }

    if(savedCheckpoint != null) {
      val nodesOfVehicleFromScratch = computeValueFromScratch(savedCheckpoint)
      for (node <- 0 until n) {
        if(movedNodesSinceCheckpointArray(node))
          require(nodesOfVehicleFromScratch(vehicleOfNodeAtCheckpointForMovedPoints(node)).contains(node))
      }
    }
  }
}
