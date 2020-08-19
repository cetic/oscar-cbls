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

object RouteLength {

  /**
   *
   * @param routes
   * @param v
   * @param perVehicle
   * @param distanceMatrix
   * @param distanceIsSymmetric
   * @return
   */
  def apply(routes : ChangingSeqValue,
            n:Int,
            v : Int,
            perVehicle:Boolean,
            distanceMatrix : Array[Array[Long]],
            distanceIsSymmetric : Boolean):Array[CBLSIntVar] = {

    val distance:Array[CBLSIntVar] =
      if(perVehicle) Array.tabulate(v)(v => CBLSIntVar(routes.model,name="distanceOfVehicle" + v))
      else Array.fill(1)(CBLSIntVar(routes.model,name="overallDistance"))

      new RouteLength(routes,
        n,
        v,
        distanceMatrix,
        distance,
        distanceIsSymmetric)

    distance
  }

  def isDistanceSymmetricArray(distanceMatrix : Array[Array[Long]]):Boolean = {
    val n = distanceMatrix.length
    var i = 0
    while(i < n){
      var j = 0
      while(j <= i){
        if (distanceMatrix(i)(j) != distanceMatrix(j)(i))
          return false
        j += 1
      }
      i += 1
    }
    true
  }

  def isDistanceSymmetric(distanceMatrix : Array[Array[Long]],n:Int):Boolean = {
    var i = 0
    while(i < n){
      var j = 0
      while(j <= i){
        if (distanceMatrix(i)(j) != distanceMatrix(j)(i))
          return false
        j += 1
      }
      i += 1
    }
    true
  }
}

/**
 * @param routes the routes of all the vehicles
 * @param v the number of vehicles in the model
 * @param distanceMatrix the matrix of distance, which is expected to be symmetric
 * @param distance it is either an array of one value, in which case it is the total distance run by all vehicle
 *                 or it can also be an array of size v. in this case it is the distance run by each vehicle, respectively.
 *                 the second option is computationally more expensive
 * @param distanceIsSymmetric true if you swear that the distance matrix is symmetric, false and it will be considered as asymmetric (slower!)
 *
 * The distance computed by this invariant considers the values o the diagonal as part of the cost (node cost are added to the distance)
 *
 * This invariant relies on the vehicle model assumption:
 * there are v vehicles
 * They are supposed to start from point of values 0 to v-1
 * These values must always be present in the sequence in increasing order
 * they cannot be included within a moved segment
 */
class RouteLength(routes:ChangingSeqValue,
                  n:Int,
                  v:Int,
                  distanceMatrix:Array[Array[Long]],
                  distance:Array[CBLSIntVar],
                  distanceIsSymmetric:Boolean,
                  nodeToMatrixID:Int=>Int = x => x)
  extends Invariant() with SeqNotificationTarget{

  protected def distanceMatrixOnNode(from:Int)(to:Int):Long = {
    distanceMatrix(nodeToMatrixID(from))(nodeToMatrixID(to))
  }

  val perVehicle:Boolean = distance.length >1
  require(distance.length == 1 || distance.length == v)

  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  for(i <- distance) i.setDefiningInvariant(this)

  //TODO: handle inactive checkpoints
  private val savedValues:Array[Long] = computeValueFromScratch(routes.value)
  protected var checkpoint = routes.value

  protected[this] val isVehicleChangedSinceCheckpoint:Array[Boolean] = Array.fill(v)(false)
  protected var changedVehiclesSinceCheckpoint:QList[Int] = null

  //only one level of stack for checkpoint here.

  protected var vehicleSearcher:((IntSequence,Int)=>Int) = if(v == 1) ((_,_) => 0) else
    RoutingConventionMethods.cachedVehicleReachingPosition(routes.value, v)

  affect(savedValues)

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit ={
    if(!digestUpdates(changes)) {
      for(v <- 0 until this.v) recordTouchedVehicle(v)
      affect(computeValueFromScratch(changes.newValue))
    }
  }

  private def digestUpdates(changes:SeqUpdate):Boolean = {
    changes match {
      case SeqUpdateDefineCheckpoint(prev,isStarMode,checkpointLevel) =>
        //we only consider level 0; other are not managed.
        if(checkpointLevel == 0) {

          if (!digestUpdates(prev)) {
            affect(computeValueFromScratch(changes.newValue))
          }
          saveCurrentCheckpoint(changes.newValue)
          true
        }else{
          //ignore it altogether
          digestUpdates(prev)
        }

      case r@SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence,checkpointLevel:Int) =>
        if(checkpointLevel == 0) {
          require(checkpoint quickEquals this.checkpoint)
          restoreCheckpoint()
          true
        }else{
          digestUpdates(r.howToRollBack)
        }

      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if(!digestUpdates(prev)) return false

        val newSeq = changes.newValue

        val oldPrev = prev.newValue.valueAtPosition(pos-1).get

        val oldSucc =prev.newValue.valueAtPosition(pos) match{
          case None => v-1 //at the end
          case Some(oldSuccIfNoLoop) =>  if(oldSuccIfNoLoop < v) oldSuccIfNoLoop-1 else oldSuccIfNoLoop
        }

        val oldDistance = distanceMatrixOnNode(oldPrev)(oldSucc)
        val newDistance = distanceMatrixOnNode(oldPrev)(value) + distanceMatrixOnNode(value)(oldSucc)
        val nodeCost = distanceMatrixOnNode(value)(value)

        if(perVehicle) {
          val vehicle = vehicleSearcher(newSeq, pos)
          recordTouchedVehicle(vehicle)
          distance(vehicle) :+= (newDistance + nodeCost - oldDistance)
        }else{
          distance(0) :+= (newDistance + nodeCost - oldDistance)
        }
        true

      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        if(!digestUpdates(prev)) false
        else if(x.isNop) true
        else if(x.isSimpleFlip){
          //this is a simple flip

          val oldPrevFromValue = prev.newValue.valueAtPosition(fromIncluded - 1).get
          val oldSuccToValue = RoutingConventionMethods.routingSuccPos2Val(toIncluded,prev.newValue,v)

          val fromValue = x.fromValue
          val toValue = x.toValue

          val oldHopBeforeMovedSegment = distanceMatrixOnNode(oldPrevFromValue)(fromValue)
          val oldHopAfterMovedSegment = distanceMatrixOnNode(toValue)(oldSuccToValue)
          val newHopBeforeMovedSegment = distanceMatrixOnNode(oldPrevFromValue)(toValue)
          val newHopAfterMovedSegment = distanceMatrixOnNode(fromValue)(oldSuccToValue)

          //for simple flip, there is no node cost to consider
          if(perVehicle) {
            val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)
            val deltaDistance = if(distanceIsSymmetric) 0L else {
              //there is a flip and distance is asymmetric
              computeValueBetween(prev.newValue,
                vehicleOfMovedSegment,
                toIncluded, toValue,
                fromIncluded, fromValue) -
                computeValueBetween(prev.newValue,
                  vehicleOfMovedSegment,
                  fromIncluded, fromValue,
                  toIncluded, toValue)
            }
            recordTouchedVehicle(vehicleOfMovedSegment)
            distance(vehicleOfMovedSegment) :+= (newHopBeforeMovedSegment + newHopAfterMovedSegment
              - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment) + deltaDistance)
          }else{//not per vehicle
          val deltaDistance = if(distanceIsSymmetric) 0L else {
              val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)
              //there is a flip and distance is asymmetric
              computeValueBetween(prev.newValue,
                vehicleOfMovedSegment,
                toIncluded, toValue,
                fromIncluded, fromValue) -
                computeValueBetween(prev.newValue,
                  vehicleOfMovedSegment,
                  fromIncluded, fromValue,
                  toIncluded, toValue)
            }
            distance(0) :+= (newHopBeforeMovedSegment + newHopAfterMovedSegment
              - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment) + deltaDistance)
          }
          true
        }else {
          //actually moving, not simple flip
          val oldPrevFromValue = prev.newValue.valueAtPosition(fromIncluded - 1).get
          val oldSuccToIfNoLoopOpt = prev.newValue.valueAtPosition(toIncluded + 1)
          val oldSuccToValue = oldSuccToIfNoLoopOpt match {
            case None => v - 1
            case Some(value) => if (value < v) value - 1 else value
          }

          val fromValue = x.fromValue
          val toValue = x.toValue
          val afterValue = x.afterValue

          val oldSuccAfterValue = RoutingConventionMethods.routingSuccPos2Val(after, prev.newValue, v)

          val oldHopBeforeMovedSegment = distanceMatrixOnNode(oldPrevFromValue)(fromValue)
          val oldHopAfterMovedSegment = distanceMatrixOnNode(toValue)(oldSuccToValue)
          val oldHopAfterAfter = distanceMatrixOnNode(afterValue)(oldSuccAfterValue)

          val newHopBeforeMovedSegment = distanceMatrixOnNode(afterValue)(if(flip) toValue else fromValue)
          val newHopAfterMovedSegment = distanceMatrixOnNode(if(flip) fromValue else toValue)(oldSuccAfterValue)
          val newHopReplacingSegment = distanceMatrixOnNode(oldPrevFromValue)(oldSuccToValue)

          if(!perVehicle){
            //not per vehicle, so no node cost to consider
            val deltaDistance = if(distanceIsSymmetric || !flip) 0L //no delta on distance
            else { //there is a flip and distance is asymmetric
            val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)
              computeValueBetween(prev.newValue,
                vehicleOfMovedSegment,
                toIncluded, toValue,
                fromIncluded, fromValue) -
                computeValueBetween(prev.newValue,
                  vehicleOfMovedSegment,
                  fromIncluded, fromValue,
                  toIncluded, toValue)

            }
            distance(0) :+= (
              newHopReplacingSegment + newHopBeforeMovedSegment + newHopAfterMovedSegment
                - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment + oldHopAfterAfter) + deltaDistance)

          }else {
            //per vehicle, there might be some node cost to consider
            val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)
            val targetVehicleOfMove = vehicleSearcher(prev.newValue, after)
            assert(vehicleOfMovedSegment == vehicleSearcher(prev.newValue,toIncluded))

            if (vehicleOfMovedSegment == targetVehicleOfMove) {
              //the segment is moved to the same vehicle, so we do not consider node cost here

              val (deltaDistance) = if(distanceIsSymmetric || !flip) 0L else {
                //there is a flip and distance is asymmetric
                computeValueBetween(prev.newValue,
                  vehicleOfMovedSegment,
                  toIncluded, toValue,
                  fromIncluded, fromValue) -
                  computeValueBetween(prev.newValue,
                    vehicleOfMovedSegment,
                    fromIncluded, fromValue,
                    toIncluded, toValue)
              }

              recordTouchedVehicle(vehicleOfMovedSegment)
              distance(vehicleOfMovedSegment) :+= (
                newHopReplacingSegment + newHopBeforeMovedSegment + newHopAfterMovedSegment
                  - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment + oldHopAfterAfter) + deltaDistance)

            } else {
              //moving a segment to another vehicle, and per vehicle required.

              //summing the moved segment (this is slow, but it is requested to compute the cost per vehicle)
              val oldCostInSegment = computeValueBetween(prev.newValue,
                vehicleOfMovedSegment,
                fromIncluded, fromValue,
                toIncluded,toValue)
              val newCostInSegment = if(distanceIsSymmetric || !flip) oldCostInSegment else{
                //there is a flip and distance is asymmetric
                computeValueBetween(prev.newValue,
                  vehicleOfMovedSegment,
                  toIncluded,toValue,
                  fromIncluded, fromValue)
              }

              recordTouchedVehicle(vehicleOfMovedSegment)
              distance(vehicleOfMovedSegment) :+= (
                newHopReplacingSegment - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment + oldCostInSegment))

              recordTouchedVehicle(targetVehicleOfMove)
              distance(targetVehicleOfMove) :+= (
                newHopBeforeMovedSegment + newCostInSegment + newHopAfterMovedSegment - oldHopAfterAfter)
            }
          }
          true
        }

      case x@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        //on which vehicle did we remove?
        //on which vehicle did we insert?
        val removedValue = x.removedValue
        //node cost to be considered
        if(!digestUpdates(prev)) return false

        val positionOfDelete = x.position

        val oldPrevValue = prev.newValue.valueAtPosition(positionOfDelete-1).get //vehicles are never deleted
        val oldSuccValue = RoutingConventionMethods.routingSuccPos2Val(positionOfDelete, prev.newValue,v)
        val newDistance = distanceMatrixOnNode(oldPrevValue)(oldSuccValue)
        val oldDistanceBefore = distanceMatrixOnNode(oldPrevValue)(removedValue)
        val oldDistanceAfter = distanceMatrixOnNode(removedValue)(oldSuccValue)
        val nodeCost = distanceMatrixOnNode(removedValue)(removedValue)

        if(perVehicle){
          val vehicle = vehicleSearcher(prev.newValue,positionOfDelete)
          recordTouchedVehicle(vehicle)
          distance(vehicle) :+= (newDistance - (oldDistanceBefore + oldDistanceAfter + nodeCost))
        }else{
          distance(0) :+= (newDistance - (oldDistanceBefore + oldDistanceAfter + nodeCost))
        }
        true

      case SeqUpdateLastNotified(value:IntSequence) =>
        require(value quickEquals routes.value)
        true //we are starting from the previous value
      case SeqUpdateAssign(value : IntSequence) =>
        false //impossible to go incremental
    }
  }

  /**
   * engages the saving of output values at this checkpoint.
   * you also must call  recordTouchedVehicle(v:Int) for this saving to be effective.
   * @param s
   */
  protected def saveCurrentCheckpoint(s:IntSequence): Unit ={
    checkpoint = s
    if(perVehicle) {
      while (changedVehiclesSinceCheckpoint != null) {
        isVehicleChangedSinceCheckpoint(changedVehiclesSinceCheckpoint.head) = false
        changedVehiclesSinceCheckpoint = changedVehiclesSinceCheckpoint.tail
      }
    }else{
      savedValues(0) = distance(0).newValue
    }

    //TODO: find stronger condition
    if(v > 1) vehicleSearcher = RoutingConventionMethods.cachedVehicleReachingPosition(checkpoint,v)
  }

  private def restoreCheckpoint(): Unit ={
    if(perVehicle) {
      while (changedVehiclesSinceCheckpoint != null) {
        val v = changedVehiclesSinceCheckpoint.head
        distance(v) := savedValues(v)
        isVehicleChangedSinceCheckpoint(v) = false
        changedVehiclesSinceCheckpoint = changedVehiclesSinceCheckpoint.tail
      }
    }else{
      distance(0) := savedValues(0)
    }
  }

  private def recordTouchedVehicle(v:Int): Unit ={
    if(perVehicle){
      if(checkpoint!= null && !isVehicleChangedSinceCheckpoint(v)){
        savedValues(v) = distance(v).newValue
        isVehicleChangedSinceCheckpoint(v) = true
        changedVehiclesSinceCheckpoint = QList(v,changedVehiclesSinceCheckpoint)
      }
    }
  }

  private def affect(value:Array[Long]): Unit ={
    var currentV = distance.length
    while(currentV >0){
      currentV -= 1
      distance(currentV) := value(currentV)
    }
  }

  // labeled forward and backward nodes with their cumulated distance
  // use invalidation per vehicle in case more than one move is performed
  // just one thing: backtrack is only performed through checkpoint; star mode will lead to recomputation of the vehicles from scratch
  //datastruct for checkpoint: forward et bw labeling per vehicle. labeling: node -> (forward,backward) in a redBlack
  protected def computeValueBetween(s:IntSequence, vehicle: Int, fromPosIncluded:Int, fromValueIncluded:Int, toPosIncluded:Int, toValueIncluded:Int):Long = {
    if(fromPosIncluded <= toPosIncluded) {
      var e = s.explorerAtPosition(fromPosIncluded).get
      var toReturn = distanceMatrixOnNode(e.value)(e.value)

      while (e.position < toPosIncluded) {
        val nextPos = e.next.get
        toReturn += distanceMatrixOnNode(e.value)(nextPos.value) + distanceMatrixOnNode(nextPos.value)(nextPos.value)
        e = nextPos
      }
      toReturn
    }else{
      var e = s.explorerAtPosition(fromPosIncluded).get
      var toReturn = distanceMatrixOnNode(e.value)(e.value)

      while (e.position > toPosIncluded) {
        val prevPos = e.prev.get
        toReturn += distanceMatrixOnNode(e.value)(prevPos.value) + distanceMatrixOnNode(prevPos.value)(prevPos.value)
        e = prevPos
      }
      toReturn
    }
  }

  /**
   *
   * @param s a sequence of integers representing routes
   * @return the distance per vehicle or the total distance in a singleton array, according to the global "perVehicle" flag
   */
  private def computeValueFromScratch(s:IntSequence):Array[Long] = {
    val toReturn = Array.tabulate(v)(v => distanceMatrixOnNode(v)(v))
    val it = s.iterator

    var prevNode:Int = it.next()
    var currentVehicle:Int = prevNode
    require(currentVehicle == 0)

    while(it.hasNext){
      val node = it.next()
      if(node < v){
        //reaching a new vehicle start
        //finishing the circle (cost of vehicle node already added)
        toReturn(currentVehicle) = toReturn(currentVehicle) + distanceMatrixOnNode(prevNode)(currentVehicle)
        currentVehicle = node
      }else{
        //continuing on the same vehicle
        toReturn(currentVehicle) = toReturn(currentVehicle) + distanceMatrixOnNode(prevNode)(node) +  distanceMatrixOnNode(node)(node)
      }
      prevNode = node
    }
    //for the last vehicle, the finishing operation in the loop will not be executed, so we have to add one here
    toReturn(currentVehicle) = toReturn(currentVehicle) + distanceMatrixOnNode(prevNode)(currentVehicle)

    if(perVehicle) toReturn
    else Array.fill(1)(toReturn.sum)
  }

  override def checkInternals(c : Checker) : Unit = {
    check(c, routes.value)
  }

  def check(c : Checker,s:IntSequence): Unit ={
    c.check(!distanceIsSymmetric || RouteLength.isDistanceSymmetric(distanceMatrix, n), Some("distance matrix should be symmetric if invariant told so"))

    if (perVehicle) {
      val values = computeValueFromScratch(s)
      for (vehicle <- 0 until v) {
        c.check(distance(vehicle).newValue == values(vehicle),
          Some(s"distance($vehicle).value=${distance(vehicle).newValue} should == computeValueFromScratch(routes.value)(0)${values(vehicle)}"))
      }

      if (checkpoint != null) {
        val values = computeValueFromScratch(checkpoint)
        for (vehicle <- 0 until v) {
          if (isVehicleChangedSinceCheckpoint(vehicle))
            c.check(savedValues(vehicle) == values(vehicle))
        }
      }

    } else {
      c.check(distance(0).newValue == computeValueFromScratch(s)(0),
        Some(s"distance(0).value=${distance(0).newValue} should== computeValueFromScratch(routes.value)(0)${computeValueFromScratch(routes.value)(0)}"))
      if (checkpoint != null) {
        c.check(savedValues(0) == computeValueFromScratch(checkpoint)(0))
      }
    }
  }
}
