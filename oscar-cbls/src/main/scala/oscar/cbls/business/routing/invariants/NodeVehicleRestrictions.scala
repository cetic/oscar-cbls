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
import oscar.cbls.core.computation.{CBLSIntConst, CBLSIntVar, ChangingSeqValue, IntValue, Invariant, SeqNotificationTarget, SeqUpdate, SeqUpdateAssign, SeqUpdateDefineCheckpoint, SeqUpdateInsert, SeqUpdateLastNotified, SeqUpdateMove, SeqUpdateRemove, SeqUpdateRollBackToCheckpoint, SetValue}
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.logic.Filter
import oscar.cbls.lib.invariant.set.BelongsTo

import scala.collection.immutable.SortedSet

object NodeVehicleRestrictions{
  /**
   * this invariant maintains a degree of violation for route restriction constraints.
   * there is a set of restrictions node<->vehicle,
   * the invariant maintains, for each vehicle, the number of node
   * that it reaches although it should not, according to the mentioned restrictions.
   * @param routes
   * @param v
   * @param nodeVehicleRestrictions the restrictions that we are monitoring
   * @return an array telling the violation per vehicle
   */
  def apply(routes:ChangingSeqValue, v:Int, nodeVehicleRestrictions:Iterable[(Int,Int)]):Array[CBLSIntVar] = {
    val violationPerVehicle =  Array.tabulate(v)(vehicle => CBLSIntVar(routes.model, name="violation of nodeVehicleRestriction for vehicle" + vehicle))

    new NodeVehicleRestrictions(routes, v, nodeVehicleRestrictions, violationPerVehicle)

    violationPerVehicle
  }

  def violatedNodes(routes:ChangingSeqValue,v:Int,nodeVehicleRestrictions:Iterable[(Int,Int)]):SetValue = {
    violatedNodes(VehicleOfNodes(routes,v).asInstanceOf[Array[IntValue]],v,nodeVehicleRestrictions)
  }

  def violatedNodes(vehicleOfNode:Array[IntValue],v:Int,nodeVehicleRestrictions:Iterable[(Int,Int)]):SetValue = {

    val n = vehicleOfNode.length

    val noForbidden = SortedSet.empty[Int]
    val forbiddenVehicleForNodes: Array[SortedSet[Int]] = Array.fill(n)(null)

    for ((node, vehicle) <- nodeVehicleRestrictions) {
      if(forbiddenVehicleForNodes(node) == null){
        forbiddenVehicleForNodes(node) = noForbidden + vehicle
      }else{
        forbiddenVehicleForNodes(node) = forbiddenVehicleForNodes(node) + vehicle
      }
    }

    Filter(Array.tabulate(n)(node => {
      val forbiddenVehicleForNode = forbiddenVehicleForNodes(node)
      if(forbiddenVehicleForNode == null) CBLSIntConst(0)
      else BelongsTo(vehicleOfNode(node),forbiddenVehicleForNode)}
    ))
  }
}

/**
 * this invariant maintains a degree of violation for route restriction constraints.
 * there is a set of restrictions node<->vehicle,
 * the invariant maintains, for each vehicle, the number of node
 * that it reaches although it should not, according to the mentioned restrictions.
 * @param routes
 * @param v
 * @param nodeVehicleRestrictions the restrictions that we are monitoring
 * @param violationPerVehicle an array of v non-controlled var that the invariant will control
 *                            and maintain to the degree of violation of each vehicle
 */
class NodeVehicleRestrictions(routes:ChangingSeqValue,
                              v:Int,
                              nodeVehicleRestrictions:Iterable[(Int,Int)],
                              violationPerVehicle:Array[CBLSIntVar])
  extends Invariant() with SeqNotificationTarget {

  require(v>1,"cannot have vehicle restrictions with only one vehicle")
  val n = routes.maxValue+1
  val vehicles = 0 until v

  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  for(v <- violationPerVehicle) v.setDefiningInvariant(this)

  protected var vehicleSearcher = RoutingConventionMethods.cachedVehicleReachingPosition(routes.value, v)

  private val nodeToVehiclesRestriction : Array[(Array[Boolean],QList[Int])] = Array.fill(n)(null)

  for ((node, vehicle) <- nodeVehicleRestrictions) {
    nodeToVehiclesRestriction(node) match{
      case null =>
        val restrictionArray = Array.fill(v)(false)
        restrictionArray(vehicle) = true
        nodeToVehiclesRestriction(node) = (restrictionArray,QList(vehicle))
      case (restrictionArray,restrictionList) =>
        if(!restrictionArray(vehicle)){
          restrictionArray(vehicle) = true
          nodeToVehiclesRestriction(node) = (restrictionArray,QList(vehicle,restrictionList))
        }
    }
  }

  def anyRestrictionForNode(node:Int):Boolean = {
    nodeToVehiclesRestriction(node) != null
  }

  def isAllowed(node : Int, vehicle : Int): Boolean = {
    val nodeRestrictions = nodeToVehiclesRestriction(node)
    nodeRestrictions == null || !nodeRestrictions._1(vehicle)
  }

  def isForbidden(node : Int, vehicle : Int): Boolean = {
    val nodeRestrictions = nodeToVehiclesRestriction(node)
    nodeRestrictions != null && nodeRestrictions._1(vehicle)
  }

  def forbiddenVehicles(node:Int):Iterable[Int] = {
    val nodeRestrictions = nodeToVehiclesRestriction(node)
    if(nodeRestrictions == null) None
    else nodeRestrictions._2
  }

  var checkpoint : IntSequence = null
  var violationAtCheckpoint:Array[Long] = Array.fill(v)(-1L)

  //TODO: improve complexity of the pre-computation: make them lazy per vehicle, only pre-compute when needed
  //node n => vehicle v => number of node from start of vehicle reaching n that cannot be reached by vehicle v
  val precomputationAtCheckpoint:Array[Array[Long]] = Array.tabulate(n)(_=>Array.fill(v)(0L))
  //TODO use magic array here

  val vehicleChangedSinceCheckpoint:Array[Boolean] = Array.fill(v)(true)
  var changedVehicleSinceCheckpoint:QList[Int] = vehicles.foldLeft[QList[Int]](null)((acc,v) => QList(v,acc))

  def setVehicleChangedSinceCheckpoint(vehicle:Int): Unit ={
    if(!vehicleChangedSinceCheckpoint(vehicle)){
      vehicleChangedSinceCheckpoint(vehicle) = true
      changedVehicleSinceCheckpoint = QList(vehicle,changedVehicleSinceCheckpoint)
    }
  }

  def setAllVehicleChangedSinceCheckpoint(): Unit ={
    for (vehicle <- vehicles) {
      setVehicleChangedSinceCheckpoint(vehicle)
    }
  }

  def updateAllInvalidPrecomputationsToCheckpointAndSaveCheckpoint(): Unit ={
    while(changedVehicleSinceCheckpoint != null){
      val vehicle = changedVehicleSinceCheckpoint.head
      changedVehicleSinceCheckpoint = changedVehicleSinceCheckpoint.tail
      violationAtCheckpoint(vehicle) = doUpdatePrecomputationToCheckpointAndSaveCheckpoint(vehicle)
    }
  }

  def doUpdateAllPrecomputationsToCheckpointAndSaveCheckpoint(): Unit ={
    for (vehicle <- vehicles) {
      violationAtCheckpoint(vehicle) = doUpdatePrecomputationToCheckpointAndSaveCheckpoint(vehicle)
    }
    changedVehicleSinceCheckpoint = null
  }

  def doUpdatePrecomputationToCheckpointAndSaveCheckpoint(vehicle:Int):Long = {
    vehicleChangedSinceCheckpoint(vehicle) = false
    val explorerAtVehicleStart = checkpoint.explorerAtAnyOccurrence(vehicle).head
    var restrictionsForPrev = precomputationAtCheckpoint(explorerAtVehicleStart.value)
    var explorerAtCurrentNode = explorerAtVehicleStart.next
    //TODO: put a while(true) here, and say that this is Gustav's suggestion in the comit
    while(explorerAtCurrentNode match{
      case None => return restrictionsForPrev(vehicle); false
      case Some(nodePosition) =>
        val node = nodePosition.value
        if(node < v){
          //we are at a vehicle
          return restrictionsForPrev(vehicle)
          false
        }else {
          val restrictionsForCurrent = precomputationAtCheckpoint(node)
          for (v <- vehicles) {
            restrictionsForCurrent(v) = restrictionsForPrev(v)
          }

          for (forbiddenVehicle <- forbiddenVehicles(node)) {
            restrictionsForCurrent(forbiddenVehicle) += 1L
          }
          restrictionsForPrev = restrictionsForCurrent
          explorerAtCurrentNode = nodePosition.next
          true
        }
    }){}
    throw new Error("unexpected end")
  }

  def violationOnSegmentFromPrecomputation(fromValue:Int,toValue:Int,vehicle:Int):Long = {
    require(!vehicleChangedSinceCheckpoint(vehicle))
    val toReturn = if(fromValue == toValue){
      if(isForbidden(toValue,vehicle)) 1L else 0L
    }else{
      val nbRejectionFrom = precomputationAtCheckpoint(fromValue)(vehicle)
      val nbRejectionTo = precomputationAtCheckpoint(toValue)(vehicle)
      nbRejectionTo - nbRejectionFrom + (if(isForbidden(fromValue,vehicle)) 1L else 0L)
    }
    toReturn
  }

  def violationOnNodesFromScratch(nodes:Iterable[Int],vehicle:Int):Long = {
    var toReturn = 0L
    val it = nodes.iterator
    while(it.hasNext){
      val node = it.next()
      if(isForbidden(node,vehicle)) toReturn += 1L
    }
    toReturn
  }

  def violationOnSegment(seqWithSegment:IntSequence, fromValue: Int, toValue: Int, vehicle: Int):Long = {
    //TODO: propose something more incremental that supports a few vehicle changes before reverting to inefficient update
    if(vehicleChangedSinceCheckpoint(vehicle)){
      violationOnNodesFromScratch(
        seqWithSegment.valuesBetweenPositionsQList(
          seqWithSegment.positionOfAnyOccurrence(fromValue).head,
          seqWithSegment.positionOfAnyOccurrence(toValue).head),vehicle)
    }else{
      val toReturn = violationOnSegmentFromPrecomputation(fromValue,toValue,vehicle)
      assert(toReturn == violationOnNodesFromScratch(
        seqWithSegment.valuesBetweenPositionsQList(
          seqWithSegment.positionOfAnyOccurrence(fromValue).head,
          seqWithSegment.positionOfAnyOccurrence(toValue).head),vehicle))
      toReturn
    }
  }

  def violationOnVehicle(vehicle: Int,seq:IntSequence):Long = {
    if(vehicleChangedSinceCheckpoint(vehicle)){
      violationOnNodesFromScratch(
        seq.valuesBetweenPositionsQList(
          seq.positionOfAnyOccurrence(vehicle).head,
          if(vehicle == v-1) seq.size-1 else seq.positionOfAnyOccurrence(vehicle+1).head),
        vehicle)
    }else{
      val toReturn = violationOnSegmentFromPrecomputation(vehicle,if(vehicle == v-1) seq.last else seq.predecessorPos2Val(seq.positionOfAnyOccurrence(vehicle+1).head).head,vehicle)
      assert(toReturn == violationOnNodesFromScratch(
        seq.valuesBetweenPositionsQList(
          seq.positionOfAnyOccurrence(vehicle).head,
          if(vehicle == v-1) seq.size-1 else seq.positionOfAnyOccurrence(vehicle+1).head),
        vehicle))
      toReturn
    }
  }

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    if (!digestUpdates(changes)) {
      setAllVehicleChangedSinceCheckpoint()
      for (vehicle <- vehicles) {
        violationPerVehicle(vehicle) := violationOnVehicle(vehicle: Int,changes.newValue)
      }
    }
  }

  private def digestUpdates(changes : SeqUpdate) : Boolean = {
    changes match {
      case SeqUpdateDefineCheckpoint(prev : SeqUpdate, isStarMode : Boolean, checkpointLevel:Int) =>

        if(checkpointLevel == 0){
          if (!digestUpdates(prev)) {
            this.checkpoint = prev.newValue
            doUpdateAllPrecomputationsToCheckpointAndSaveCheckpoint()
            for(vehicle <- vehicles) violationPerVehicle(vehicle) := violationOnVehicle(vehicle,checkpoint)
          }else {
            this.checkpoint = prev.newValue
            updateAllInvalidPrecomputationsToCheckpointAndSaveCheckpoint()
          }
          vehicleSearcher = RoutingConventionMethods.cachedVehicleReachingPosition(checkpoint, v)
          true
        }else{
          digestUpdates(prev)
        }

      case r@SeqUpdateRollBackToCheckpoint(checkpoint : IntSequence, checkpointLevel:Int) =>
        if(checkpointLevel == 0) {
          require(checkpoint quickEquals this.checkpoint)
          while (changedVehicleSinceCheckpoint != null) {
            val vehicle = changedVehicleSinceCheckpoint.head
            changedVehicleSinceCheckpoint = changedVehicleSinceCheckpoint.tail
            violationPerVehicle(vehicle) := violationAtCheckpoint(vehicle)
            vehicleChangedSinceCheckpoint(vehicle) = false
          }
          true
        }else{
          digestUpdates(r.howToRollBack)
        }
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        //on which vehicle did we insert?

        if (!digestUpdates(prev)) return false

        val vehicleOfInsert = vehicleSearcher(changes.newValue, pos)

        if (isForbidden(value, vehicleOfInsert)) {
          violationPerVehicle(vehicleOfInsert) :+= 1L
        }
        setVehicleChangedSinceCheckpoint(vehicleOfInsert)

        true
      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        if (!digestUpdates(prev)) false
        else if (x.isNop) true
        else if (x.isSimpleFlip) {
          //this is a simple flip, no change on vehicle violation, since obeys routing rule!
          val vehicleOfMovedSegment = vehicleSearcher(prev.newValue,fromIncluded)
          setVehicleChangedSinceCheckpoint(vehicleOfMovedSegment)
          true
        } else if(fromIncluded == toIncluded) {
          //per vehicle, there might be some node cost to consider
          val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)
          val targetVehicleOfMove = vehicleSearcher(prev.newValue, after)

          if (vehicleOfMovedSegment == targetVehicleOfMove) {
            //moving within the vehicle; same as flip, actually
            setVehicleChangedSinceCheckpoint(vehicleOfMovedSegment)
          }else {
            //moving accross vehicles

            val value = x.fromValue

            if(isForbidden(value,vehicleOfMovedSegment)){
              violationPerVehicle(vehicleOfMovedSegment) :-= 1L
            }
            if(isForbidden(value,targetVehicleOfMove)){
              violationPerVehicle(targetVehicleOfMove) :+= 1L
            }

            if(anyRestrictionForNode(value)) {
              setVehicleChangedSinceCheckpoint(vehicleOfMovedSegment)
            }
            //TODO: we can actually update this in O(1) if there is no restriction on the moved node instead of invalidating it
            setVehicleChangedSinceCheckpoint(targetVehicleOfMove)
          }
          true
        }else {
          //per vehicle, there might be some node cost to consider
          val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)
          val targetVehicleOfMove = vehicleSearcher(prev.newValue, after)
          //assert(vehicleOfMovedSegment == RoutingConventionMethods.searchVehicleReachingPosition(toIncluded, prev.newValue, v))

          if (vehicleOfMovedSegment == targetVehicleOfMove) {
            //moving within the vehicle; same as flip, actually
            setVehicleChangedSinceCheckpoint(vehicleOfMovedSegment)
          }else {
            //moving accross vehicles

            val fromValue = x.fromValue
            val toValue = x.toValue

            if(violationPerVehicle(vehicleOfMovedSegment).newValue != 0L){
              //check if we decrease the violation on that vehicle
              violationPerVehicle(vehicleOfMovedSegment) :-= violationOnSegment(prev.newValue, fromValue, toValue, vehicleOfMovedSegment)
            }//else we do not introduce a violation on that vehicle by removing some nodes, right?

            violationPerVehicle(targetVehicleOfMove) :+= violationOnSegment(prev.newValue, fromValue, toValue, targetVehicleOfMove)

            setVehicleChangedSinceCheckpoint(vehicleOfMovedSegment)
            setVehicleChangedSinceCheckpoint(targetVehicleOfMove)
          }
          true
        }

      case x@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        //on which vehicle did we remove?
        if (!digestUpdates(prev)) return false
        val removedNode = x.removedValue
        val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, position)
        if(isForbidden(removedNode,vehicleOfMovedSegment)){
          violationPerVehicle(vehicleOfMovedSegment) :-= 1L
        }
        if(anyRestrictionForNode(removedNode)) {
          setVehicleChangedSinceCheckpoint(vehicleOfMovedSegment)
        }
        true

      case SeqUpdateLastNotified(value : IntSequence) =>
        true //we are starting from the previous value
      case SeqUpdateAssign(value : IntSequence) =>
        false //impossible to go incremental
    }
  }

  override def checkInternals(c : Checker) : Unit = {

    val seq = routes.value
    for(vehicle <- vehicles){
      c.check(violationPerVehicle(vehicle).value == violationOnVehicle(vehicle,seq),
        Some(s"violationPerVehicle(vehicle).value=${violationPerVehicle(vehicle).value} should == violationOnVehicle(vehicle,seq)=${violationOnVehicle(vehicle,seq)} vehicle:$vehicle route:$seq"))

      val oldUpToDate = vehicleChangedSinceCheckpoint(vehicle)
      vehicleChangedSinceCheckpoint(vehicle) = true
      c.check(violationPerVehicle(vehicle).value == violationOnVehicle(vehicle,seq))
      vehicleChangedSinceCheckpoint(vehicle) = oldUpToDate
    }
    if(checkpoint != null){
      for(vehicle <- vehicles){
        val oldUpToDate = vehicleChangedSinceCheckpoint(vehicle)
        vehicleChangedSinceCheckpoint(vehicle) = true
        c.check(violationAtCheckpoint(vehicle) == violationOnVehicle(vehicle,checkpoint))
        vehicleChangedSinceCheckpoint(vehicle) = oldUpToDate
      }
    }
  }
}
