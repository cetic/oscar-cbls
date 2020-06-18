package oscar.cbls.business.routing.invariants.global

import oscar.cbls.algo.magicArray.IterableMagicBoolArray
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.model.VehicleLocation
import oscar.cbls.core.computation.{ChangingSeqValue, Invariant, SeqNotificationTarget, SeqUpdate, SeqUpdateAssign, SeqUpdateDefineCheckpoint, SeqUpdateInsert, SeqUpdateLastNotified, SeqUpdateMove, SeqUpdateRemove, SeqUpdateRollBackToCheckpoint}
import oscar.cbls.core.propagation.Checker

import scala.annotation.tailrec

case class GlobalConstraintCore(routes: ChangingSeqValue, v: Int)
  extends Invariant with SeqNotificationTarget{

  val n: Int = routes.maxValue+1
  val vehicles: Range = 0 until v

  private var managedConstraints: QList[GlobalConstraintDefinition[_]] = _
  private var invariantAreInitiated = false
  private var useGlobalConstraintPositionCache = false

  private var checkpointLevel: Int = -1
  private var checkpointAtLevel0: IntSequence = _
  private val changedVehiclesSinceCheckpoint0 = new IterableMagicBoolArray(v, false)

  // An array holding the ListSegment modifications as a QList
  // Int == checkpoint level, ListSegments the new ListSegment of the vehicle considering the modifications
  // It holds at least one value : the initial value whose checkpoint level == -1
  // Then each time we do a modification to the ListSegment, it's stored with the current checkpoint level
  private var segmentsOfVehicle: Array[ListSegments] = Array.fill(v)(null)
  // Each time we define a checkpoint we save the current constraint state including :
  //    - changedVehiclesSinceCheckpoint0
  //    - vehiclesValueAtCheckpoint
  //    - vehicleSearcher
  //    - positionToValueCache
  private var savedDataAtCheckpointLevel: QList[(QList[Int], VehicleLocation, Array[Int], Array[ListSegments])] = null
  // Store the position value of a node for a given checkpoint level. -1 == position not computed
  private var positionToValueCache: Array[Int] = Array.fill(n)(-1)

  protected var vehicleSearcher: VehicleLocation = VehicleLocation((0 until v).toArray)

  registerStaticAndDynamicDependency(routes)

  finishInitialization()

  private def computeAndAssignVehicleValues(vehicle: Int, segmentsOfVehicle: QList[Segment], newRoute: IntSequence): Unit ={
    QList.qForeach(managedConstraints, (c: GlobalConstraintDefinition[_]) => c.computeSaveAndAssingVehicleValue(vehicle, segmentsOfVehicle, newRoute))
  }

  private def performPreComputes(vehicle: Int, routes: IntSequence): Unit ={
    QList.qForeach(managedConstraints, (c: GlobalConstraintDefinition[_]) => c.performPreCompute(vehicle, routes))
  }

  private def rollBackVehicleValuesToCheckPoint(vehiclesToRollBack: QList[Int]): Unit ={
    managedConstraints.foreach(c => c.rollBackToCheckpoint(vehiclesToRollBack))
  }

  private def setCheckpointLevel0Values(vehicle: Int): Unit ={
    QList.qForeach(managedConstraints, (c: GlobalConstraintDefinition[_]) => c.setCheckpointLevel0Value(vehicle))
  }

  private def computeSaveAndAssignVehicleValuesFromScratch(routes: IntSequence): Unit ={
    invariantAreInitiated = true
    QList.qForeach(managedConstraints, (c: GlobalConstraintDefinition[_]) => c.computeSaveAndAssignVehicleValuesFromScratch(routes))
  }

  def register(invariant: GlobalConstraintDefinition[_]): Unit ={
    managedConstraints = QList(invariant, managedConstraints)
  }

  override def notifySeqChanges(r: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    val newRoute = routes.newValue
    if(!invariantAreInitiated) computeSaveAndAssignVehicleValuesFromScratch(newRoute)

    if(digestUpdates(changes) && !(this.checkpointLevel == -1)){
      QList.qForeach(changedVehiclesSinceCheckpoint0.indicesAtTrueAsQList,(vehicle: Int) => {
        // Compute new vehicle value based on last segment changes
        computeAndAssignVehicleValues(vehicle, segmentsOfVehicle(vehicle).segments, newRoute)
      })
    } else {
      computeSaveAndAssignVehicleValuesFromScratch(newRoute)
    }
  }

  private def digestUpdates(changes:SeqUpdate): Boolean = {
    changes match {
      case SeqUpdateDefineCheckpoint(prev: SeqUpdate, isStarMode: Boolean, checkpointLevel: Int) =>
        val newRoute = changes.newValue
        val prevUpdate = digestUpdates(prev)

        // Either we got an assign update or this is the very first define checkpoint ==> We must init all vehicles
        if(!prevUpdate || this.checkpointLevel < 0) {
          checkpointAtLevel0 = newRoute // Save the state of the route for further comparison
          computeSaveAndAssignVehicleValuesFromScratch(newRoute)
          (0 until v).foreach(vehicle => {
            setCheckpointLevel0Values(vehicle)
            performPreComputes(vehicle, newRoute)
            initSegmentsOfVehicle(vehicle, newRoute)
          })
          // Defining checkpoint 0 ==> we must init every changed since checkpoint 0 vehicles
        } else if(checkpointLevel == 0) {
          checkpointAtLevel0 = newRoute // Save the state of the route for further comparison
          // Computing init ListSegment value for each vehicle that has changed
          QList.qForeach(changedVehiclesSinceCheckpoint0.indicesAtTrueAsQList,(vehicle: Int) => {
            setCheckpointLevel0Values(vehicle)
            performPreComputes(vehicle, newRoute)
            initSegmentsOfVehicle(vehicle, newRoute)
          })

          // Resetting the savedData QList.
          savedDataAtCheckpointLevel = null

          changedVehiclesSinceCheckpoint0.all = false

          // Defining another checkpoint. We must keep current segments state
        } else {
          // Saving position of nodes of the previous checkpoint level to avoid excessive calls to positionAtAnyOccurence(...) when roll-backing
          val previousCheckpointSaveData = savedDataAtCheckpointLevel.head
          savedDataAtCheckpointLevel = QList(
            (previousCheckpointSaveData._1, previousCheckpointSaveData._2, positionToValueCache, previousCheckpointSaveData._4),
            savedDataAtCheckpointLevel.tail)
        }

        // Persisting recent updates of the vehicleSearcher
        vehicleSearcher = vehicleSearcher.regularize

        // Common manipulations
        this.checkpointLevel = checkpointLevel
        if (savedDataAtCheckpointLevel == null || savedDataAtCheckpointLevel.size == checkpointLevel) {
          positionToValueCache = Array.fill(n)(-1)
          savedDataAtCheckpointLevel =
            QList((changedVehiclesSinceCheckpoint0.indicesAtTrueAsQList, vehicleSearcher, positionToValueCache, segmentsOfVehicle), savedDataAtCheckpointLevel)
          segmentsOfVehicle = savedDataAtCheckpointLevel.head._4.clone()
        }
        true

      case r@SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence,checkpointLevel:Int) =>
        if(checkpointLevel == 0) {
          require(checkpoint quickEquals this.checkpointAtLevel0)
        }

        // Restore the saved data corresponding to the required checkpoint
        savedDataAtCheckpointLevel = QList.qDrop(savedDataAtCheckpointLevel, this.checkpointLevel - checkpointLevel)

        // Restoring the vehicle values if it has changed since checkpoint 0 (using current changedVehiclesSinceCheckpoint0 value)
        QList.qForeach(savedDataAtCheckpointLevel.head._1, (value: Int) => changedVehiclesSinceCheckpoint0(value) = false)
        rollBackVehicleValuesToCheckPoint(changedVehiclesSinceCheckpoint0.indicesAtTrueAsQList)

        // Restoring all other values
        this.changedVehiclesSinceCheckpoint0.all = false
        QList.qForeach(savedDataAtCheckpointLevel.head._1, (vehicle: Int) =>  this.changedVehiclesSinceCheckpoint0(vehicle) = true)
        vehicleSearcher = savedDataAtCheckpointLevel.head._2
        positionToValueCache = savedDataAtCheckpointLevel.head._3
        segmentsOfVehicle = savedDataAtCheckpointLevel.head._4.clone()

        this.checkpointLevel = checkpointLevel
        true

      case sui@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if(digestUpdates(prev)){
          useGlobalConstraintPositionCache(prev)

          val prevRoutes = prev.newValue

          val impactedVehicle = vehicleSearcher.vehicleReachingPosition(pos-1)
          val impactedSegment = segmentsOfVehicle(impactedVehicle)

          // InsertSegment insert a segment AFTER a defined position and SeqUpdateInsert at a position => we must withdraw 1
          segmentsOfVehicle(impactedVehicle) = impactedSegment.insertSegments(QList[Segment](NewNode(value).asInstanceOf[Segment]),pos-1,prevRoutes)

          vehicleSearcher = vehicleSearcher.push(sui.oldPosToNewPos)
          changedVehiclesSinceCheckpoint0(impactedVehicle) = true
          true
        } else {
          false
        }

      case sum@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        if(digestUpdates(prev)) {
          useGlobalConstraintPositionCache(prev)

          val prevRoutes = prev.newValue

          val fromVehicle = vehicleSearcher.vehicleReachingPosition(fromIncluded)
          val toVehicle = vehicleSearcher.vehicleReachingPosition(after)
          val sameVehicle = fromVehicle == toVehicle

          val fromImpactedSegment = segmentsOfVehicle(fromVehicle)

          // Identification of the sub-segments to remove
          val (listSegmentsAfterRemove, segmentsToRemove) =
            fromImpactedSegment.removeSubSegments(fromIncluded, toIncluded, prevRoutes)

          val toImpactedSegment = if (sameVehicle) listSegmentsAfterRemove else segmentsOfVehicle(toVehicle)
          // If we are in same vehicle and we remove nodes to put them later in the route, the route length before insertion point has shortened
          val delta =
            if (!sameVehicle || after < fromIncluded) 0
            else toIncluded - fromIncluded + 1

          // Insert the sub-segments at his new position
          val listSegmentsAfterInsertion =
            if (flip)
              toImpactedSegment.insertSegments(segmentsToRemove.qMap(_.flip()).reverse, after, prevRoutes, delta)
            else
              toImpactedSegment.insertSegments(segmentsToRemove, after, prevRoutes, delta)

          segmentsOfVehicle(toVehicle) = listSegmentsAfterInsertion
          if (!sameVehicle) segmentsOfVehicle(fromVehicle) = listSegmentsAfterRemove

          vehicleSearcher = vehicleSearcher.push(sum.oldPosToNewPos)

          changedVehiclesSinceCheckpoint0(fromVehicle) = true
          changedVehiclesSinceCheckpoint0(toVehicle) = true
          true
        } else {
          false
        }

      case sur@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        if(digestUpdates(prev)) {
          useGlobalConstraintPositionCache(prev)

          val prevRoutes = prev.newValue

          val impactedVehicle = vehicleSearcher.vehicleReachingPosition(position)
          val impactedSegment = segmentsOfVehicle(impactedVehicle)

          val (listSegmentAfterRemove, _) = impactedSegment.removeSubSegments(position, position, prevRoutes)

          segmentsOfVehicle(impactedVehicle) = listSegmentAfterRemove

          vehicleSearcher = vehicleSearcher.push(sur.oldPosToNewPos)
          changedVehiclesSinceCheckpoint0(impactedVehicle) = true
          true
        } else {
          false
        }

      case SeqUpdateLastNotified(value:IntSequence) =>
        require(value quickEquals routes.value)
        true

      case SeqUpdateAssign(value : IntSequence) =>
        false //impossible to go incremental
    }
  }

  /**
    * Determine if we can use the cache or not.
    * We can use it only if the current insert/move/remove update is right after a defineCheckPoint or a rollbackToCheckpoint
    * Otherwise, Some changes have occurred and the positions may not be correct anymore.
    */
  private def useGlobalConstraintPositionCache(prev: SeqUpdate): Unit ={
    prev match {
      case _:SeqUpdateInsert => useGlobalConstraintPositionCache = false
      case _:SeqUpdateMove => useGlobalConstraintPositionCache = false
      case _:SeqUpdateRemove => useGlobalConstraintPositionCache = false
      case _:SeqUpdateLastNotified => useGlobalConstraintPositionCache = false
      case _ => useGlobalConstraintPositionCache = true
    }
  }

  /**
    * Initialize the ListSegment of a given vehicle. It's done each time we define a checkpoint lvl 0.
    * (for changed vehicle)
    */
  private def initSegmentsOfVehicle(vehicle: Int, route: IntSequence): Unit ={
    val posOfVehicle = vehicleSearcher.startPosOfVehicle(vehicle)
    val (lastNodeOfVehicle, posOfLastNodeOfVehicle) =
      if(vehicle < v-1) {
        val lastNodePos = vehicleSearcher.startPosOfVehicle(vehicle+1)-1
        (route.valueAtPosition(lastNodePos).get,lastNodePos)
      }
      else {
        (route.valueAtPosition(route.size-1).get,route.size-1)
      }

    segmentsOfVehicle(vehicle) = ListSegments(
      QList[Segment](PreComputedSubSequence(
        vehicle,
        lastNodeOfVehicle,
        posOfLastNodeOfVehicle - posOfVehicle + 1
      )),
      vehicle)
  }


  override def checkInternals(c : Checker): Unit = {
    for (c <- managedConstraints) {
      for (v <- vehicles) {
        c.checkInternals(v, routes, segmentsOfVehicle(v).segments.toList)
      }
    }
  }

  object ListSegments{
    def apply(segments: QList[Segment], vehicle: Int): ListSegments = new ListSegments(segments, vehicle)

    def apply(listSegments: ListSegments): ListSegments =
      new ListSegments(listSegments.segments, listSegments.vehicle)
  }

  class ListSegments(val segments: QList[Segment], val vehicle: Int){

    private def getValueAtPosition(pos: Int, routes: IntSequence): Int ={
      if(pos >= n) -1
      else if(!useGlobalConstraintPositionCache){   // If we can't use the cache we use the explorer
        val explorer = routes.explorerAtPosition(pos)
        if (explorer.isDefined)
          explorer.get.value
        else
          -1
      } else {  // Else we use the memorized value or the explorer if not defined
        if (pos < n && positionToValueCache(pos) == -1) {
          val explorer = routes.explorerAtPosition(pos)
          if (explorer.isDefined)
            positionToValueCache(pos) = explorer.get.value
        }
        positionToValueCache(pos)
      }
    }

    /**
      * Remove Segments and Sub-Segment from the Segments List
      *
      * Find the Segments holding the specified positions and split them in two parts right after these positions.
      * If a position is at the end of a Segment, this Segment is not splitted.
      * The in-between segments are gathered as well
      * @param from From position
      * @param to To position
      * @return A ListSegments containing the segments containing or between from and to
      */
    def removeSubSegments(from: Int, to: Int, routes: IntSequence): (ListSegments, QList[Segment]) ={
      val vehiclePos = vehicleSearcher.startPosOfVehicle(vehicle)
      val (fromImpactedSegment, segmentsBeforeFromImpactedSegment, segmentsAfterFromImpactedSegment, currentCounter) =
        findImpactedSegment(from,vehicle,vehiclePos-1)
      val (toImpactedSegment, tempSegmentsBetweenFromAndTo, segmentsAfterToImpactedSegment,_) =
        findImpactedSegment(to,vehicle, currentCounter,QList(fromImpactedSegment,segmentsAfterFromImpactedSegment))

      val segmentsBetweenFromAndTo = QList.qDrop(tempSegmentsBetweenFromAndTo,1)

      // nodeBeforeFrom is always defined because it's at worst a vehicle node
      val nodeBeforeFrom = getValueAtPosition(from-1, routes)
      val fromNode = getValueAtPosition(from, routes)
      val toNode = getValueAtPosition(to, routes)
      val nodeAfterTo = if(to+1 < n)getValueAtPosition(to+1, routes) else -1

      val lengthUntilFromImpactedSegment = QList.qFold[Segment,Int](segmentsBeforeFromImpactedSegment, (acc,item) => acc + item.length(),0)

      // We split the fromImpactedSegment in two part fromLeftResidue and toLeftResidue
      val fromLeftResidueLength = from - lengthUntilFromImpactedSegment - vehiclePos    // From is not part of the left residue
      val fromRightResidueLength = fromImpactedSegment.length() - fromLeftResidueLength
      val fromLeftAndRightResidue = fromImpactedSegment.splitAtNode(nodeBeforeFrom, fromNode, fromLeftResidueLength, fromRightResidueLength)
      val fromLeftResidue = fromLeftAndRightResidue._1
      val fromRightResidue = fromLeftAndRightResidue._2

      val (removedSegments, toRightResidue) =
        if(fromImpactedSegment == toImpactedSegment){
          // Same segment => We split the fromRightResidue in two parts removedSubSegment and toRightResidue
          if (nodeAfterTo < v)      // To is at the end of the vehicle route
            (QList(fromRightResidue), null)
          else {
            val toRightResidueLength = fromRightResidueLength - to + from - 1
            val removedSubSegmentLength = fromRightResidueLength - toRightResidueLength
            val removedSegmentAndRightResidue = fromRightResidue.splitAtNode(toNode, nodeAfterTo, removedSubSegmentLength, toRightResidueLength)
            (QList(removedSegmentAndRightResidue._1), removedSegmentAndRightResidue._2)
          }

        } else {
          var removedSegments = QList(fromRightResidue,segmentsBetweenFromAndTo)
          val lengthUntilToImpactedSegment =
            lengthUntilFromImpactedSegment + fromImpactedSegment.length() +
              QList.qFold[Segment,Int](segmentsBetweenFromAndTo, (acc,item) => acc + item.length(),0)

          // Diff segment => We split the toImpactedSegment in two parts toLeftResidue and toRightResidue
          val toLeftAndRightResidue =
            if(nodeAfterTo < v)
              (toImpactedSegment, null)
            else {
              val toLeftResidueLength = to - vehiclePos - lengthUntilToImpactedSegment + 1    // To is part of the left residue
              val toRightResidueLength = toImpactedSegment.length() - toLeftResidueLength
              toImpactedSegment.splitAtNode(toNode, nodeAfterTo, toLeftResidueLength, toRightResidueLength)
            }

          val toLeftResidue = toLeftAndRightResidue._1
          val toRightResidue = toLeftAndRightResidue._2

          removedSegments = QList.nonReversedAppend(removedSegments, QList(toLeftResidue))
          (removedSegments, toRightResidue)
        }

      var newSegments: QList[Segment] = segmentsAfterToImpactedSegment
      if(toRightResidue != null) newSegments = QList(toRightResidue, newSegments)
      if(fromLeftResidue != null) newSegments = QList(fromLeftResidue, newSegments)
      newSegments = QList.nonReversedAppend(segmentsBeforeFromImpactedSegment, newSegments)

      (ListSegments(newSegments,vehicle),removedSegments)
    }

    /**
      * Insert a list of segments at the specified position
      * @param segmentsToInsert
      * @param afterPosition
      * @return
      */
    def insertSegments(segmentsToInsert: QList[Segment], afterPosition: Int, routes: IntSequence, delta: Int = 0): ListSegments ={
      val vehiclePos = vehicleSearcher.startPosOfVehicle(vehicle)

      val (impactedSegment, segmentsBeforeImpactedSegment, segmentsAfterImpactedSegment,_) = findImpactedSegment(afterPosition - delta, vehicle, vehiclePos-1)

      val insertAfterNode = getValueAtPosition(afterPosition, routes)
      val insertBeforeNode: Int = if(afterPosition+1 < n) getValueAtPosition(afterPosition+1, routes) else -1

      // We split the impacted segment in two parts (leftResidue and rightResidue)
      // 1° => Compute parts' length
      // 2° => Split the impacted segment
      val segmentsLengthBeforeImpactedSegment = QList.qFold[Segment,Int](segmentsBeforeImpactedSegment, (acc,item) => acc + item.length(),0)
      val leftRightResidue = if(insertBeforeNode < v){
        (impactedSegment, null)
      } else {
        val rightResidueLength = segmentsLengthBeforeImpactedSegment + impactedSegment.length() - afterPosition + vehiclePos - 1 + delta
        val leftResidueLength = impactedSegment.length() - rightResidueLength
        impactedSegment.splitAtNode(insertAfterNode, insertBeforeNode, leftResidueLength, rightResidueLength)
      }
      val leftResidue = leftRightResidue._1
      val rightResidue = leftRightResidue._2


      // Building the resulting QList starting at the end
      var newSegments: QList[Segment] = segmentsAfterImpactedSegment                     // Segments after impacted segment
      if(rightResidue != null) newSegments = QList(rightResidue, newSegments)               // add right residue
      newSegments = QList.nonReversedAppend(segmentsToInsert, newSegments)                  // prepend the segments to insert
      if(leftResidue != null) newSegments = QList(leftResidue, newSegments)                 // add left residue
      newSegments = QList.nonReversedAppend(segmentsBeforeImpactedSegment, newSegments)     // Prepend segments before impacted segments

      ListSegments(newSegments, vehicle)
    }

    /**
      * This method finds the impacted segment of the previous update.
      * The segment is found if the endNode is after or equal to the search position.
      *
      * @param pos the searched position
      * @param vehicle the vehicle in which we want to add a node
      * @return a tuple (impactedSegment: Segment, exploredSegments: Option[QList[Segment ] ], unexploredSegments: Option[QList[Segment ] ])
      */
    private def findImpactedSegment(pos: Int, vehicle: Int, initCounter: Int, segmentsToExplore: QList[Segment] = segments): (Segment, QList[Segment], QList[Segment], Int) ={
      @tailrec
      def checkSegment(segmentsToExplore: QList[Segment], counter: Int = initCounter, exploredSegments: QList[Segment] = null): (Segment, QList[Segment], QList[Segment], Int) ={
        require(segmentsToExplore != null, "Shouldn't happen, it means that the desired position is not within this vehicle route")
        val segment = segmentsToExplore.head
        val newCounter = counter + segment.length
        if(newCounter >= pos)
          (segment, if(exploredSegments != null) exploredSegments.reverse else exploredSegments, segmentsToExplore.tail, counter)
        else
          checkSegment(segmentsToExplore.tail, newCounter, QList(segment,exploredSegments))
      }

      checkSegment(segmentsToExplore)
    }

    def length(): Int ={
      QList.qFold[Segment, Int](segments, (acc, item) => acc + item.length, 0)
    }

    override def toString: String ={
      s"Segments of vehicle $vehicle : ${segments.mkString(", ")}"
    }
  }

}

trait Segment{
  /**
    * Split this Segment in two Segments right before the split node
    * If split node == start node, there will only be one Segment, the Segment itself
    * @return the left part of the splitted Segment (if exist) and the right part (starting at splitNode)
    */
  def splitAtNode(beforeSplitNode: Int, splitNode: Int, leftLength: Int, rightLength: Int): (Segment,Segment)

  def flip(): Segment

  def length(): Int

  def startNode(): Int

  def endNode(): Int
}

/**
  * This represents a subsequence starting at startNode and ending at endNode.
  * This subsequence was present in the global sequence when the pre-computation was performed
  * @param startNode the first node of the subsequence
  * @param endNode the last node of the subsequence
  */
case class PreComputedSubSequence(startNode:Int,
                                                  endNode:Int,
                                                  length: Int) extends Segment{
  override def toString: String = {
    s"PreComputedSubSequence (StartNode : $startNode EndNode : $endNode Length : $length)"
  }

  override def splitAtNode(beforeSplitNode: Int, splitNode: Int, leftLength: Int, rightLength: Int): (Segment,Segment) = {
    if(splitNode == startNode) (null,this)
    else if(beforeSplitNode == endNode) (this,null)
    else {
      (PreComputedSubSequence(startNode,beforeSplitNode,leftLength),
        PreComputedSubSequence(splitNode,endNode,rightLength))
    }
  }

  override def flip(): Segment = {
    FlippedPreComputedSubSequence(endNode, startNode, length)
  }
}

/**
  * This represents a subsequence starting at startNode and ending at endNode.
  * This subsequence was not present in the global sequence when the pre-computation was performed, but
  * the flippedd subsequence obtained by flippig it was present in the global sequence when the pre-computation was performed, but
  * @param startNode the first node of the subsequence (it was after the endNode when pre-computation ws performed)
  * @param endNode the last node of the subsequence (it was before the endNode when pre-computation ws performed)
  */
case class FlippedPreComputedSubSequence(startNode:Int,
                                                         endNode:Int,
                                                         length: Int) extends Segment{
  override def toString: String = {
    s"FlippedPreComputedSubSequence (StartNode : $startNode EndNode : $endNode Length : $length)"
  }

  override def splitAtNode(beforeSplitNode: Int, splitNode: Int, leftLength: Int, rightLength: Int): (Segment,Segment) = {
    if(splitNode == startNode) (null,this)
    else if(beforeSplitNode == endNode) (this,null)
    else {
      (FlippedPreComputedSubSequence(startNode,beforeSplitNode,leftLength),
        FlippedPreComputedSubSequence(splitNode,endNode,rightLength))
    }
  }

  override def flip(): Segment = {
    PreComputedSubSequence(endNode, startNode, length)
  }
}

/**
  * This represent that a node that was not present in the initial sequence when pre-computation was performed.
  * @param node
  */
case class NewNode(node:Int) extends Segment{
  override def toString: String = {
    s"NewNode - Node : $node"
  }

  override def splitAtNode(beforeSplitNode: Int, splitNode: Int, leftLength: Int, rightLength: Int): (Segment,Segment) = {
    require(beforeSplitNode == node)
    (this, null)
  }

  override def flip(): Segment = {
    this
  }

  override def length(): Int = 1

  override def startNode(): Int = node

  override def endNode(): Int = node
}
