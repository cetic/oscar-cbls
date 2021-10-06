package oscar.cbls.business.routing.invariants.global

import oscar.cbls.algo.magicArray.IterableMagicBoolArray
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.model.VehicleLocation
import oscar.cbls.core.computation.{ChangingSeqValue, Invariant, SeqNotificationTarget, SeqUpdate, SeqUpdateAssign, SeqUpdateDefineCheckpoint, SeqUpdateInsert, SeqUpdateLastNotified, SeqUpdateMove, SeqUpdateRemove, SeqUpdateRollBackToCheckpoint}
import oscar.cbls.core.propagation.Checker

import scala.annotation.tailrec
import oscar.cbls.business.routing.invariants.segments._
import oscar.cbls.business.routing.invariants

/**
 * This abstract class must be extends in order to define a new global constraint.
 *
 * It uses a parameterized type U that represents the output type of your constraint
 * (for instance for RouteLength output value is of type Long (the total distance))
 *
 * It's associated to a GlobalConstraintCore (gc) by adding gc.register(this) at the beginning of your constraint.
 *
 * The following methods must be implemented :
 *     - performPreCompute
 *         --> A method that computes some stuff in order to evaluate the value of a vehicle the fastest possible
 *         --> Can be slow (only performed after a move is accepted)
 *     - computeVehicleValue
 *         --> A method that computes the value of a vehicle given a an explored movement (as a List of Segment)
 *         --> Must be as fast as possible (using your precomputed values)
 *     - assignVehicleValue
 *         --> Update your output variable (ex: routeLength(vehicle) := value)
 *     - computeVehicleValueFromScratch
 *         --> A naive method that computes the value of a vehicle given the route of the problem (IntSequence)
 *         --> Must be correct for it's used for debugging purpose (as the good value)
 */
abstract class GlobalConstraintCore[U <: Any :Manifest](routes: ChangingSeqValue, v: Int)
  extends Invariant with SeqNotificationTarget{

  val n: Int = routes.maxValue+1
  val vehicles: Range = 0 until v

  private var checkpointLevel: Int = -1
  private var checkpointAtLevel0: IntSequence = _
  private val changedVehiclesSinceCheckpoint0 = new IterableMagicBoolArray(v, false)
  private var variableInitiated = false

  // This variable holds the vehicles value at checkpoint 0.
  // It's used to effectively roll-back to this checkpoint 0 when exploring neighborhood
  private var vehiclesValueAtCheckpoint0: Array[U] = Array.empty[U]

  // This variable purpose is to keep the latest computed value of each vehicle
  // So that we can easily update the checkpoint0 value when the movement is accepted
  private var lastComputedVehiclesValue: Array[U] = Array.empty[U]

  // An array holding the ListSegment modifications as a QList
  // Int == checkpoint level, ListSegments the new ListSegment of the vehicle considering the modifications
  // It holds at least one value : the initial value whose checkpoint level == -1
  // Then each time we do a modification to the ListSegment, it's stored with the current checkpoint level
  private var segmentsOfVehicle: Array[VehicleSegments] = Array.fill(v)(null)
  // Each time we define a checkpoint we save the current constraint state including :
  //    - changedVehiclesSinceCheckpoint0
  //    - vehiclesValueAtCheckpoint
  //    - vehicleSearcher
  //    - positionToValueCache
  private var savedDataAtCheckpointLevel: QList[(QList[Int], VehicleLocation, Array[VehicleSegments])] = null
  // Store the position value of a node for a given checkpoint level. -1 == position not computed

  protected var vehicleSearcher: VehicleLocation = VehicleLocation((0 until v).toArray)

  registerStaticAndDynamicDependency(routes)

  finishInitialization()

  /**
   * This method is called by the framework when a pre-computation must be performed.
   * you are expected to assign a value of type T to each node of the vehicle "vehicle" through the method "setNodeValue"
   *
   * @param vehicle         the vehicle where pre-computation must be performed
   * @param routes          the sequence representing the route of all vehicle
   *                        BEWARE,other vehicles are also present in this sequence; you must only work on the given vehicle
   */
  protected def performPreCompute(vehicle: Int, routes: IntSequence): Unit

  /**
   * this method is called by the framework when the value of a vehicle must be computed.
   *
   * @param vehicle         the vehicle that we are focusing on
   * @param segments        the segments that constitute the route.
   *                        The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
   * @param routes          the sequence representing the route of all vehicle
   * @return the value associated with the vehicle
   */
  protected def computeVehicleValue(vehicle: Int, segments: QList[Segment], routes: IntSequence): U

  /**
   * the framework calls this method to assign the value U to he output variable of your invariant.
   * It has been dissociated from the method above because the framework memorizes the output value of the vehicle,
   * and is able to restore old value without the need to re-compute them, so it only will call this assignVehicleValue method
   *
   * @param vehicle the vehicle number
   */
  protected def assignVehicleValue(vehicle: Int, value: U): Unit

  /**
   * This method is defined for verification purpose.
   * It computes the value of the vehicle from scratch.
   *
   * @param vehicle the vehicle on which the value is computed
   * @param routes  the sequence representing the route of all vehicle
   * @return the value of the constraint for the given vehicle
   */
  protected def computeVehicleValueFromScratch(vehicle: Int, routes: IntSequence): U

  /**
   * This method computes the new value of a vehicle from scratch (by calling computeVehicleValueFromScratch),
   * saves it in the lastComputedVehiclesValues variable and
   * finally assign this value to the output CBLS var (using assignVehicleValue method)
   *
   * This method is used at the beginning of the search to set the initial value of the constraint or
   * when we assign the value of the route (not incremental ==> we need to compute from scratch)
   * @param routes The IntSequence representing the route
   */
  private def initVariables(routes: IntSequence): Unit = {
    variableInitiated = true
    lastComputedVehiclesValue = Array.tabulate(v)(vehicle => computeVehicleValueFromScratch(vehicle, routes))
    vehiclesValueAtCheckpoint0 = Array.tabulate(v)(_vehicle => lastComputedVehiclesValue(_vehicle))
  }

  def computeSaveAndAssignVehicleValuesFromScratch(newRoute: IntSequence): Unit = {
    for (vehicle <- 0 until v){
      lastComputedVehiclesValue(vehicle) = computeVehicleValueFromScratch(vehicle, newRoute)
      assignVehicleValue(vehicle, lastComputedVehiclesValue(vehicle))
    }
  }

  override def notifySeqChanges(r: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    val newRoute = routes.newValue
    if(!variableInitiated) initVariables(newRoute)

    if(digestUpdates(changes) && (this.checkpointLevel != -1)){
      QList.qForeach(changedVehiclesSinceCheckpoint0.indicesAtTrueAsQList,(vehicle: Int) => {
        // Compute new vehicle value based on last segment changes
        lastComputedVehiclesValue(vehicle) = computeVehicleValue(vehicle, segmentsOfVehicle(vehicle).segments, newRoute)
        assignVehicleValue(vehicle, lastComputedVehiclesValue(vehicle))
      })
    } else {
      computeSaveAndAssignVehicleValuesFromScratch(newRoute)
    }
  }

  private def digestUpdates(changes:SeqUpdate): Boolean = {
    changes match {
      case SeqUpdateDefineCheckpoint(prev: SeqUpdate, checkpointLevel: Int) =>
        val newRoute = changes.newValue
        val prevUpdate = digestUpdates(prev)

        // Either we got an assign update or this is the very first define checkpoint ==> We must init all vehicles
        if (!prevUpdate || this.checkpointLevel < 0) {
          checkpointAtLevel0 = newRoute // Save the state of the route for further comparison
          computeSaveAndAssignVehicleValuesFromScratch(newRoute)
          (0 until v).foreach(vehicle => {
            vehiclesValueAtCheckpoint0(vehicle) = lastComputedVehiclesValue(vehicle)
            performPreCompute(vehicle, newRoute)
            initSegmentsOfVehicle(vehicle, newRoute)
          })
          // Defining checkpoint 0 ==> we must init every changed since checkpoint 0 vehicles
        } else if (checkpointLevel == 0) {
          checkpointAtLevel0 = newRoute // Save the state of the route for further comparison
          // Computing init ListSegment value for each vehicle that has changed
          QList.qForeach(changedVehiclesSinceCheckpoint0.indicesAtTrueAsQList, (vehicle: Int) => {
            vehiclesValueAtCheckpoint0(vehicle) = lastComputedVehiclesValue(vehicle)
            performPreCompute(vehicle, newRoute)
            initSegmentsOfVehicle(vehicle, newRoute)
          })

          // Resetting the savedData QList.
          savedDataAtCheckpointLevel = null
          changedVehiclesSinceCheckpoint0.all = false

          //TODO: not a good idea to do that so frequently.
          vehicleSearcher = vehicleSearcher.regularize

        }

        // Persisting recent updates of the vehicleSearcher

        // Common manipulations
        this.checkpointLevel = checkpointLevel
        if (savedDataAtCheckpointLevel == null || savedDataAtCheckpointLevel.size == checkpointLevel) {
          savedDataAtCheckpointLevel =
            QList((changedVehiclesSinceCheckpoint0.indicesAtTrueAsQList, vehicleSearcher, segmentsOfVehicle), savedDataAtCheckpointLevel)
          segmentsOfVehicle = savedDataAtCheckpointLevel.head._3.clone()
        }
        true

      case r@SeqUpdateRollBackToCheckpoint(checkpoint: IntSequence, checkpointLevel: Int) =>
        if (checkpointLevel == 0) {
          require(checkpoint quickEquals this.checkpointAtLevel0)
        }

        //Todo: vérifier si on dépile bin les vehicle searcher

        // Restore the saved data corresponding to the required checkpoint
        savedDataAtCheckpointLevel = QList.qDrop(savedDataAtCheckpointLevel, this.checkpointLevel - checkpointLevel)

        // Restoring the vehicle values if it has changed since checkpoint 0 (using current changedVehiclesSinceCheckpoint0 value)
        QList.qForeach(savedDataAtCheckpointLevel.head._1, (value: Int) => changedVehiclesSinceCheckpoint0(value) = false)
        QList.qForeach(changedVehiclesSinceCheckpoint0.indicesAtTrueAsQList, (vehicle: Int) => {
          lastComputedVehiclesValue(vehicle) = vehiclesValueAtCheckpoint0(vehicle)
          assignVehicleValue(vehicle, lastComputedVehiclesValue(vehicle))
        })

        // Restoring all other values
        this.changedVehiclesSinceCheckpoint0.all = false
        QList.qForeach(savedDataAtCheckpointLevel.head._1, (vehicle: Int) => this.changedVehiclesSinceCheckpoint0(vehicle) = true)
        vehicleSearcher = savedDataAtCheckpointLevel.head._2
        segmentsOfVehicle = savedDataAtCheckpointLevel.head._3.clone()

        this.checkpointLevel = checkpointLevel
        true

      case sui@SeqUpdateInsert(value: Int, pos: Int, prev: SeqUpdate) =>
        digestUpdates(prev)

        val prevRoutes = prev.newValue

        val impactedVehicle = vehicleSearcher.vehicleReachingPosition(pos - 1)
        val vehiclePosition = vehicleSearcher.startPosOfVehicle(impactedVehicle)
        val impactedSegment = segmentsOfVehicle(impactedVehicle)


        // InsertSegment insert a segment AFTER a defined position and SeqUpdateInsert at a position => we must withdraw 1
        segmentsOfVehicle(impactedVehicle) = impactedSegment.insertSegments(QList[Segment](NewNode(value).asInstanceOf[Segment]), pos - 1, prevRoutes,vehiclePosition)

        vehicleSearcher = vehicleSearcher.push(sui.oldPosToNewPos)
        changedVehiclesSinceCheckpoint0(impactedVehicle) = true
        true

      case sum@SeqUpdateMove(fromIncluded: Int, toIncluded: Int, after: Int, flip: Boolean, prev: SeqUpdate) =>
        digestUpdates(prev)

        val prevRoutes = prev.newValue

        val fromVehicle = vehicleSearcher.vehicleReachingPosition(fromIncluded)
        val fromVehiclePosition = vehicleSearcher.startPosOfVehicle(fromVehicle)
        val toVehicle = vehicleSearcher.vehicleReachingPosition(after)
        val sameVehicle = fromVehicle == toVehicle
        val toVehiclePosition = if (sameVehicle) fromVehiclePosition else vehicleSearcher.startPosOfVehicle(toVehicle)

        val fromImpactedSegment = segmentsOfVehicle(fromVehicle)

        // Identification of the sub-segments to remove
        val (listSegmentsAfterRemove, segmentsToRemove) =
          fromImpactedSegment.removeSubSegments(fromIncluded, toIncluded, prevRoutes,fromVehiclePosition)

        val toImpactedSegment = if (sameVehicle) listSegmentsAfterRemove else segmentsOfVehicle(toVehicle)
        // If we are in same vehicle and we remove nodes to put them later in the route, the route length before insertion point has shortened
        val delta =
          if (!sameVehicle || after < fromIncluded) 0
          else toIncluded - fromIncluded + 1

        // Insert the sub-segments at his new position
        val listSegmentsAfterInsertion =
          if (flip)
            toImpactedSegment.insertSegments(segmentsToRemove.qMap(_.flip()).reverse, after, prevRoutes,toVehiclePosition , delta)
          else
            toImpactedSegment.insertSegments(segmentsToRemove, after, prevRoutes, toVehiclePosition , delta)

        segmentsOfVehicle(toVehicle) = listSegmentsAfterInsertion
        if (!sameVehicle) segmentsOfVehicle(fromVehicle) = listSegmentsAfterRemove

        vehicleSearcher = vehicleSearcher.push(sum.oldPosToNewPos)

        changedVehiclesSinceCheckpoint0(fromVehicle) = true
        changedVehiclesSinceCheckpoint0(toVehicle) = true
        true

      case sur@SeqUpdateRemove(position: Int, prev: SeqUpdate) =>
        digestUpdates(prev)

        val prevRoutes = prev.newValue

        val impactedVehicle = vehicleSearcher.vehicleReachingPosition(position)
        val vehiclePos = vehicleSearcher.startPosOfVehicle(impactedVehicle)
        val impactedSegment = segmentsOfVehicle(impactedVehicle)

        val (listSegmentAfterRemove, _) = impactedSegment.removeSubSegments(position, position, prevRoutes,vehiclePos)

        segmentsOfVehicle(impactedVehicle) = listSegmentAfterRemove

        vehicleSearcher = vehicleSearcher.push(sur.oldPosToNewPos)
        changedVehiclesSinceCheckpoint0(impactedVehicle) = true
        true

      case SeqUpdateLastNotified(value: IntSequence) =>
        require(value quickEquals routes.value)
        true

      case SeqUpdateAssign(value: IntSequence) =>
        vehicleSearcher = VehicleLocation(Array.tabulate(v)(vehicle => {
          value.positionOfAnyOccurrence(vehicle).get
        }))
        for (vehicle <- 0 until v) initSegmentsOfVehicle(vehicle, value)
        false //impossible to go incremental

      case _ => false //Default case
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

    segmentsOfVehicle(vehicle) = VehicleSegments(
      QList[Segment](PreComputedSubSequence(
        vehicle,
        lastNodeOfVehicle,
        posOfLastNodeOfVehicle - posOfVehicle + 1
      )),n,v)
  }


  override def checkInternals(c : Checker): Unit = {
    for (vehicle <- vehicles) {
      val fromScratch = computeVehicleValueFromScratch(vehicle, routes.value)
      require(fromScratch == lastComputedVehiclesValue(vehicle),
        s"""Constraint ${this.getClass.getName} failed.
           |For Vehicle $vehicle should be $fromScratch got ${lastComputedVehiclesValue(vehicle)} $routes
           |After receiving segments : ${segmentsOfVehicle(vehicle).segments.toList.mkString("\n    ")}""".stripMargin)

      }
  }

}
