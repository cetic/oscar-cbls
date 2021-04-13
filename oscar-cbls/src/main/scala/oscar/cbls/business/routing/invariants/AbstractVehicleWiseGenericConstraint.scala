package oscar.cbls.business.routing.invariants

import oscar.cbls.algo.magicArray.IterableMagicBoolArray
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.model.VehicleLocation
import oscar.cbls.core.{Checker, Invariant}
import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.Checker

/**
 * This abstract class purpose is to ease the implementation of constraints that only need the route on which a modification occurred.
 * For instance if you have a very complicated computation that's not supported by another constraint.
 *
 * You simply need to implement three methods and the abstract class do the rest :
 *
 * @param routes The routes of the problem
 * @param v The number of vehicles
 * @tparam U The value type of your constraint (Int, Boolean...)
 */
abstract class AbstractVehicleWiseGenericConstraint[U <: Any : Manifest](routes: ChangingSeqValue, v: Int, values: Iterable[IntValue] = Iterable.empty)
  extends Invariant with SeqNotificationTarget {
  val n: Int = routes.maxValue + 1
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

  // Each time we define a checkpoint we save the current constraint state including :
  //    - changedVehiclesSinceCheckpoint0
  //    - vehiclesValueAtCheckpoint
  //    - vehicleSearcher
  private var savedDataAtCheckpointLevel: QList[(QList[Int], VehicleLocation)] = null
  // Store the position value of a node for a given checkpoint level. -1 == position not computed

  protected var vehicleSearcher: VehicleLocation = VehicleLocation((0 until v).toArray)

  registerStaticAndDynamicDependency(routes)
  for(value <- values){
    registerStaticAndDynamicDependency(value)
  }

  finishInitialization()

  /**
   * this method is called by the framework when the value of a vehicle must be computed.
   *
   * @param vehicle the vehicle that we are focusing on
   * @param routes  the sequence representing the route of all vehicle
   * @return the value associated with the vehicle
   */
  protected def computeVehicleValue(vehicle: Int, vehicleStartPos: Int, vehicleRouteSize: Int, routes: IntSequence): U

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
   *
   * @param routes The IntSequence representing the route
   */
  private def initVariables(routes: IntSequence): Unit = {
    variableInitiated = true
    lastComputedVehiclesValue = Array.tabulate(v)(vehicle => computeVehicleValueFromScratch(vehicle, routes))
    vehiclesValueAtCheckpoint0 = Array.tabulate(v)(_vehicle => lastComputedVehiclesValue(_vehicle))
  }

  def computeSaveAndAssignVehicleValuesFromScratch(newRoute: IntSequence): Unit = {
    for (vehicle <- 0 until v) {
      lastComputedVehiclesValue(vehicle) = computeVehicleValueFromScratch(vehicle, newRoute)
      assignVehicleValue(vehicle, lastComputedVehiclesValue(vehicle))
    }
  }

  override def notifySeqChanges(r: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    val newRoute = routes.newValue
    if (!variableInitiated) initVariables(newRoute)

    if (digestUpdates(changes) && (this.checkpointLevel != -1)) {
      QList.qForeach(changedVehiclesSinceCheckpoint0.indicesAtTrueAsQList, (vehicle: Int) => {
        // Compute new vehicle value based on last segment changes
        val vehicleStartPos = vehicleSearcher.startPosOfVehicle(vehicle)
        val vehicleRouteSize = (if (vehicle + 1 == v) newRoute.size else vehicleSearcher.startPosOfVehicle(vehicle + 1)) - vehicleStartPos
        lastComputedVehiclesValue(vehicle) = computeVehicleValue(vehicle, vehicleStartPos, vehicleRouteSize, newRoute)
        assignVehicleValue(vehicle, lastComputedVehiclesValue(vehicle))
      })
    } else {
      computeSaveAndAssignVehicleValuesFromScratch(newRoute)
    }
  }

  private def digestUpdates(changes: SeqUpdate): Boolean = {
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
          })
          // Defining checkpoint 0 ==> we must init every changed since checkpoint 0 vehicles
        } else if (checkpointLevel == 0) {
          checkpointAtLevel0 = newRoute // Save the state of the route for further comparison
          // Computing init ListSegment value for each vehicle that has changed
          QList.qForeach(changedVehiclesSinceCheckpoint0.indicesAtTrueAsQList, (vehicle: Int) => {
            vehiclesValueAtCheckpoint0(vehicle) = lastComputedVehiclesValue(vehicle)
          })

          // Resetting the savedData QList.
          savedDataAtCheckpointLevel = null
          changedVehiclesSinceCheckpoint0.all = false
        }

        // Persisting recent updates of the vehicleSearcher
        vehicleSearcher = vehicleSearcher.regularize

        // Common manipulations
        this.checkpointLevel = checkpointLevel
        if (savedDataAtCheckpointLevel == null || savedDataAtCheckpointLevel.size == checkpointLevel)
          savedDataAtCheckpointLevel =
            QList((changedVehiclesSinceCheckpoint0.indicesAtTrueAsQList, vehicleSearcher), savedDataAtCheckpointLevel)
        true

      case r@SeqUpdateRollBackToCheckpoint(checkpoint: IntSequence, checkpointLevel: Int) =>
        if (checkpointLevel == 0) {
          require(checkpoint quickEquals this.checkpointAtLevel0)
        }

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


        this.checkpointLevel = checkpointLevel
        true

      case sui@SeqUpdateInsert(value: Int, pos: Int, prev: SeqUpdate) =>
        digestUpdates(prev)

        val impactedVehicle = vehicleSearcher.vehicleReachingPosition(pos - 1)

        vehicleSearcher = vehicleSearcher.push(sui.oldPosToNewPos)
        changedVehiclesSinceCheckpoint0(impactedVehicle) = true
        true

      case sum@SeqUpdateMove(fromIncluded: Int, toIncluded: Int, after: Int, flip: Boolean, prev: SeqUpdate) =>
        digestUpdates(prev)

        val fromVehicle = vehicleSearcher.vehicleReachingPosition(fromIncluded)
        val toVehicle = vehicleSearcher.vehicleReachingPosition(after)

        vehicleSearcher = vehicleSearcher.push(sum.oldPosToNewPos)
        changedVehiclesSinceCheckpoint0(fromVehicle) = true
        changedVehiclesSinceCheckpoint0(toVehicle) = true
        true

      case sur@SeqUpdateRemove(position: Int, prev: SeqUpdate) =>
        digestUpdates(prev)

        val impactedVehicle = vehicleSearcher.vehicleReachingPosition(position)

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
        false //impossible to go incremental
    }
  }


  override def checkInternals(c: Checker): Unit = {
    for (vehicle <- vehicles) {
      val fromScratch = computeVehicleValueFromScratch(vehicle, routes.value)
      require(fromScratch.equals(lastComputedVehiclesValue(vehicle)),
        s"""Constraint ${this.getClass.getName} failed.
           |For Vehicle $vehicle should be $fromScratch got ${lastComputedVehiclesValue(vehicle)} $routes """.stripMargin)
    }
  }
}