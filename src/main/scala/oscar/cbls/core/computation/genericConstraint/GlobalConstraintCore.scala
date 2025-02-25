// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.core.computation.genericConstraint

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.Invariant
import oscar.cbls.core.computation.genericConstraint.segment.{Segment, VehicleSegments}
import oscar.cbls.core.computation.seq._
import oscar.cbls.modeling.routing.{StackedVehicleSearcher, VRS}

import scala.collection.immutable.{HashMap, HashSet}

/** This abstract class must be extended in order to define a new global constraint.<br>
  *
  * It uses a parametrized type `U` that represents the output type of the constraint, for example,
  * `Long` for `RouteLength` (the total distance).
  *
  * The following methods must be implemented:
  *   - [[performPrecomputation]]
  *     - Precomputes some data in order to evaluate the value of a vehicle the fastest possible.
  *     - Can be slow (only performed when a level 0 checkpoint is defined).
  *   - [[computeVehicleValue]]
  *     - Computes the value of vehicle given a list of segments.
  *     - Must be as fast as possible (using the precomputed values).
  *   - [[assignVehicleValue]]
  *     - Updates the output variable (ex: `routeLength(vehicle) := value`)
  *   - [[computeVehicleValueFromScratch]]
  *     - Naive method that computes the value of a vehicle given the associated routes (as an
  *       [[oscar.cbls.algo.sequence.IntSequence]])
  *
  * @param vrs
  *   The object that represents the vehicle routing structure.
  * @param name
  *   The (optional) name of the Invariant.
  * @tparam U
  *   Parametrized type that represents the output type of the constraint, for example, `Long` for
  *   `RouteLength` (the total distance).
  */
abstract class GlobalConstraintCore[U: Manifest](vrs: VRS, name: Option[String])
    extends Invariant(vrs.store, name)
    with SeqNotificationTarget {

  vrs.routes.registerStaticallyAndDynamicallyListeningElement(this)

  /** This variable keeps the last value computed for each vehicle. It eases the update of the
    * output variable and the implementation of `checkInternals`.
    */
  private[this] val vehiclesValues: Array[U] = new Array[U](vrs.v)

  computeSaveAndAssignVehicleValuesFromScratch(vrs.routes.value()) // Init the output

  protected var vehicleSearcher: StackedVehicleSearcher =
    StackedVehicleSearcher(vrs.routes.value(), vrs.v)

  /** This variable holds all the vehicles that have their routes changed since the level 0
    * checkpoint definition.
    */
  private[this] var changedVehiclesSinceCheckpoint0: HashSet[Int] = HashSet.empty

  /** The updated list of Segments which defines the precomputation that can used. */
  private[this] var segmentsOfVehicle: HashMap[Int, VehicleSegments] = initSegments(
    vrs.routes.value()
  )

  /** A stack of values saved at each checkpoint.<br>
    *
    * At each checkpoint, we save:
    *   - The set of vehicles changed since checkpoint 0.
    *   - The segments of each vehicle.
    */
  private[this] val savedDataAtCheckpoints: SeqValueStack[SavedValuesAtCheckpoints] =
    new SeqValueStack[SavedValuesAtCheckpoints]()

  /** Flag to know if precomputations are available. */
  private[this] var precomputationAvailable: Boolean = false

  // This variable holds the vehicles value at checkpoint 0
  // It's used to effectively roll back to the checkpoint 0
  private[this] val vehiclesValuesAtCheckpoint0: Array[U] =
    Array.tabulate(vrs.v)(vehicle => vehiclesValues(vehicle))

  /** Method called by the framework when precomputation must be performed. <br>
    *
    * These precomputations will be used by the `computeVehicleValue` method.
    * @param vehicle
    *   The vehicle on which precomputations have to be performed.
    * @param routes
    *   The sequence representing the route of '''all''' the vehicles. '''Warning:''' other vehicle
    *   are also present on this sequence; be sure to only work on the given vehicle.
    */
  protected def performPrecomputation(vehicle: Int, routes: IntSequence): Unit

  /** Method called by the framework when the value of a vehicle must be computed.<br>
    *
    * The method calls the precomputation.
    * @param vehicle
    *   The vehicle to focus on.
    * @param segments
    *   The segments that constitute the route. The route of the given vehicle is equal to the
    *   concatenation of all the given segment in order.
    * @param routes
    *   The sequence representing the route of '''all''' the vehicles.
    */
  protected def computeVehicleValue(vehicle: Int, segments: List[Segment], routes: IntSequence): U

  /** Method called by the framework to assign a value of type `U` to the output variable of the
    * invariant.
    *
    * @param vehicle
    *   The vehicle to focus on.
    * @param value
    *   The value to assign.
    */
  protected def assignVehicleValue(vehicle: Int, value: U): Unit

  /** Method defined for verification purpose or when precomputations are unavailable.
    *
    * @param vehicle
    *   The vehicle on which the value is computed.
    * @param routes
    *   The sequence representing the route of '''all''' the vehicles.
    * @return
    *   The value of the invariant for the given vehicle.
    */
  protected def computeVehicleValueFromScratch(vehicle: Int, routes: IntSequence): U

  override def notifySeqChanges(
    seqVariable: SeqVariable,
    contextualVarIndex: Int,
    changes: SeqUpdate
  ): Unit = {
    if (digestUpdates(changes)) {
      // Precomputation can be used.
      for (vehicle <- changedVehiclesSinceCheckpoint0) {
        vehiclesValues(vehicle) = computeVehicleValue(
          vehicle,
          segmentsOfVehicle(vehicle).segments,
          seqVariable.pendingValue
        )
        assignVehicleValue(vehicle, vehiclesValues(vehicle))
      }
    } else {
      // An Assign has been performed or no level 0 checkpoint has been defined.
      // We cannot use precomputation.
      computeSaveAndAssignVehicleValuesFromScratch(seqVariable.pendingValue)
      segmentsOfVehicle = initSegments(seqVariable.pendingValue)
    }
  }

  override def checkInternals(): Unit = {
    for (vehicle <- 0 until vrs.v) {
      val expected = computeVehicleValueFromScratch(vehicle, vrs.routes.pendingValue)
      require(
        expected == vehiclesValues(vehicle),
        s"""Constraint ${this.getClass.getName} failed for vehicle $vehicle.
           |Expected: $expected
           |Got: ${vehiclesValues(vehicle)}
           |Sequence: ${vrs.routes.pendingValue}
           |Current segments: ${segmentsOfVehicle(vehicle)}
           |""".stripMargin
      )
    }
  }

  /** Method that computes values and assigns to the output variables from scratch.
    *
    * @param route
    *   The sequence representing the route of '''all''' the vehicles.
    */
  private[this] def computeSaveAndAssignVehicleValuesFromScratch(route: IntSequence): Unit = {
    for (vehicle <- 0 until vrs.v) {
      vehiclesValues(vehicle) = computeVehicleValueFromScratch(vehicle, route)
      assignVehicleValue(vehicle, vehiclesValues(vehicle))
    }
  }

  private[this] def initSegments(routes: IntSequence): HashMap[Int, VehicleSegments] = {
    var toReturn: HashMap[Int, VehicleSegments] = HashMap.empty

    for (vehicle <- 0 until vrs.v)
      toReturn += (vehicle -> VehicleSegments(routes, vehicle, vrs.v))

    toReturn
  }

  private[this] def performPrecomputationComputeValuesAndAssign(
    vehicle: Int,
    route: IntSequence
  ): Unit = {
    performPrecomputation(vehicle, route)
    segmentsOfVehicle += (vehicle -> VehicleSegments(route, vehicle, vrs.v))
    vehiclesValues(vehicle) =
      computeVehicleValue(vehicle, segmentsOfVehicle(vehicle).segments, route)
    assignVehicleValue(vehicle, vehiclesValues(vehicle))
    vehiclesValuesAtCheckpoint0(vehicle) = vehiclesValues(vehicle)
  }

  private[this] def digestUpdates(changes: SeqUpdate): Boolean = {

    changes match {
      case defineCheckpointUpdate @ SeqUpdateDefineCheckpoint(
            prev: SeqUpdate,
            checkpointLevel: Int
          ) =>
        val newRoute = defineCheckpointUpdate.newValue
        val digested = digestUpdates(prev)

        if (!digested || !precomputationAvailable) {
          // A previous update was an assign, or we never have performed precomputation.
          // We need to perform precomputations on all the vehicles
          for (vehicle <- 0 until vrs.v)
            performPrecomputationComputeValuesAndAssign(vehicle, newRoute)

          precomputationAvailable = true
          changedVehiclesSinceCheckpoint0 = HashSet.empty
        } else if (checkpointLevel == 0) {
          // We redefine a checkpoint at level 0. Precomputations have already been performed.
          // They must be redone only for modified vehicles.
          for (vehicle <- changedVehiclesSinceCheckpoint0)
            performPrecomputationComputeValuesAndAssign(vehicle, newRoute)

          changedVehiclesSinceCheckpoint0 = HashSet.empty
        }

        vehicleSearcher = vehicleSearcher.defineCheckpoint()
        savedDataAtCheckpoints.pushToLevel(
          newRoute,
          checkpointLevel,
          SavedValuesAtCheckpoints(changedVehiclesSinceCheckpoint0, segmentsOfVehicle)
        )

        true

      case releaseUpdate: SeqUpdateReleaseTopCheckpoint =>
        val digested = digestUpdates(releaseUpdate.prev)
        savedDataAtCheckpoints.pop()
        vehicleSearcher = vehicleSearcher.releaseTopCheckpoint()
        digested

      case SeqUpdateRollBackToTopCheckpoint(
            checkpoint: IntSequence,
            howToRollback: SeqUpdate,
            level: Int,
            _
          ) =>
        rollbackUpdates(howToRollback)
        assert(
          savedDataAtCheckpoints.stackLevel == level,
          s"Checkpoint levels are not coherent (sequence level: $level != this invariant " +
            s"level: ${savedDataAtCheckpoints.stackLevel})"
        )

        val savedData = savedDataAtCheckpoints.topValue(checkpoint)

        // Gets the vehicles that have only changed since the top checkpoint
        val changedExclusivelySinceLastCheckpoint =
          changedVehiclesSinceCheckpoint0.diff(savedData.changedSinceCheckpoint0)
        // Restores the value of the concerned vehicles
        changedExclusivelySinceLastCheckpoint.foreach(vehicle => {
          vehiclesValues(vehicle) = vehiclesValuesAtCheckpoint0(vehicle)
          assignVehicleValue(vehicle, vehiclesValues(vehicle))
        })

        // Restores the other values
        changedVehiclesSinceCheckpoint0 = savedData.changedSinceCheckpoint0
        segmentsOfVehicle = savedData.segmentsOfVehicle
        vehicleSearcher = vehicleSearcher.rollbackToTopCheckpoint()

        true // No assign can be performed when a checkpoint is defined. So a rollback is always digested
      case insertUpdate @ SeqUpdateInsert(
            toInsert: Int,
            insertAfterPosExp: IntSequenceExplorer,
            prev: SeqUpdate
          ) =>
        val digested: Boolean = digestUpdates(prev)

        if (digested) {
          // If an assign was performed, we can avoid the following updates.
          // All the values will be computed from scratch or a new checkpoint will be defined.
          val impactedVehicle = vehicleSearcher.vehicleReachingPosition(insertAfterPosExp.position)
          val startPosOfImpactedVehicle = vehicleSearcher.startPosOfVehicle(impactedVehicle)
          val impactedSegments          = segmentsOfVehicle(impactedVehicle)

          segmentsOfVehicle +=
            (impactedVehicle -> impactedSegments.insertNode(
              toInsert,
              insertAfterPosExp,
              startPosOfImpactedVehicle
            ))

          changedVehiclesSinceCheckpoint0 += impactedVehicle
        }
        vehicleSearcher = vehicleSearcher.push(insertUpdate.oldPosToNewPos)

        digested

      case removeUpdate @ SeqUpdateRemove(toRemoveExp: IntSequenceExplorer, prev: SeqUpdate) =>
        val digested = digestUpdates(prev)

        if (digested) {
          // If an assign was performed, we can avoid the following updates.
          // All the values will be computed from scratch or a new checkpoint will be defined.
          val impactedVehicle        = vehicleSearcher.vehicleReachingPosition(toRemoveExp.position)
          val startOfImpactedVehicle = vehicleSearcher.startPosOfVehicle(impactedVehicle)
          val impactedSegments       = segmentsOfVehicle(impactedVehicle)

          segmentsOfVehicle += (impactedVehicle -> impactedSegments
            .removeNode(toRemoveExp, startOfImpactedVehicle)
            ._1)
          changedVehiclesSinceCheckpoint0 += impactedVehicle
        }
        vehicleSearcher = vehicleSearcher.push(removeUpdate.oldPosToNewPos)

        digested

      case moveUpdate @ SeqUpdateMove(
            fromExp: IntSequenceExplorer,
            toExp: IntSequenceExplorer,
            afterPosExp: IntSequenceExplorer,
            flip: Boolean,
            prev: SeqUpdate
          ) =>
        val digested = digestUpdates(prev)

        if (digested) {
          // If an assign was performed, we can avoid the following updates.
          // All the values will be computed from scratch or a new checkpoint will be defined.
          val fromImpactedVehicle = vehicleSearcher.vehicleReachingPosition(fromExp.position)
          val startOfFromVehicle  = vehicleSearcher.startPosOfVehicle(fromImpactedVehicle)
          val fromSegments        = segmentsOfVehicle(fromImpactedVehicle)
          val toImpactedVehicle   = vehicleSearcher.vehicleReachingPosition(afterPosExp.position)
          val startOfToVehicle    = vehicleSearcher.startPosOfVehicle(toImpactedVehicle)
          val toSegments          = segmentsOfVehicle(toImpactedVehicle)

          val (updatedFrom, updatedTo) = VehicleSegments.moveSegments(
            startOfFromVehicle,
            startOfToVehicle,
            fromSegments,
            toSegments,
            fromExp,
            toExp,
            afterPosExp,
            flip
          )

          segmentsOfVehicle += (toImpactedVehicle -> updatedTo)
          changedVehiclesSinceCheckpoint0 += toImpactedVehicle
          if (fromImpactedVehicle != toImpactedVehicle) {
            segmentsOfVehicle += (fromImpactedVehicle -> updatedFrom)
            changedVehiclesSinceCheckpoint0 += fromImpactedVehicle
          }
        }
        vehicleSearcher = vehicleSearcher.push(moveUpdate.oldPosToNewPos)

        digested

      case _: SeqUpdateLastNotified      => precomputationAvailable
      case assignUpdate: SeqUpdateAssign =>
        // All new route. Precomputations are expired, and we have to reset the vehicle searcher
        precomputationAvailable = false
        vehicleSearcher = StackedVehicleSearcher(assignUpdate.newValue, vrs.v)
        false
      case x: SeqUpdate => throw new IllegalArgumentException(s"Unexpected update $x")

    }
  }

  // When rolling back, we only need to digest the "release top checkpoint" to maintains
  // a good stack level and the other rollbacks. Other updates can be ignored.
  private[this] def rollbackUpdates(howToRollback: SeqUpdate): Unit = {
    howToRollback match {
      case release: SeqUpdateReleaseTopCheckpoint =>
        rollbackUpdates(release.prev)
        savedDataAtCheckpoints.pop()
        vehicleSearcher = vehicleSearcher.releaseTopCheckpoint()

      case SeqUpdateRollBackToTopCheckpoint(
            checkpoint: IntSequence,
            howToRollback: SeqUpdate,
            level: Int,
            _
          ) =>
        rollbackUpdates(howToRollback)
        assert(
          savedDataAtCheckpoints.stackLevel == level,
          s"Checkpoint levels are not coherent (sequence level: $level != this " +
            s"invariant level: ${savedDataAtCheckpoints.stackLevel})"
        )
        savedDataAtCheckpoints.popUntilLevel(checkpoint, level)
        vehicleSearcher = vehicleSearcher.rollbackToTopCheckpoint()
      case _: SeqUpdateLastNotified =>
      case x: SeqUpdateWithPrev     => rollbackUpdates(x.prev)
      case x: SeqUpdate => throw new IllegalArgumentException(s"Unexpected rollback: $x")
    }
  }

  /** Little case class to save the set of vehicle changes since checkpoint 0 and the segments of
    * each vehicle.
    */
  private[this] case class SavedValuesAtCheckpoints(
    changedSinceCheckpoint0: HashSet[Int],
    segmentsOfVehicle: HashMap[Int, VehicleSegments]
  )

}
