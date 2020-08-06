package oscar.cbls.business.routing.invariants.global

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.core.computation.ChangingSeqValue

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
  *
  * How does it works ?
  *     The GlobalConstraintCore (gc) associated with this global constraint manages all the process of the global constraint :
  *       - Converting route's moves into segment
  *       - Calling the right method at the right time
  *       - ...
  *     When you implement a new global constraint you just need to implements the four methods above and
  *     associate this constraint to the GlobalConstraintCore (gc.register(this)) it does the rest.
  *
  * @param gc The GlobalConstraintCore you want to associate this constraint to
  * @param v The number of vehicle
  */
abstract class GlobalConstraintDefinition[U <: Any :Manifest](gc: GlobalConstraintCore, v: Int) {

  // This variable holds the vehicles value at checkpoint 0.
  // It's used to effectively roll-back to this checkpoint 0 when exploring neighborhood
  private var vehiclesValueAtCheckpoint0: Array[U] = Array.empty[U]

  // This variable purpose is to keep the latest computed value of each vehicle
  // So that we can easily update the checkpoint0 value when the movement is accepted
  private var lastComputedVehiclesValue: Array[U] = Array.empty[U]

  /**
    * Update the checkpoint level 0 value of the given vehicle.
    * Used when the tested movement is accepted or at the beginning of the search
    * @param vehicle the vehicle whose checkpoint level 0 value needs to be updated
    */
  private[global] def setCheckpointLevel0Value(vehicle: Int): Unit ={
    if(vehiclesValueAtCheckpoint0.isEmpty)
      vehiclesValueAtCheckpoint0 = Array.tabulate(v)(_vehicle => lastComputedVehiclesValue(_vehicle))
    else
      vehiclesValueAtCheckpoint0(vehicle) = lastComputedVehiclesValue(vehicle)
  }

  /**
    * Roll-back the current value of the specified vehicle to their checkpoint level 0 value.
    * It's used after evaluating a movement.
    * @param vehiclesToRollBack the vehicles whose current value needs to be roll-backed
    */
  private[global] def rollBackToCheckpoint(vehiclesToRollBack: QList[Int]): Unit ={
    QList.qForeach(vehiclesToRollBack, (vehicle: Int) => {
      lastComputedVehiclesValue(vehicle) = vehiclesValueAtCheckpoint0(vehicle)
      assignVehicleValue(vehicle, lastComputedVehiclesValue(vehicle))
    })
  }

  /**
    * This method computes the new value of a vehicle from scratch (by calling computeVehicleValueFromScratch),
    * saves it in the lastComputedVehiclesValues variable and
    * finally assign this value to the output CBLS var (using assignVehicleValue method)
    *
    * This method is used at the beginning of the search to set the initial value of the constraint or
    * when we assign the value of the route (not incremental ==> we need to compute from scratch)
    * @param routes The IntSequence representing the route
    */
  private[global] def computeSaveAndAssignVehicleValuesFromScratch(routes: IntSequence): Unit = {
    if(lastComputedVehiclesValue.isEmpty)
      lastComputedVehiclesValue = Array.tabulate(v)(vehicle => computeVehicleValueFromScratch(vehicle, routes))
    else
      for (vehicle <- 0 until v) {
        lastComputedVehiclesValue(vehicle) = computeVehicleValueFromScratch(vehicle, routes)
        assignVehicleValue(vehicle, lastComputedVehiclesValue(vehicle))
      }
  }

  /**
    * This method computes the new value of a vehicle (by calling computeVehicleValue),
    * saves it in the lastComputedVehiclesValues variable and
    * finally assign this value to the output CBLS var (using assignVehicleValue method)
    *
    * This method is used during the exploration of the neighborhood
    * @param routes The IntSequence representing the route
    */
  private[global] def computeSaveAndAssingVehicleValue(vehicle:Int,
                                                segments:QList[Segment],
                                                routes:IntSequence): Unit ={
    lastComputedVehiclesValue(vehicle) = computeVehicleValue(vehicle, segments, routes)
    assignVehicleValue(vehicle, lastComputedVehiclesValue(vehicle))
  }

  /**
    * This method is called by the framework when a pre-computation must be performed.
    * @param vehicle the vehicle for which a pre-computation must be performed
    * @param routes the sequence representing the route of all vehicle
    *               BEWARE,other vehicles are also present in this sequence; you must only work on the given vehicle
    */
  protected[global] def performPreCompute(vehicle:Int, routes:IntSequence): Unit

  /**
    * This method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle the vehicle for which we must compute the value
    * @param segments the segments that constitute the route.
    *                 The route of the vehicle is equal to the concatenation of all given segments in the order they appear in this list
    * @param routes the sequence representing the route of all vehicle
    */
  protected def computeVehicleValue(vehicle:Int,
                          segments:QList[Segment],
                          routes:IntSequence): U

  /**
    * The framework calls this method to assign the value U corresponding to a specific checkpointLevel to the output variable of your invariant.
    * It has been dissociated from the method computeVehicleValue because the system should be able to restore a previously computed value without re-computing it.
    * @param vehicle the vehicle number
    * @param value The value to assign to the output variable
    */
  protected def assignVehicleValue(vehicle:Int, value: U): Unit

  /**
    * This method is mainly defined for verification purpose.
    * But it's also used when we can't compute the vehicle value incrementally
    * (at the beginning of the search or when we assign the value of the route)
    * It computes the value of the vehicle from scratch.
    *
    * @param vehicle the vehicle on which the value is computed
    * @param routes the sequence representing the route of all vehicle
    */
  protected[global] def computeVehicleValueFromScratch(vehicle : Int, routes : IntSequence): U

  /**
    * This method is used for debugging purpose. (See Checker class)
    * It compares the lastComputedvalue of each vehicle to the value from scratch of the same vehicle.
    * Usually the from scratch algorithm is much more easier to implement therefore it's considered as correct
    * @param vehicle The vehicle whose current value must be checked
    * @param routes The sequence representing the route of all vehicle
    * @param segments The list of segments used to compute the current value fo the vehicle
    */
  protected[global] def checkInternals(vehicle: Int, routes: ChangingSeqValue, segments: List[Segment]): Unit ={
    val fromScratch = computeVehicleValueFromScratch(vehicle, routes.value)
    require(fromScratch.equals(lastComputedVehiclesValue(vehicle)),
      s"""Constraint ${this.getClass.getName} failed.
         |For Vehicle $vehicle should be $fromScratch got ${lastComputedVehiclesValue(vehicle)} $routes
         |After receiving segments : ${segments.mkString("\n    ")}""".stripMargin)
  }

}
