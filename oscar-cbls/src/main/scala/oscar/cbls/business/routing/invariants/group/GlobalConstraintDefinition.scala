package oscar.cbls.business.routing.invariants.group

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.core.computation.ChangingSeqValue
import oscar.cbls._

/**
  * In order to mutualise the computation of the global constraint segments between several invariants,
  * those invariants should hold and manipulate their own values : precomputation and vehicle values.
  * The precomputation is perform only at checkpoint 0 and the vehicleComputation at each checkpointLevel (to have a effective roll-back)
  *
  * The GlobalConstraint mechanism then simply call the adequate method to :
  *   - perform precomputation
  *   - compute vehicle value for a given checkpoint level (!! store the result in vehiclesValuesAtCheckpoint
  *   - assign the vehicle value at a given checkpoint level
  */
abstract class GlobalConstraintDefinition(gc: GlobalConstraintCore, v: Int) {

  /**
    * T is the type of the precomputed value associated to each node of the problem.
    * U is the type of the violation value associated to each vehicle of the problem.
    */
  type U
  type AU = Array[U]

  // You must initialize those variable when initiating your invariant
  var vehiclesValueAtCheckpoint0: AU = _
  var currentVehiclesValue: AU = _

  def saveVehicleValue(vehicle: Long, value: U): Unit ={
    currentVehiclesValue(vehicle) = value
  }

  def setCheckpointLevel0Value(vehicle: Int): Unit ={
    vehiclesValueAtCheckpoint0(vehicle) = currentVehiclesValue(vehicle)
  }

  def rollBackToCheckpoint(vehiclesToRollBack: QList[Int]): Unit ={
    QList.qForeach(vehiclesToRollBack, (vehicle: Int) => {
      currentVehiclesValue(vehicle) = vehiclesValueAtCheckpoint0(vehicle)
      assignVehicleValue(vehicle)
    })
  }

  // Initialize the invariant variable using the initial route of the problem
  def init(routes: IntSequence): Unit = {
    for (vehicle <- 0 until v) {
      computeVehicleValueFromScratch(vehicle, routes)
      assignVehicleValue(vehicle)
    }
  }

  /**
    * this method is called by the framework when a pre-computation must be performed.
    * you are expected to assign a value of type T to each node of the vehicle "vehicle" through the method "setNodeValue"
    * @param vehicle the vehicle where pre-computation must be performed
    * @param routes the sequence representing the route of all vehicle
    *               BEWARE,other vehicles are also present in this sequence; you must only work on the given vehicle
    */
  def performPreCompute(vehicle:Long,
                        routes:IntSequence)

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    * When implementing this method, DON'T FORGET to call saveVehicleValue at the end.
    *
    * @param vehicle the vehicle that we are focusing on
    * @param segments the segments that constitute the route.
    *                 The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @param routes the sequence representing the route of all vehicle
    */
  def computeVehicleValue(vehicle:Long,
                          segments:QList[Segment],
                          routes:IntSequence)

  /**
    * The framework calls this method to assign the value U corresponding to a specific checkpointLevel to the output variable of your invariant.
    * It has been dissociated from the method computeVehicleValue because the system should be able to restore a previously computed value without re-computing it.
    * @param vehicle the vehicle number
    */
  def assignVehicleValue(vehicle:Long): Unit

  /**
    * this method is defined for verification purpose. It computes the value of the vehicle from scratch.
    *
    * @param vehicle the vehicle on which the value is computed
    * @param routes the sequence representing the route of all vehicle
    */
  def computeVehicleValueFromScratch(vehicle : Long, routes : IntSequence, save: Boolean = true): U


  def checkInternals(vehicle: Long, routes: ChangingSeqValue, segments: List[Segment]): Unit ={
    val fromScratch = computeVehicleValueFromScratch(vehicle, routes.value, false)
    require(fromScratch.equals(currentVehiclesValue(vehicle)), "Constraint " + this.getClass.getName + " failed " +
    "For Vehicle " + vehicle + " : should be " + fromScratch + " got " +
      currentVehiclesValue(vehicle) + " " + routes + "\nAfter receiving segments : " + segments.mkString("\n    "))
  }

}
