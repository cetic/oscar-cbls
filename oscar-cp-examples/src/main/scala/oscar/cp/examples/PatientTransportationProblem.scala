package oscar.cp.examples

import oscar.algo.branchings.MaxSelectionBranching
import oscar.cp._
/**
  * This problem consists in transporting patients to and from a hospital for medical appointments.
  * It is a specific kind of DARP:
  * The objective is to maximize the number of patients transported. A patient can be transported to an appointment,
  * brought back after the end of the appointment or both. In this last case, if one of the travels is done, the other
  * must be also performed but not necessarily by the same vehicle.
  * The patients can only be transported by compatible vehicles. The places from which and where the patient has to be
  * transported or the starting and ending depots of the vehicles are not necessarily the same.
  *
  * @author Charles Thomas cftmthomas@gmail.com
  */
object PatientTransportationProblem extends CPModel with App {

  /* data */

  val startTime = 0
  val horizon = 400
  val maxWaitTime = 30 //Maximum waiting time for patients before and after appointments.

  val nPatient = 8
  val nVehicle = 2

  val patientRdv = Array(60, 60, 120, 150, 240, 300, 300, 300) //Rdv time
  val patientDur = Array(20, 120, 40, 40, 60, 20, 20, 60) //Rdv duration
  val patientSrv = Array(2, 3, 2, 2, 5, 2, 2, 2) //Service time (represents time needed to embark/disembark vehicle)
  val patientRdvEnd = patientRdv.indices.map(i => patientRdv(i) + patientDur(i))
  val patientLoad = Array(1, 2, 1, 1, 1, 2, 1, 1) //Number of places taken in the vehicle
  val patientCategory = Array(0, 1, 0, 0, 1, 0, 0, 0)

  val vehicleCap = Array(6, 4) //Capacity of vehicles
  val vehicleStartDepot = Array(7, 8)
  val vehicleEndDepot = Array(7, 8)
  val vehicleCompatibility = Array(Set(0), Set(0, 1)) //Categories of patients that the vehicle can take

  //Trip related variables, the forward and backward trips are separated:
  val patientForward = Array(0, 1, 2, 3, 4, 6, 7)
  val patientBackward = Array(0, 1, 2, 3, 4, 5, 6, 7)

  val originForward = Array(9, 10, 11, 13, 14, 16, 17)
  val destForward = Array(0, 1, 2, 3, 4, 6, 6)

  val originBackward = Array(0, 1, 2, 3, 4, 5, 6, 6)
  val destBackward = Array(9, 10, 12, 13, 14, 15, 16, 17)

  val allPatient: Array[Int] = patientForward ++ patientBackward //Patient for each trip
  val allOrigin: Array[Int] = originForward ++ originBackward //Origin for each trip
  val allDest: Array[Int] = destForward ++ destBackward //Destination for each trip

  //Distance matrix, the places are identified by their id in this array:
  val locationMatrix = Array(
    Array(0, 2, 6, 3, 3, 11, 2, 11, 9, 9, 0, 12, 9, 13, 9, 11, 10, 11),
    Array(2, 0, 7, 1, 0, 11, 2, 10, 8, 11, 2, 13, 11, 15, 6, 13, 9, 13),
    Array(6, 7, 0, 8, 8, 9, 7, 11, 10, 4, 6, 10, 5, 8, 13, 6, 13, 6),
    Array(3, 1, 8, 0, 1, 11, 3, 9, 7, 12, 3, 13, 12, 15, 5, 13, 8, 14),
    Array(3, 0, 8, 1, 0, 11, 2, 10, 7, 11, 2, 12, 11, 15, 6, 13, 8, 13),
    Array(11, 11, 9, 11, 11, 0, 12, 5, 6, 12, 11, 2, 13, 7, 14, 7, 9, 9),
    Array(2, 2, 7, 3, 2, 12, 0, 12, 10, 10, 2, 14, 10, 15, 8, 13, 11, 13),
    Array(11, 10, 11, 9, 10, 5, 12, 0, 2, 16, 11, 6, 17, 13, 10, 12, 4, 14),
    Array(9, 8, 10, 7, 7, 6, 10, 2, 0, 14, 8, 7, 15, 13, 8, 12, 3, 13),
    Array(9, 11, 4, 12, 11, 12, 10, 16, 14, 0, 9, 14, 1, 9, 17, 7, 17, 6),
    Array(0, 2, 6, 3, 2, 11, 2, 11, 8, 9, 0, 12, 9, 13, 8, 11, 10, 11),
    Array(12, 13, 10, 13, 12, 2, 14, 6, 7, 14, 12, 0, 15, 8, 15, 7, 10, 10),
    Array(9, 11, 5, 12, 11, 13, 10, 17, 15, 1, 9, 15, 0, 10, 17, 9, 18, 7),
    Array(13, 15, 8, 15, 15, 7, 15, 13, 13, 9, 13, 8, 10, 0, 20, 2, 16, 4),
    Array(9, 6, 13, 5, 6, 14, 8, 10, 8, 17, 8, 15, 17, 20, 0, 18, 7, 19),
    Array(11, 13, 6, 13, 13, 7, 13, 12, 12, 7, 11, 7, 9, 2, 18, 0, 14, 3),
    Array(10, 9, 13, 8, 8, 9, 11, 4, 3, 17, 10, 10, 18, 16, 7, 14, 0, 16),
    Array(11, 13, 6, 14, 13, 9, 13, 14, 13, 6, 11, 10, 7, 4, 19, 3, 16, 0)
  )


  /* variables */

  //Selection variable indicating if the patient is serviced:
  val visited: Array[CPBoolVar] = Array.fill(nPatient)(CPBoolVar())

  //Time related variables for forward trips:
  val (startForward, endForward, durForward) = patientForward.map(p => {
    val start = CPIntVar(math.max(startTime, patientRdv(p) - maxWaitTime) to patientRdv(p))
    val end = CPIntVar(math.max(startTime, patientRdv(p) - maxWaitTime) to patientRdv(p))
    (start, end, end-start)
  }).unzip3

  //Time related variables for backward trips:
  val (startBackward, endBackward, durBackward) = patientBackward.map(p =>{
    val start = CPIntVar(patientRdvEnd(p) to math.min(horizon, patientRdvEnd(p) + maxWaitTime))
    val end = CPIntVar(patientRdvEnd(p) to math.min(horizon, patientRdvEnd(p) + maxWaitTime))
    (start, end, end-start)
  }).unzip3

  val allStart: Array[CPIntVar] = startForward ++ startBackward // duplicated variables
  val allEnd: Array[CPIntVar] = endForward ++ endBackward // duplicated variables
  val allDuration: Array[CPIntVar] = durForward ++ durBackward // duplicated variables

  val vehicleForward: Array[CPIntVar] = patientForward.map(p => CPIntVar(
    (0 until nVehicle).filter(v => vehicleCompatibility(v).contains(patientCategory(p)))
  ))
  val vehicleBackward: Array[CPIntVar] = patientBackward.map(p => CPIntVar(
    (0 until nVehicle).filter(v => vehicleCompatibility(v).contains(patientCategory(p)))
  ))

  val allVehicle: Array[CPIntVar] = vehicleForward ++ vehicleBackward

  val loadVar = allPatient.map(p => CPIntVar(patientLoad(p)))


  /* constraints */

  for (d <- allDuration) add(d >= 0) //Durations must be positive

  //For each trip:
  for(i <- allPatient.indices) {
    val p = allPatient(i)

    //If the patient is visited:

    //Depot travel time and vehicle availability taken into account:
    add(visited(p) ==> (allStart(i) ?>= locationMatrix(i)(vehicleStartDepot(allVehicle(i)))))
    add(visited(p) ==> ((allEnd(i) + patientDur(p)) ?<= (-locationMatrix(i)(vehicleStartDepot(allVehicle(i))) + horizon)))

    //Rdv start and end taken into account:
    if(i < patientForward.length) add(visited(p) ==> (allEnd(i) ?<= patientRdv(p) - patientSrv(p)))
    else add(visited(p) ==> (allStart(i) ?>= patientRdv(p) + patientDur(p)))
  }

  //Setting up travel times between activities:
  for (i <- allPatient.indices; j <- allPatient.indices) {
    if (i != j) {
      add((allVehicle(i) ?=== allVehicle(j)) ==> ((allStart(j) - allStart(i)).abs ?>= (patientSrv(allPatient(i)) + locationMatrix(allOrigin(i))(allOrigin(j)))))
      add((allVehicle(i) ?=== allVehicle(j)) ==> ((allEnd(j) - allEnd(i)).abs ?>= (patientSrv(allPatient(i)) + locationMatrix(allDest(i))(allDest(j)))))
      add((allVehicle(i) ?=== allVehicle(j)) ==> ((allStart(j) - allEnd(i)).abs ?>= (patientSrv(allPatient(i)) + locationMatrix(allDest(i))(allOrigin(j)))))
      add((allVehicle(i) ?=== allVehicle(j)) ==> ((allEnd(j) - allStart(i)).abs ?>= (patientSrv(allPatient(i)) + locationMatrix(allOrigin(i))(allDest(j)))))
    } else {
      add(allDuration(i) ?>= (patientSrv(allPatient(i)) + locationMatrix(allDest(i))(allOrigin(j))))
    }
  }

  //Capacity constraints:
  for(i <- 0 until nVehicle) {
    val capVar = CPIntVar(vehicleCap(i))
    add(maxCumulativeResource(allStart,allDuration,allEnd,loadVar,allVehicle,capVar,i))
  }


  /* Objective function */

  val nServed = sum(visited)
  maximize(nServed)


  /* Search */

  //Linking selection variables with optional variables:
  val decisionVars = (for (p <- 0 until nPatient) yield {
    val indices = allPatient.indices.filter(allPatient(_) == p).toArray
    val selectionVar = visited(p)
    val optionalVars = (for(i <- indices) yield Array(allVehicle(i), allStart(i), allEnd(i))).flatten
    CPOptionalSelection(selectionVar, optionalVars, binaryFirstFail(optionalVars, _.min))
  }).toArray

  search(MaxSelectionBranching(
    decisionVars,
    conflictOrderingSearch(decisionVars.map(_.selectionVar), (i) => -patientRdv(i), _ => 1)
  ))


  /* Solution */

  onSolution {

    println("Patients serviced: " + nServed)

    allPatient.indices.filter(i => visited(allPatient(i)).value == 1).sortBy(i => allStart(i).value).foreach(i => {
      print(if(i < patientForward.length) "Forward" else "Backward")
      print(" travel (id: " + i)
      print(") for patient " + allPatient(i))
      print(" of duration " + allDuration(i).value)
      print(" and load " + patientLoad(allPatient(i)))
      print(" is executed on vehicle " + allVehicle(i).value)
      print(" at time " + allStart(i).value + ":" + allEnd(i).value)
      println()
    })

    println("------------")

  }

  println(start())
}
