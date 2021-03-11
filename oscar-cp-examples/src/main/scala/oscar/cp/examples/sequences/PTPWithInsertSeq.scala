package oscar.cp.examples.sequences

import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints.AlternativeActivities
import oscar.cp.constraints.sequence._
import oscar.cp.core.variables.{CPInsertSeqVar, CPSeqVar}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


/**
 * This problem consists in transporting patients to and from a hospital for medical appointments.
 * It is a specific kind of DARP:
 * The objective is to maximize the number of patients transported. A patient can be transported to an appointment,
 * brought back after the end of the appointment or both. In this last case, if one of the travels is done, the other
 * must be also performed but not necessarily by the same vehicle.
 * The patients can only be transported by compatible vehicles. The places from which and where the patient has to be
 * transported or the starting and ending depots of the vehicles are not necessarily the same.
 *
 * This model uses insertion sequence variables
 *
 * @author Charles Thomas cftmthomas@gmail.com
 */
object PTPWithInsertSeq extends CPModel with App {

  /* data */

  val startTime = 0
  val endTime = 400
  val maxWaitTime = 30 //Maximum waiting time for patients before and after appointments.

  val nPatient = 8
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
  val vehicleStartWindow = Array(0, 0) //Start window of vehicle
  val vehicleEndWindow = Array(400, 400) //End window of vehicle
  val nVehicle = vehicleCap.length

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

  def distances(i: Int)(j: Int): Int = {
    if (i == j || i == -1 || j == -1) 0
    else locationMatrix(i)(j)
  }

  val stopBuffer: ArrayBuffer[Stop] = ArrayBuffer[Stop]()
  val travelBuffer: ArrayBuffer[(Int, Int, Boolean)] = ArrayBuffer[(Int, Int, Boolean)]() //stop1 (id), stop2 (id), forward

  //Generating stops for each patient:
  for (i <- patientForward.indices) {
    val p = patientForward(i)
    val lstFor = patientRdv(p) - patientSrv(p) * 2 - distances(originForward(i))(destForward(i))

    if (patientRdv(p) < startTime || patientRdv(p) > endTime || lstFor < startTime) {
      println("Warning! Patient " + p + " is infeasible!")
    } else {
      val est = math.max(startTime, math.min(patientRdv(p) - maxWaitTime, lstFor))
      val ect = est + patientSrv(p) + distances(originForward(i))(destForward(i))
      val lct = patientRdv(p) - patientSrv(p)
      stopBuffer += Stop(i, originForward(i), est, lstFor, 0)
      stopBuffer += Stop(i, destForward(i), ect, lct, 1)
      travelBuffer += ((stopBuffer.size - 2, stopBuffer.size - 1, true))
    }
  }

  for (i <- patientBackward.indices) {
    val p = patientBackward(i)
    val ectBack = patientRdvEnd(p) + patientSrv(p) + distances(originBackward(i))(destBackward(i))

    if (patientRdv(p) < startTime || patientRdv(p) > endTime || ectBack > endTime) {
      println("Warning! Patient " + p + " is infeasible!")
    } else {
      val est = patientRdvEnd(p)
      val lct = math.min(endTime, math.max(est + maxWaitTime, ectBack))
      val lst = lct - distances(originBackward(i))(destBackward(i)) - patientSrv(p)
      stopBuffer += Stop(i, originBackward(i), est, lst, 2)
      stopBuffer += Stop(i, destBackward(i), ectBack, lct, 3)
      travelBuffer += ((stopBuffer.size - 2, stopBuffer.size - 1, false))
    }
  }

  val nStop: Int = stopBuffer.length

  //Generating stop for start depot:
  for (v <- vehicleCap.indices)
    stopBuffer += Stop(-1, vehicleStartDepot(v), math.max(startTime, vehicleStartWindow(v)), math.min(endTime, vehicleEndWindow(v)), 0)

  //Generating stop for end depot:
  for (v <- vehicleCap.indices)
    stopBuffer += Stop(-1, vehicleEndDepot(v), math.max(startTime, vehicleStartWindow(v)), math.min(endTime, vehicleEndWindow(v)), 3)

  val sites: Array[Stop] = stopBuffer.toArray
  val travels: Array[(Int, Int, Boolean)] = travelBuffer.toArray
  val travelMapping: Map[Int, Int] = travels.indices.flatMap(t => Array((travels(t)._1, t), (travels(t)._2, t))).toMap
  val travelPatient: Array[Int] = travels.map(t => sites(t._1).patient)

  val nSite: Int = sites.length

  val minTransportTime: Array[Int] = Array.tabulate(travels.length)(i => locationMatrix(allOrigin(i))(allDest(i)) + patientSrv(allPatient(i)) * 2)

  val transMatrix = Array.tabulate(nSite, nSite)((i, j) => {
    if (i == j) 0
    else distances(sites(i).place)(sites(j).place)
  })


  /* variables */

  val visited: Array[CPBoolVar] = Array.fill(nPatient)(CPBoolVar())

  //Stop and travel variables:

  val arrival: Array[CPIntVar] = Array.tabulate(nSite)(i => {
    CPIntVar(sites(i).winStart to sites(i).winEnd)
  })

  val duration: Array[CPIntVar] = Array.tabulate(nSite)(i => {
    CPIntVar(sites(i).service)
  })

  val departure: Array[CPIntVar] = Array.tabulate(nSite)(i => arrival(i) + duration(i))

  val siteVehicle: Array[CPIntVar] = Array.tabulate(nSite)(i => {
    if (sites(i).isDepot) CPIntVar((i - nStop) % 2)
    else CPIntVar((0 to nVehicle).filter(v => v == nVehicle || vehicleCompatibility(v).contains(sites(i).category)))
  })

  val siteLoad: Array[CPIntVar] = sites.map(s => CPIntVar(s.load))

  val travelLoad: Array[CPIntVar] = travelPatient.map(p => CPIntVar(patientLoad(p)))

  //Optional time windows:
  val optionalArrival = Array.tabulate(nVehicle + 1, nSite)((_, s) => CPIntVar(arrival(s).min, arrival(s).max))
  val optionalDuration = Array.tabulate(nVehicle + 1, nSite)((_, s) => CPIntVar(duration(s).min, duration(s).max))
  val optionalDeparture = Array.tabulate(nVehicle + 1, nSite)((v, s) => optionalArrival(v)(s) + optionalDuration(v)(s))

  //Sequence variables:
  val sequences: Array[CPInsertSeqVar] = Array.fill(nVehicle)(CPInsertSeqVar(sites.length))


  /* constraints */

  //Setting depots:
  for (v <- sequences.indices) {
    add(First(sequences(v), nStop + v))
    add(Last(sequences(v), nStop + nVehicle + v))
  }

  //Sequence Allocation
  add(SequenceAllocation(sequences.asInstanceOf[Array[CPSeqVar]], sites.indices, siteVehicle))

  //Linking optional and real time windows:
  for (s <- sites.indices) {
    add(AlternativeActivities(
      arrival(s),
      duration(s),
      departure(s),
      optionalArrival.map(_ (s)),
      optionalDuration.map(_ (s)),
      optionalDeparture.map(_ (s)),
      siteVehicle(s)
    ))
  }

  //Precedence constraints:
  travels.foreach{case (pickup, drop, _) =>
    for(v <- sequences.indices) add(Precedence(sequences(v), pickup, drop, dependent = true))
  }

  //Adding dial and ride constraints:
  val (startSites, endSites) = travels.map{case (s, e, _) => (s, e)}.unzip
  for (v <- sequences.indices) {
    add(TransitionTimes(sequences(v),optionalArrival(v),optionalDuration(v),optionalDeparture(v),transMatrix)) //Transition times constraints

    //Adding cumulative:
    val maxCapVar = CPIntVar(vehicleCap(v))
    val minCapVar = CPIntVar(0)
    add(Cumulative(sequences(v), startSites, endSites, travelLoad, maxCapVar, minCapVar))
  }

  //Linking stop visit:
  for (s <- 0 until nStop) add(visited(sites(s).patient) === (siteVehicle(s) ?!== nVehicle))


  /* Objective function */

  val nServed: CPIntVar = sum(visited)
  maximize(nServed)


  /* Solution */

  onSolution {
    println("Patients serviced: " + nServed.value)
    println("Sequences:\n" + sequences.mkString("\n"))

    println("------------")
  }


  /* Search */

  def isDecided(stop: Int): Boolean = {
    if(siteVehicle(stop).isBound) siteVehicle(stop).value == nVehicle || sequences(siteVehicle(stop).value).isMember(stop)
    else false
  }

  def nInsertionsPossible(i: Int): Int = siteVehicle(i).map(s => {
    if(s == nVehicle) 0
    else sequences(s).nCurrentInsertionsFor(i)
  }).sum

  //Return possible insertions for stop: (seq, elem, pred)
  def computeInsertions(i: Int): Seq[(Int, Int, Int)] = siteVehicle(i).filterNot(_ == nVehicle).flatMap(seq => {
    sequences(seq).allCurrentInsertionsFor(i).map(pred => (seq, i, pred))
  }).toSeq

  search {
    val undecidedElems = sites.indices.filterNot(isDecided) //Filtering undecided stops
    if(undecidedElems.isEmpty) noAlternative //If all stops decided => solution
    else{
      val selectStop = undecidedElems.minBy(nInsertionsPossible) //Selecting stop with less insertions possible
      val inserts = Random.shuffle(if(siteVehicle(selectStop).hasValue(nVehicle)) computeInsertions(selectStop) :+ (nVehicle, selectStop, -1) else computeInsertions(selectStop))
      if(inserts.isEmpty) branchOne(throw Inconsistency) //If no insertion possible for stop => Inconsitency
      else branchAll(inserts){
        case (seq, elem, pred) => if(seq == nVehicle) add(siteVehicle(elem) === nVehicle) else sequences(seq).insertAfter(elem, pred)
      }
    }
  }

  println(start())

  protected case class Stop(
                             patient: Int,
                             place: Int,
                             winStart: Int,
                             winEnd: Int,
                             operation: Int
                           ) {
    def isDepot: Boolean = patient == -1

    def category: Int = if (isDepot) -1 else patientCategory(patient)

    def service: Int = if (isDepot) 0 else patientSrv(patient)

    def forward: Boolean = operation < 2

    def pickup: Boolean = operation == 0 || operation == 2

    def isStartDepot: Boolean = isDepot && pickup

    def isEndDepot: Boolean = isDepot && !pickup

    def load: Int = if (isDepot) 0 else if (pickup) patientLoad(patient) else -patientLoad(patient)
  }
}
