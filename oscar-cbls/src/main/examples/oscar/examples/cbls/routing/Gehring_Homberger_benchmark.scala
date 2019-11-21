package oscar.examples.cbls.routing

import java.io.File

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.WeightedNodesPerVehicle
import oscar.cbls.business.routing.invariants.global.{GlobalConstraintCore, RouteLength}
import oscar.cbls.business.routing.invariants.timeWindow.{TimeWindowConstraint, TimeWindowConstraintWithLogReduction}
import oscar.cbls.business.routing.model.extensions.TimeWindows
import oscar.cbls.business.routing.neighborhood.{InsertPointUnroutedFirst, RemovePoint}
import oscar.cbls.core.search.Best

import scala.io.Source
import scala.util.Random

object Gehring_Homberger_benchmark extends App {
  val size = 100
  val files = new File("/home/fg/Documents/OscaR/Solomon/"+size+"/").listFiles().toList.sorted

  for(file <- files) {
    println(file.getName)
    val rawPb = generateProblem(file)
    val refinePb = adjustDataToOscaR(rawPb._1, rawPb._2, rawPb._3, rawPb._4, rawPb._5, rawPb._6)
    new Gehring_Homberger_benchmark_VRPTW(refinePb._1, refinePb._2, refinePb._3, refinePb._4, refinePb._5, refinePb._6)
  }

  private def adjustDataToOscaR(oldN: Long, v: Long, c: Long, oldCoords: Array[(Long,Long)], oldTimeWindows: TimeWindows, oldDemands: Array[Long]): (Long, Long, Long, Array[Array[Long]], TimeWindows, Array[Long]) ={
    val n = oldN + v - 1 // oldN contains only one instance of a depot

    val vehiclesCoords: Array[(Long,Long)] = Array.fill(v)(oldCoords(0))
    val nodesCoords: Array[(Long,Long)] = oldCoords.drop(1)
    val coords = vehiclesCoords ++ nodesCoords
    require(coords.length == oldN+v-1, "coords length should be equal to " + (n+v-1) + " instead " + coords.length)

    val vehiclesEas = Array.fill(v)(oldTimeWindows.earliestArrivalTimes(0))
    val nodesEas: Array[Long] = oldTimeWindows.earliestArrivalTimes.drop(1)
    val eas = vehiclesEas ++ nodesEas

    val vehiclesLas = Array.fill(v)(oldTimeWindows.latestArrivalTimes(0))
    val nodesLas: Array[Long] = oldTimeWindows.latestArrivalTimes.drop(1)
    val las = vehiclesLas ++ nodesLas

    val vehiclesTs = Array.fill(v)(oldTimeWindows.taskDurations(0))
    val nodesTs: Array[Long] = oldTimeWindows.taskDurations.drop(1)
    val ts = vehiclesTs ++ nodesTs

    val timeWindows = TimeWindows(earliestArrivalTimes = Some(eas), latestArrivalTimes = Some(las), taskDurations = ts)

    val vehiclesDemands = Array.fill(v)(0L)
    val nodesDemands: Array[Long] = oldDemands.drop(1)
    val demands = vehiclesDemands ++ nodesDemands


    (n,v,c,generateMatrix(coords),timeWindows,demands)
  }

  private def generateProblem(file: File): (Long, Long, Long, Array[(Long,Long)], TimeWindows, Array[Long]) ={
    val lines = Source.fromFile(file).getLines
    lines.next()        // NAME
    lines.next()        // blank space
    lines.next()        // VEHICLE
    lines.next()        // NUMBER CAPACITY
    val vehicleInfo = lines.next().split("\\s+").drop(1)
    val (v,c) = (vehicleInfo.head.toLong, vehicleInfo.last.toLong)
    lines.next()        // blank space
    lines.next()        // CUSTOMER
    lines.next()        // COLUMN NAME
    lines.next()        // blank space

    val datas = Array.tabulate(size)(x => {
      val nodeInfo = lines.next().split("\\s+").drop(2)
      (nodeInfo(0),nodeInfo(1),nodeInfo(2),nodeInfo(3),nodeInfo(4),nodeInfo(5))

    })
    val coords = Array.tabulate(datas.length)(x => (datas(x)._1.toLong, datas(x)._2.toLong))
    val demands = Array.tabulate(datas.length)(x => datas(x)._3.toLong)
    val eas = Array.tabulate(datas.length)(x => datas(x)._4.toLong*100)
    val las = Array.tabulate(datas.length)(x => datas(x)._5.toLong*100)
    val ts = Array.tabulate(datas.length)(x => datas(x)._6.toLong*100)
    val timeWindows = TimeWindows(earliestArrivalTimes = Some(eas), latestArrivalTimes = Some(las), taskDurations = ts)
    (datas.length,v,c,coords,timeWindows,demands)
  }

  private def generateMatrix(coords: Array[(Long,Long)]): Array[Array[Long]] = {
    def distance(from: (Long, Long), to: (Long, Long)) =
      math.ceil(math.sqrt(math.pow(from._1 - to._1, 2) + math.pow(from._2 - to._2, 2))*100.0).toLong

    //for each delivery point, the distance to each warehouse
    Array.tabulate(coords.length)(
      n1 => Array.tabulate(coords.length)(
        n2 => distance(coords(n1), coords(n2))))
  }
}

class Gehring_Homberger_benchmark_VRPTW(n: Int, v: Int, c: Int, distanceMatrix: Array[Array[Long]], timeWindows: TimeWindows, demands: Array[Long]){
  val m = new Store(noCycle = false)
  val myVRP = new VRP(m,n,v)
  val penaltyForUnrouted = 1000000
  val penaltyForMovingVehicle = 10000

  val gc = GlobalConstraintCore(myVRP.routes, v)

  val travelDurationMatrix = RoutingMatrixGenerator.generateLinearTravelTimeFunction(n,distanceMatrix)
  val nodeWeight = demands

  // Distance
  val routeLengths = Array.fill(v)(CBLSIntVar(m,0))
  val routeLength = new RouteLength(gc,n,v,routeLengths,(from: Long, to: Long) => distanceMatrix(from)(to))
  val movingVehiclesNow = movingVehicles(myVRP.routes,v)

  //Time window constraints
  val timeWindowRoute = myVRP.routes.createClone()
  val timeWindowViolations = Array.fill(v)(new CBLSIntVar(m, 0, Domain.coupleToDomain((0,1))))

  val timeWindowConstraint = TimeWindowConstraint(gc,n,v,timeWindows.earliestArrivalTimes, timeWindows.latestLeavingTimes, timeWindows.taskDurations, distanceMatrix, timeWindowViolations)

  // Weighted nodes
  // The sum of node's weight can't excess the capacity of a vehicle
  val weightPerVehicle = Array.tabulate(v)(_ => CBLSIntVar(m))
  // This invariant maintains the total node's weight encountered by each vehicle
  val weightedNodesConstraint = WeightedNodesPerVehicle(gc, n, v, nodeWeight, weightPerVehicle)
  // This invariant maintains the capacity violation of each vehicle (le means lesser or equals)
  val vehicleCapacityViolation = Array.tabulate(v)(vehicle => weightPerVehicle(vehicle) le c)
  val constraintSystem = new ConstraintSystem(m)
  vehicleCapacityViolation.foreach(constraintSystem.post(_))

  //Constraints & objective
  val obj = new CascadingObjective(sum(timeWindowViolations),
      new CascadingObjective(constraintSystem,
        sum(routeLengths) + (penaltyForUnrouted*(n - length(myVRP.routes)) + penaltyForMovingVehicle*setSum(movingVehiclesNow, x => 1))))

  m.close()

  val relevantPredecessorsOfNodes = TimeWindowHelper.relevantPredecessorsOfNodes(myVRP, timeWindows, travelDurationMatrix)

  val relevantSuccessorsOfNodes = TimeWindowHelper.relevantSuccessorsOfNodes(myVRP, timeWindows, travelDurationMatrix)
  val closestRelevantNeighborsByDistance = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(distanceMatrix,relevantPredecessorsOfNodes)(_))

  def postFilter(node:Long): (Long) => Boolean = {
    (neighbor: Long) => {
      val successor = myVRP.nextNodeOf(neighbor)
      myVRP.isRouted(neighbor) &&
        (successor.isEmpty || relevantSuccessorsOfNodes(node).contains(successor.get))
    }
  }

  def onePtMove(k:Long) = profile(onePointMove(myVRP.routed, () => myVRP.kFirst(k,closestRelevantNeighborsByDistance(_),postFilter), myVRP)) name "One Point Move"

  private var removingRoute: List[Long] = List.empty

  private object RemoveNode {
    def apply(): RemovePoint = {
      RemovePoint(
        relevantPointsToRemove = () => {
          removingRoute = Random.shuffle(movingVehiclesNow.value).map(myVRP.getRouteOfVehicle(_).drop(1)).head
          removingRoute.take(1)
        },
        vrp = myVRP)
    }
  }

  private object NextRemoveGenerator {
    def apply() ={
      (exploredMoves:List[RemovePointMove], t:Option[List[Long]]) => {
        val chainTail: List[Long] = t match {
          case None => removingRoute.drop(1)
          case Some(tail:List[Long]) => tail
        }

        chainTail match {
          case Nil => None
          case nextNodeToRemove :: newTail =>
            val removeNeighborhood = RemovePoint(() => Some(nextNodeToRemove), myVRP)
            Some(removeNeighborhood, Some(newTail))
        }
      }
    }
  }

  object EmptyVehicle {
    def apply() = {
      profile(atomic(mu[RemovePointMove, Option[List[Long]]](
        RemoveNode(),
        NextRemoveGenerator(),
        None,
        Long.MaxValue,
        false
      ).acceptAll(), _ > 1).guard(() => {
          movingVehiclesNow.value.nonEmpty
        }))
    }
  }

  val routeUnroutedPoint =  profile(new InsertPointUnroutedFirst(myVRP.unrouted,()=> myVRP.kFirst(n,closestRelevantNeighborsByDistance(_)), myVRP,selectInsertionPointBehavior = Best(),neighborhoodName = "InsertUF"))


  val search = (routeUnroutedPoint exhaust onePtMove(n/2)).
    onExhaustRestartAfter(EmptyVehicle(),3, obj)

  search.doAllMoves(obj=obj)

  println("Unrouted nodes : " + myVRP.unroutedNodes.size)
  println("Distance totale parcourue :" + routeLengths.map(_.value).sum/100.0)
  println("Nombre de véhicules utilisés :" + movingVehiclesNow.value.size)
  println("\n\n###########################################################################################\n\n")


}