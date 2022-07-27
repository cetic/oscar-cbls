package examples.oscar.cbls.benchmarks

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.WeightedNodesPerVehicle
import oscar.cbls.business.routing.invariants.global.RouteLength
import oscar.cbls.business.routing.invariants.timeWindow.{TimeWindowConstraint, TransferFunction}
import oscar.cbls.business.routing.model.helpers.DistanceHelper
import oscar.cbls.business.routing.neighborhood.{InsertPointUnroutedFirst, RemovePoint}
import oscar.cbls.core.computation.{CBLSIntVar, Domain, Store}
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.core.objective.CascadingObjective
import oscar.cbls.core.search.Best

import java.io.File
import scala.io.Source
import scala.math.Ordering.Implicits.seqOrdering
import scala.util.Random

object GehringHombergerBench extends App {
  val size = args(1).toInt
  val files = new File(args(0)).listFiles().toList.sorted

  for(file <- files) {
    println(file.getName)
    val problem = generateProblem(file)
    new GehringHombergerBenchmarkVRPTW(problem._1, problem._2, problem._3, problem._4, problem._5, problem._6)
  }

  private def generateProblem(file: File): (Int, Int, Long, Array[Array[Long]], Array[TransferFunction], Array[Long]) ={
    // Retrieve data from file
    val bufSource = Source.fromFile(file)
    val lines = bufSource.getLines()
    lines.next()        // NAME
    lines.next()        // blank space
    lines.next()        // VEHICLE
    lines.next()        // NUMBER CAPACITY
    val vehicleInfo = lines.next().split("\\s+").drop(1)
    val (v,vehicleMaxCapacity) = (vehicleInfo.head.toInt, vehicleInfo.last.toLong)
    lines.next()        // blank space
    lines.next()        // CUSTOMER
    lines.next()        // COLUMN NAME
    lines.next()        // blank space

    val datas = Array.tabulate(size)(x => {
      val nodeInfo = lines.next().split("\\s+").drop(2)
      (nodeInfo(0),nodeInfo(1),nodeInfo(2),nodeInfo(3),nodeInfo(4),nodeInfo(5))
    })

    // Generate usual routing data
    val n = datas.length + v - 1 // datas contains 1 driver
    val coords = Array.tabulate(datas.length)(x => (datas(x)._1.toLong, datas(x)._2.toLong)) // ONLY ONE DRIVER
    val fullCoords = Array.fill(v)(coords.head) ++ coords.drop(1)
    val distanceMatrix = generateMatrix(fullCoords)

    // Time
    val eas = Array.tabulate(datas.length)(x => datas(x)._4.toLong*100)
    val las = Array.tabulate(datas.length)(x => datas(x)._5.toLong*100)
    val ts = Array.tabulate(datas.length)(x => datas(x)._6.toLong*100)
    val singleNodeTransferFunctions = Array.tabulate(n)(node =>
      if(node < v) TransferFunction.createFromEarliestAndLatestArrivalTime(node, eas(0), las(0), ts(0))
      else TransferFunction.createFromEarliestAndLatestArrivalTime(node, eas(node - v + 1), las(node - v + 1), ts(node - v + 1)))

    // Client demands
    val demands = Array.tabulate(n)(node => if(node < v) 0L else datas(node - v + 1)._3.toLong)
    (n,v,vehicleMaxCapacity,distanceMatrix,singleNodeTransferFunctions,demands)
  }

  private def generateMatrix(coords: Array[(Long,Long)]): Array[Array[Long]] = {
    def distance(from: (Long, Long), to: (Long, Long)): Long =
      math.ceil(math.sqrt(math.pow((from._1 - to._1).toDouble, 2.0) + math.pow((from._2 - to._2).toDouble, 2.0))*100.0).toLong

    //for each delivery point, the distance to each warehouse
    Array.tabulate(coords.length)(
      n1 => Array.tabulate(coords.length)(
        n2 => distance(coords(n1), coords(n2))))
  }
}

class GehringHombergerBenchmarkVRPTW(n: Int, v: Int, c: Long, distanceMatrix: Array[Array[Long]], singleNodeTransferFunctions: Array[TransferFunction], demands: Array[Long]){
  val m = Store(noCycle = false)
  val myVRP = new VRP(m,n,v)
  val penaltyForUnrouted = 1000000
  val penaltyForMovingVehicle = 10000

  val nodeWeight = demands

  // Distance
  val routeLengths = Array.fill(v)(CBLSIntVar(m,0))
  val routeLength = new RouteLength(myVRP.routes,n,v,routeLengths,(from: Int, to: Int) => distanceMatrix(from)(to))
  val movingVehiclesNow = movingVehicles(myVRP.routes,v)

  //Time window constraints
  val timeWindowRoute = myVRP.routes.createClone()
  val timeWindowViolations = Array.fill(v)(new CBLSIntVar(m, 0, Domain.coupleToDomain((0,1))))

  val timeWindowConstraint = TimeWindowConstraint(myVRP.routes,n,v,singleNodeTransferFunctions, distanceMatrix, timeWindowViolations)

  // Weighted nodes
  // The sum of node's weight can't excess the capacity of a vehicle
  val weightPerVehicle = Array.tabulate(v)(_ => CBLSIntVar(m))
  // This invariant maintains the total node's weight encountered by each vehicle
  val weightedNodesConstraint = WeightedNodesPerVehicle(myVRP.routes, n, v, nodeWeight, weightPerVehicle)
  // This invariant maintains the capacity violation of each vehicle (le means lesser or equals)
  val vehicleCapacityViolation = Array.tabulate(v)(vehicle => weightPerVehicle(vehicle) le c)
  val constraintSystem = ConstraintSystem(m)
  vehicleCapacityViolation.foreach(constraintSystem.post(_))

  //Constraints & objective
  val obj = new CascadingObjective(sum(timeWindowViolations),
      new CascadingObjective(constraintSystem,
        sum(routeLengths) + (penaltyForUnrouted*(n - length(myVRP.routes)) + penaltyForMovingVehicle*setSum(movingVehiclesNow, x => 1))))

  m.close()

  val relevantPredecessorsOfNodes = TransferFunction.relevantPredecessorsOfNodes(n,v,singleNodeTransferFunctions,distanceMatrix)

  val relevantSuccessorsOfNodes = TransferFunction.relevantSuccessorsOfNodes(n,v,singleNodeTransferFunctions, distanceMatrix)
  val closestRelevantNeighborsByDistance = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(distanceMatrix,relevantPredecessorsOfNodes)(_))

  def postFilter(node:Int): Int => Boolean = {
    (neighbor: Int) => {
      val successor = myVRP.nextNodeOf(neighbor)
      myVRP.isRouted(neighbor) &&
        (successor.isEmpty || relevantSuccessorsOfNodes(node).exists(_ == successor.get))
    }
  }

  def onePtMove(k:Int) = profile(onePointMove(myVRP.routed, () => myVRP.kFirst(k,closestRelevantNeighborsByDistance(_),postFilter), myVRP)) name "One Point Move"

  private var removingRoute: List[Int] = List.empty

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
      (exploredMoves:List[RemovePointMove], t:Option[List[Int]]) => {
        val chainTail: List[Int] = t match {
          case None => removingRoute.drop(1)
          case Some(tail:List[Int]) => tail
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
      profile(atomic(mu[RemovePointMove, Option[List[Int]]](
        RemoveNode(),
        NextRemoveGenerator(),
        None,
        Long.MaxValue,
        intermediaryStops = false
      ).acceptAll(), _ > 1).guard(() => {
          movingVehiclesNow.value.nonEmpty
        }))
    }
  }

  val routeUnroutedPoint =  profile(InsertPointUnroutedFirst(myVRP.unrouted,()=> myVRP.kFirst(n,closestRelevantNeighborsByDistance(_)), myVRP,selectInsertionPointBehavior = Best(),neighborhoodName = "InsertUF"))


  val search = (routeUnroutedPoint exhaust onePtMove(n/2)).
    onExhaustRestartAfter(EmptyVehicle(),3, obj)

  search.doAllMoves(obj=obj)

  println("Unrouted nodes : " + myVRP.unroutedNodes.size)
  println("Distance totale parcourue :" + routeLengths.map(_.value).sum/100.0)
  println("Nombre de véhicules utilisés :" + movingVehiclesNow.value.size)
  println("\n\n###########################################################################################\n\n")

}
