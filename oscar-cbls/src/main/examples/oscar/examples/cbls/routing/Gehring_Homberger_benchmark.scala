package oscar.examples.cbls.routing

import java.io.File

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.timeWindow.{TimeWindowConstraint, TimeWindowConstraintWithLogReduction}
import oscar.cbls.business.routing.model.extensions.TimeWindows
import oscar.cbls.business.routing.neighborhood.{RemovePoint, SegmentExchangeOnSegments}
import oscar.cbls.core.search.Best

import scala.io.Source
import scala.util.Random

object Gehring_Homberger_benchmark extends App {
  val size = args(1).toInt
  val files = new File(args(0)).listFiles().toList.sorted

  for(file <- files) {
    println(file.getName)
    val rawPb = generateProblem(file)
    val refinePb = adjustDataToOscaR(rawPb._1, rawPb._2, rawPb._3, rawPb._4, rawPb._5, rawPb._6)
    new Gehring_Homberger_benchmark_VRPTW(refinePb._1, refinePb._2, refinePb._3, refinePb._4, refinePb._5, refinePb._6)
  }

  private def adjustDataToOscaR(oldN: Long, v: Long, c: Long, oldCoords: Array[(Long,Long)], oldTimeWindows: TimeWindows, oldDemands: Array[Long]): (Long, Long, Long, Array[Array[Long]], TimeWindows, Array[Long]) ={
    val n = oldN*2 + v - 2 // we need to add one source of demand for each nodes

    val vehiclesCoords: Array[(Long,Long)] = Array.fill(v)(oldCoords(0))
    val nodesCoords: Array[(Long,Long)] = oldCoords.drop(1).foldLeft(List[(Long,Long)]())((acc, item) => acc :+ oldCoords(0) :+ item).toArray
    val coords = vehiclesCoords ++ nodesCoords
    require(coords.length == (2*size)+v-2)

    val vehiclesEas = Array.fill(v)(oldTimeWindows.earliestArrivalTimes(0))
    val nodesEas: Array[Long] = oldTimeWindows.earliestArrivalTimes.drop(1).foldLeft(List[Long]())(
      (acc, item) => acc :+ oldTimeWindows.earliestArrivalTimes(0) :+ item).toArray
    val eas = vehiclesEas ++ nodesEas

    val vehiclesLas = Array.fill(v)(oldTimeWindows.latestArrivalTimes(0))
    val nodesLas: Array[Long] = oldTimeWindows.latestArrivalTimes.drop(1).foldLeft(List[Long]())(
      (acc, item) => acc :+ oldTimeWindows.latestArrivalTimes(0) :+ item).toArray
    val las = vehiclesLas ++ nodesLas

    val vehiclesTs = Array.fill(v)(oldTimeWindows.taskDurations(0))
    val nodesTs: Array[Long] = oldTimeWindows.taskDurations.drop(1).foldLeft(List[Long]())(
      (acc, item) => acc :+ oldTimeWindows.taskDurations(0) :+ item).toArray
    require(nodesTs.length == (2*size)-2)
    val ts = vehiclesTs ++ nodesTs

    val timeWindows = TimeWindows(earliestArrivalTimes = Some(eas), latestArrivalTimes = Some(las), taskDurations = ts)

    val vehiclesDemands = Array.fill(v)(0L)
    val nodesDemands: Array[Long] = oldDemands.drop(1).foldLeft(List[Long]())(
      (acc, item) => acc :+ item :+ (-item)).toArray
    require(nodesTs.length == (2*size)-2)
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
  val penaltyForUnrouted = 100000
  val penaltyForMovingVehicle = 10000

  println("\nVehicules : " + v)
  println("Noeuds totals : " + n)
  println("Capacité max : " + c + "\n")

  val travelDurationMatrix = RoutingMatrixGenerator.generateLinearTravelTimeFunction(n,distanceMatrix)
  val (listOfChains,precedences): (List[List[Long]], List[(Long,Long)]) =
    (List.tabulate(Gehring_Homberger_benchmark.size-1)(x => List(v+(2*x),v+(2*x)+1)),
      List.tabulate(Gehring_Homberger_benchmark.size-1)(x => (v+(2*x),v+(2*x)+1)))
  val contentsFlow = demands

  // Distance
  val totalRouteLength = routeLength(myVRP.routes,n,v,false,distanceMatrix,true,true,false)(0)
  val movingVehiclesNow = movingVehicles(myVRP.routes,v)

  //Chains
  val precedenceRoute = myVRP.routes.createClone()
  val precedenceInvariant = precedence(precedenceRoute,precedences)
  val vehicleOfNodesNow = vehicleOfNodes(precedenceRoute,v)
  val precedencesConstraints = new ConstraintSystem(m)
  for(start <- precedenceInvariant.nodesStartingAPrecedence)
    precedencesConstraints.add(vehicleOfNodesNow(start) === vehicleOfNodesNow(precedenceInvariant.nodesEndingAPrecedenceStartedAt(start).head))
  precedencesConstraints.add(0 === precedenceInvariant)
  val chainsExtension = chains(myVRP,listOfChains)


  //Time window constraints
  val timeWindowRoute = precedenceRoute.createClone()
  val timeWindowViolations = Array.fill(v)(new CBLSIntVar(m, 0, Domain.coupleToDomain((0,1))))
  val timeWindowConstraint = TimeWindowConstraint(myVRP.routes,n,v,timeWindows.earliestArrivalTimes, timeWindows.latestLeavingTimes, timeWindows.taskDurations, distanceMatrix, timeWindowViolations)


  // Vehicle content
  val contentRoute = timeWindowRoute.createClone()
  val violationOfContentAtNode = new CBLSIntVar(myVRP.routes.model, 0, 0 to Int.MaxValue, "violation of capacity " + "Content at node")
  val capacityInvariant = forwardCumulativeConstraintOnVehicle(myVRP.routes,n,v,
    (from,to,fromContent) => fromContent + contentsFlow(to),
    c,
    Array.fill(v)(0),
    violationOfContentAtNode,
    4,
    "Content at node")

  //Constraints & objective
  val obj = new CascadingObjective(precedencesConstraints,
    new CascadingObjective(sum(timeWindowViolations),
      new CascadingObjective(capacityInvariant.violation,
        totalRouteLength + (penaltyForUnrouted*(n - length(myVRP.routes)) + penaltyForMovingVehicle*setSum(movingVehiclesNow, x => 1)))))

  m.close()

  val relevantToTime = TimeWindowHelper.relevantPredecessorsOfNodes(myVRP, timeWindows, travelDurationMatrix)
  val relevantToCapacity = CapacityHelper.relevantPredecessorsOfNodes(myVRP, c, Array.fill(v)(c), contentsFlow)

  val relevantPredecessorsOfNodes = relevantToTime.map(x => x._1 -> x._2.filter(relevantToCapacity(x._1)))
  val relevantSuccessorsOfNodes = TimeWindowHelper.relevantSuccessorsOfNodes(myVRP, timeWindows, travelDurationMatrix)
  val closestRelevantNeighborsByDistance = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(distanceMatrix,relevantPredecessorsOfNodes)(_))

  def relevantPredecessorsForLastNode(lastNode: Long) = ChainsHelper.relevantNeighborsForLastNodeAfterHead(myVRP,chainsExtension,Some(relevantPredecessorsOfNodes(lastNode)))(lastNode)
  val relevantPredecessorsForInternalNodes = ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP, chainsExtension)_

  var enoughSpaceAfterNeighborNow: (Long,Long,Array[Long]) => Boolean =
    CapacityHelper.enoughSpaceAfterNeighbor(n,capacityInvariant)

  def postFilter(node:Long): (Long) => Boolean = {
    (neighbor: Long) => {
      val successor = myVRP.nextNodeOf(neighbor)
      myVRP.isRouted(neighbor) &&
        (successor.isEmpty || relevantSuccessorsOfNodes(node).contains(successor.get)) &&
        enoughSpaceAfterNeighborNow(node,neighbor,contentsFlow)
    }
  }


  // MOVING


  val nextMoveGenerator = {
    (exploredMoves:List[OnePointMoveMove], t:Option[List[Long]]) => {
      val chainTail: List[Long] = t match {
        case None =>
          val movedNode = exploredMoves.head.movedPoint
          chainsExtension.nextNodesInChain(chainsExtension.firstNodeInChainOfNode(movedNode))
        case Some(tail: List[Long]) => tail
      }

      chainTail match {
        case Nil => None
        case head :: Nil => None
        case nextNodeToMove :: newTail =>
          val moveNeighborhood = onePointMove(() => Some(nextNodeToMove),
            () => relevantPredecessorsForInternalNodes, myVRP)
          Some(moveNeighborhood, Some(newTail))
      }
    }
  }

  val firstNodeOfChainMove = onePointMove(() => myVRP.routed.value.filter(chainsExtension.isHead),()=> myVRP.kFirst(v*2,closestRelevantNeighborsByDistance(_),postFilter), myVRP,neighborhoodName = "MoveHeadOfChain")

  def lastNodeOfChainMove(lastNode:Long) = onePointMove(() => List(lastNode),()=> myVRP.kFirst(v*2,relevantPredecessorsForLastNode,postFilter), myVRP,neighborhoodName = "MoveLastOfChain")

  val oneChainMove = {
    dynAndThen(firstNodeOfChainMove,
      (moveMove: OnePointMoveMove) => {
        mu[OnePointMoveMove, Option[List[Long]]](
          lastNodeOfChainMove(chainsExtension.lastNodeInChainOfNode(moveMove.movedPoint)),
          nextMoveGenerator,
          None,
          Long.MaxValue,
          false)
      }
    ) name "One Chain Move"

  }

  def onePtMove(k:Long) = profile(onePointMove(myVRP.routed, () => myVRP.kFirst(k,closestRelevantNeighborsByDistance(_),postFilter), myVRP)) name "One Point Move"

  // INSERTING

  val nextInsertGenerator = {
    (exploredMoves:List[InsertPointMove], t:Option[List[Long]]) => {
      val chainTail: List[Long] = t match {
        case None =>
          val insertedNode = exploredMoves.head.insertedPoint
          chainsExtension.nextNodesInChain(chainsExtension.firstNodeInChainOfNode(insertedNode))
        case Some(tail: List[Long]) => tail
      }

      chainTail match {
        case Nil => None
        case head :: Nil => None
        case nextNodeToInsert :: newTail =>
          val insertNeighborhood = insertPointUnroutedFirst(() => Some(nextNodeToInsert),
            () => relevantPredecessorsForInternalNodes, myVRP)
          Some(insertNeighborhood, Some(newTail))
      }
    }
  }

  val firstNodeOfChainInsertion = insertPointUnroutedFirst(
    () => chainsExtension.heads.filter(n => !myVRP.isRouted(n)),
    ()=> myVRP.kFirst(v*2,closestRelevantNeighborsByDistance(_), postFilter),
    myVRP,
    neighborhoodName = "InsertUF")

  def lastNodeOfChainInsertion(lastNode:Long) = insertPointUnroutedFirst(() => List(lastNode),()=> myVRP.kFirst(v*2,relevantPredecessorsForLastNode,postFilter), myVRP,
    neighborhoodName = "InsertUF")

  val oneChainInsert = {
    dynAndThen(firstNodeOfChainInsertion,
      (insertMove: InsertPointMove) => {
        mu[InsertPointMove,Option[List[Long]]](
          lastNodeOfChainInsertion(chainsExtension.lastNodeInChainOfNode(insertMove.insertedPoint)),
          nextInsertGenerator,
          None,
          Long.MaxValue,
          false)
      }) name "One Chain Insert"

  }


  private object RemoveNode {
    def apply(nodeToRemove: Option[List[Long]] = None): RemovePoint = {
      RemovePoint(
        relevantPointsToRemove = () => nodeToRemove.getOrElse(Random.shuffle(listOfChains.map(_.head))).filter(myVRP.isRouted),
        vrp = myVRP)
    }
  }

  private object NextRemoveGenerator {
    def apply() ={
      (exploredMoves:List[RemovePointMove], t:Option[List[Long]]) => {
        val chainTail: List[Long] = t match {
          case None => val removedNode = exploredMoves.head.pointToRemove
            chainsExtension.nextNodesInChain(removedNode)
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

  object OneChainRemove {
    def apply(nodeToRemove: Option[List[Long]] = None) = {
      profile(mu[RemovePointMove, Option[List[Long]]](
        RemoveNode(nodeToRemove),
        NextRemoveGenerator(),
        None,
        Long.MaxValue,
        false
      ) name ("One Chain Remove"))
    }
  }

  object ProfiledMultiChainRemove {
    def apply(nbToRemove: Int, nodesToRemove: Option[List[Long]] = None) ={
      profile(atomic(OneChainRemove(nodesToRemove).acceptAll(), _ > nbToRemove).guard(() => nbToRemove > 0)) name ("Multi Chain Remove " + nbToRemove)
    }
  }

  //val routeUnroutedPoint =  Profile(new InsertPointUnroutedFirst(myVRP.unrouted,()=> myVRP.kFirst(10,filteredClosestRelevantNeighborsByDistance), myVRP,neighborhoodName = "InsertUF"))


  val search = bestSlopeFirst(List(oneChainMove,onePtMove(20)), refresh = 10) exhaust
    bestSlopeFirst(List(oneChainInsert,oneChainMove,onePtMove(20)), refresh = 10).afterMove(
    {enoughSpaceAfterNeighborNow = CapacityHelper.enoughSpaceAfterNeighbor(n,capacityInvariant)}).
    onExhaustRestartAfter(ProfiledMultiChainRemove(listOfChains.size/10),3, obj)
  //val search = (BestSlopeFirst(List(routeUnroutdPoint2, routeUnroutdPoint, vlsn1pt)))


  //search.verbose = 1
  //search.verboseWithExtraInfo(4, ()=> "" + myVRP)



  search.doAllMoves(obj=obj)

  println("Unrouted nodes : " + myVRP.unroutedNodes.size)
  println("Distance totale parcourue :" + totalRouteLength.value/100.0)
  println("Nombre de véhicules utilisés :" + movingVehiclesNow.value.size)
  println("\n\n###########################################################################################\n\n")

  println(search.profilingStatistics)
}



