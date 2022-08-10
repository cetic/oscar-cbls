package examples.oscar.cbls.routing

import oscar.cbls._
import oscar.cbls.algo.generator.RoutingMatrixGenerator
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.global.RouteLength
import oscar.cbls.business.routing.invariants.timeWindow.{TimeWindowConstraint, TransferFunction}
import oscar.cbls.business.routing.invariants.vehicleCapacity.GlobalVehicleCapacityConstraintWithLogReduction
import oscar.cbls.core.computation.{CBLSIntVar, Store}
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.core.objective.CascadingObjective

import scala.collection.immutable.HashSet

/**
 * Created by fg on 12/05/17.
 */

object SimpleVRPWithTimeWindowsAndVehicleContent extends App {
  val nbIterations = 5
  for(v <- 10 to 20 by 5){
    for(n <- 100 to 500 by 100){
      var totalTime = 0L
      for(seed <- 0 until nbIterations){
        val start = System.currentTimeMillis()
        new SimpleVRPWithTimeWindowsAndVehicleContent(n, v, seed)
        totalTime += (System.currentTimeMillis()-start)
      }
      println("N : " + n + "\tV : " + v + "\tIterations : " + nbIterations + "\t=>\tTook (avg) : " + (totalTime/nbIterations) + " ms")
    }
  }
}

class SimpleVRPWithTimeWindowsAndVehicleContent(n: Int, v: Int, seed: Int) {
  val m = Store(noCycle = false/*, checker = Some(new ErrorChecker)*/)
  val penaltyForUnrouted = 10000
  val maxVehicleContent = 8
  val minVehicleContent = 4

  RoutingMatrixGenerator.random.setSeed(seed)

  val symmetricDistance = RoutingMatrixGenerator.apply(n)._1
  val travelDurationMatrix = symmetricDistance
  val (listOfChains,precedences) = RoutingMatrixGenerator.generateChainsPrecedence(n,v,(n-v)/2)
  val singleNodeTransferFunctions = RoutingMatrixGenerator.generateFeasibleTransferFunctions(n,v,travelDurationMatrix,listOfChains)
  //val maxTravelDurations = RoutingMatrixGenerator.generateMaxTravelDurations(listOfChains,singleNodeTransferFunctions.map(_.ea),travelDurationMatrix)
  val contentsFlow = RoutingMatrixGenerator.generateContentFlow(n,listOfChains,maxVehicleContent)
  val vehiclesSize = RoutingMatrixGenerator.generateVehiclesSize(v,maxVehicleContent,minVehicleContent)

  val myVRP =  new VRP(m,n,v)

  val contentRoute = myVRP.routes.createClone()
  val timeWindowRoute = contentRoute.createClone()
  val precedenceRoute = timeWindowRoute.createClone()
  val routeLengthRoute = precedenceRoute.createClone()

  // Distance
  val routeLengthPerVehicles = Array.tabulate(v)(vehicle => CBLSIntVar(m,name = s"Length of route $vehicle"))
  val routeLengthInvariant = new RouteLength(routeLengthRoute,n,v,routeLengthPerVehicles,(from: Int, to: Int) => symmetricDistance(from)(to))

  //Chains
  val precedenceInvariant = precedence(precedenceRoute,precedences)
  val vehicleOfNodesNow = vehicleOfNodes(precedenceRoute,v)
  val precedencesConstraints = new ConstraintSystem(m)
  for(start <- precedenceInvariant.nodesStartingAPrecedence)
    precedencesConstraints.add(vehicleOfNodesNow(start) === vehicleOfNodesNow(precedenceInvariant.nodesEndingAPrecedenceStartedAt(start).head))
  precedencesConstraints.add(0 === precedenceInvariant)
  val chainsExtension = chains(myVRP,listOfChains)

  // Vehicle content
  val violationOfContentAtVehicle = Array.tabulate(v)(vehicle => new CBLSIntVar(m, 0, 0 to Int.MaxValue, s"violation of capacity of vehicle $vehicle"))
  val capacityInvariant = GlobalVehicleCapacityConstraintWithLogReduction(contentRoute, n, v, vehiclesSize, contentsFlow, violationOfContentAtVehicle)

  //TimeWindow
  val violationOfTimeAtVehicle = TimeWindowConstraint(timeWindowRoute, n, v, singleNodeTransferFunctions, travelDurationMatrix)

  //Constraints & objective
  val obj = new CascadingObjective(sum(violationOfContentAtVehicle),
    new CascadingObjective(sum(violationOfTimeAtVehicle),
      new CascadingObjective(precedencesConstraints,
        sum(routeLengthPerVehicles) + (penaltyForUnrouted*(n - length(myVRP.routes))))))
  m.close()

  val timeMatrix = Array.tabulate(n)(from => Array.tabulate(n)(to => travelDurationMatrix(from)(to)))
  val relevantToTime = TransferFunction.relevantPredecessorsOfNodes(n,v,singleNodeTransferFunctions,timeMatrix)
  val relevantToCapacity = GlobalVehicleCapacityConstraintWithLogReduction.relevantPredecessorsOfNodes(capacityInvariant)

  val relevantPredecessorsOfNodes = relevantToTime.map(x => x._1 -> x._2.toList.intersect(relevantToCapacity(x._1).toList))
  val relevantSuccessorsOfNodes = TransferFunction.relevantSuccessorsOfNodes(n,v,singleNodeTransferFunctions,timeMatrix)
  val closestRelevantNeighborsByDistance = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance,relevantPredecessorsOfNodes)(_))

  def relevantPredecessorsForLastNode(lastNode: Int) = ChainsHelper.relevantNeighborsForLastNodeAfterHead(myVRP,chainsExtension,Some(HashSet() ++ relevantPredecessorsOfNodes(lastNode)))(lastNode)
  val relevantPredecessorsForInternalNodes = ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP, chainsExtension)_


  // MOVING


  val nextMoveGenerator = {
    (exploredMoves:List[OnePointMoveMove], t:Option[List[Int]]) => {
      val chainTail: List[Int] = t match {
        case None =>
          val movedNode = exploredMoves.head.movedPoint
          chainsExtension.nextNodesInChain(chainsExtension.firstNodeInChainOfNode(movedNode))
        case Some(tail: List[Int]) => tail
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

  val firstNodeOfChainMove = onePointMove(() => myVRP.routed.value.filter(chainsExtension.isHead),()=> myVRP.kFirst(v*2,closestRelevantNeighborsByDistance(_)), myVRP,neighborhoodName = "MoveHeadOfChain")

  def lastNodeOfChainMove(lastNode:Int) = onePointMove(() => List(lastNode),()=> myVRP.kFirst(v*2,relevantPredecessorsForLastNode), myVRP,neighborhoodName = "MoveLastOfChain")

  val oneChainMove = {
    dynAndThen(firstNodeOfChainMove,
      (moveMove: OnePointMoveMove) => {
        mu[OnePointMoveMove, Option[List[Int]]](
          lastNodeOfChainMove(chainsExtension.lastNodeInChainOfNode(moveMove.movedPoint)),
          nextMoveGenerator,
          None,
          Long.MaxValue,
          false)
      }
    ) name "One Chain Move"

  }

  def onePtMove(k:Int) = profile(onePointMove(myVRP.routed, () => myVRP.kFirst(k,closestRelevantNeighborsByDistance(_)), myVRP)) name "One Point Move"

  // INSERTING

  val nextInsertGenerator = {
    (exploredMoves:List[InsertPointMove], t:Option[List[Int]]) => {
      val chainTail: List[Int] = t match {
        case None =>
          val insertedNode = exploredMoves.head.insertedPoint
          chainsExtension.nextNodesInChain(chainsExtension.firstNodeInChainOfNode(insertedNode))
        case Some(tail: List[Int]) => tail
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

  val firstNodeOfChainInsertion = insertPointUnroutedFirst(() => chainsExtension.heads.filter(n => !myVRP.isRouted(n)),()=> myVRP.kFirst(v*2,closestRelevantNeighborsByDistance(_)), myVRP,neighborhoodName = "InsertUF")

  def lastNodeOfChainInsertion(lastNode:Int) = insertPointUnroutedFirst(() => List(lastNode),()=> myVRP.kFirst(v*2,relevantPredecessorsForLastNode), myVRP,neighborhoodName = "InsertUF")

  val oneChainInsert = {
    dynAndThen(firstNodeOfChainInsertion,
      (insertMove: InsertPointMove) => {
        mu[InsertPointMove,Option[List[Int]]](
          lastNodeOfChainInsertion(chainsExtension.lastNodeInChainOfNode(insertMove.insertedPoint)),
          nextInsertGenerator,
          None,
          Long.MaxValue,
          false)
      }) name "One Chain Insert"

  }

  //val routeUnroutedPoint =  Profile(new InsertPointUnroutedFirst(myVRP.unrouted,()=> myVRP.kFirst(10,filteredClosestRelevantNeighborsByDistance), myVRP,neighborhoodName = "InsertUF"))


  val search = oneChainInsert exhaust oneChainMove exhaust onePtMove(20)
  //val search = (BestSlopeFirst(List(routeUnroutdPoint2, routeUnroutdPoint, vlsn1pt)))


  search.verbose = 0
  //search.verboseWithExtraInfo(4, ()=> "" + myVRP)



  search.doAllMoves(obj=obj)

  search.profilingStatistics
}
