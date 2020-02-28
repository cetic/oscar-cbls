package oscar.examples.cbls.routing

import javax.swing.TransferHandler.TransferSupport
import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.global.{GlobalConstraintCore, RouteLength}
import oscar.cbls.business.routing.invariants.timeWindow.{NaiveTimeWindowConstraint, TransferFunction}
import oscar.cbls.business.routing.invariants.vehicleCapacity.GlobalVehicleCapacityConstraintWithLogReduction

import scala.collection.immutable.HashSet

/**
  * Created by fg on 12/05/17.
  */

object SimpleVRPWithTimeWindowsAndVehicleContent extends App{
  val m = new Store(noCycle = false, checker = Some(new ErrorChecker))
  val v = 10
  val n = 100
  val penaltyForUnrouted = 10000
  val maxVehicleContent = 8
  val minVehicleContent = 4

  val symmetricDistance = RoutingMatrixGenerator.apply(n)._1
  val travelDurationMatrix = symmetricDistance
  val (listOfChains,precedences) = RoutingMatrixGenerator.generateChainsPrecedence(n,v,(n-v)/2)
  val singleNodeTransferFunctions = RoutingMatrixGenerator.generateFeasibleTransferFunctions(n,v,travelDurationMatrix,listOfChains)
  val maxTravelDurations = RoutingMatrixGenerator.generateMaxTravelDurations(listOfChains,singleNodeTransferFunctions.map(_.ea),travelDurationMatrix)
  val contentsFlow = RoutingMatrixGenerator.generateContentFlow(n,listOfChains,maxVehicleContent)
  val vehiclesSize = RoutingMatrixGenerator.generateVehiclesSize(v,maxVehicleContent,minVehicleContent)

  val myVRP =  new VRP(m,n,v)
  NaiveTimeWindowConstraint.maxTransferFunctionWithTravelDurationRestriction(n,v,singleNodeTransferFunctions,maxTravelDurations,listOfChains, travelDurationMatrix)

  val gc = GlobalConstraintCore(myVRP.routes, v)

  // Distance
  val routeLengthPerVehicles = Array.tabulate(v)(vehicle => CBLSIntVar(m,name = "Length of route " + vehicle))
  val routeLengthInvariant = new RouteLength(gc,n,v,routeLengthPerVehicles,(from: Long, to: Long) => symmetricDistance(from)(to))

  //Chains
  val precedenceRoute = myVRP.routes.createClone()
  val precedenceInvariant = precedence(precedenceRoute,precedences)
  val vehicleOfNodesNow = vehicleOfNodes(precedenceRoute,v)
  val precedencesConstraints = new ConstraintSystem(m)
  for(start <- precedenceInvariant.nodesStartingAPrecedence)
    precedencesConstraints.add(vehicleOfNodesNow(start) === vehicleOfNodesNow(precedenceInvariant.nodesEndingAPrecedenceStartedAt(start).head))
  precedencesConstraints.add(0 === precedenceInvariant)
  val chainsExtension = chains(myVRP,listOfChains)

  // Vehicle content
  val violationOfContentAtVehicle = Array.tabulate(v)(vehicle => new CBLSIntVar(myVRP.routes.model, 0, 0 to Int.MaxValue, "violation of capacity of vehicle " + vehicle))
  val capacityInvariant = GlobalVehicleCapacityConstraintWithLogReduction(gc, n, v, vehiclesSize, contentsFlow, violationOfContentAtVehicle)

  //TimeWindow
  val timeWindowRoute = precedenceRoute.createClone()
  val timeWindowInvariant = NaiveTimeWindowConstraint(myVRP.routes, n, v, singleNodeTransferFunctions, travelDurationMatrix)
  timeWindowInvariant.addMaxTravelDurationConstraint(maxTravelDurations)
  val timeWindowConstraint = timeWindowInvariant.violation

  //Constraints & objective
  val obj = new CascadingObjective(precedencesConstraints,
    new CascadingObjective(timeWindowConstraint,
      new CascadingObjective(sum(violationOfContentAtVehicle),
        sum(routeLengthPerVehicles) + (penaltyForUnrouted*(n - length(myVRP.routes))))))
  m.close()

  val timeMatrix = Array.tabulate(n)(from => Array.tabulate(n)(to => travelDurationMatrix(from)(to)))
  val relevantToTime = TransferFunction.relevantPredecessorsOfNodes(n,v,singleNodeTransferFunctions,timeMatrix)
  val relevantToCapacity = GlobalVehicleCapacityConstraintWithLogReduction.relevantPredecessorsOfNodes(capacityInvariant)

  val relevantPredecessorsOfNodes = relevantToTime.map(x => x._1 -> x._2.toList.intersect(relevantToCapacity(x._1).toList))
  val relevantSuccessorsOfNodes = TransferFunction.relevantSuccessorsOfNodes(n,v,singleNodeTransferFunctions,timeMatrix)
  val closestRelevantNeighborsByDistance = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance,relevantPredecessorsOfNodes)(_))

  def relevantPredecessorsForLastNode(lastNode: Long) = ChainsHelper.relevantNeighborsForLastNodeAfterHead(myVRP,chainsExtension,Some(HashSet() ++ relevantPredecessorsOfNodes(lastNode)))(lastNode)
  val relevantPredecessorsForInternalNodes = ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP, chainsExtension)_




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

  val firstNodeOfChainMove = onePointMove(() => myVRP.routed.value.filter(chainsExtension.isHead),()=> myVRP.kFirst(v*2,closestRelevantNeighborsByDistance(_)), myVRP,neighborhoodName = "MoveHeadOfChain")

  def lastNodeOfChainMove(lastNode:Long) = onePointMove(() => List(lastNode),()=> myVRP.kFirst(v*2,relevantPredecessorsForLastNode), myVRP,neighborhoodName = "MoveLastOfChain")

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

  def onePtMove(k:Long) = profile(onePointMove(myVRP.routed, () => myVRP.kFirst(k,closestRelevantNeighborsByDistance(_)), myVRP)) name "One Point Move"

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

  val firstNodeOfChainInsertion = insertPointUnroutedFirst(() => chainsExtension.heads.filter(n => !myVRP.isRouted(n)),()=> myVRP.kFirst(v*2,closestRelevantNeighborsByDistance(_)), myVRP,neighborhoodName = "InsertUF")

  def lastNodeOfChainInsertion(lastNode:Long) = insertPointUnroutedFirst(() => List(lastNode),()=> myVRP.kFirst(v*2,relevantPredecessorsForLastNode), myVRP,neighborhoodName = "InsertUF")

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

  //val routeUnroutedPoint =  Profile(new InsertPointUnroutedFirst(myVRP.unrouted,()=> myVRP.kFirst(10,filteredClosestRelevantNeighborsByDistance), myVRP,neighborhoodName = "InsertUF"))


  val search = bestSlopeFirst(List(oneChainInsert,oneChainMove,onePtMove(20)))
  //val search = (BestSlopeFirst(List(routeUnroutdPoint2, routeUnroutdPoint, vlsn1pt)))


  search.verbose = 1
  //search.verboseWithExtraInfo(4, ()=> "" + myVRP)



  search.doAllMoves(obj=obj)

  println(myVRP)

  search.profilingStatistics
}