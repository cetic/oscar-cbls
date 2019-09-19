package oscar.examples.cbls.routing

import oscar.cbls._
import oscar.cbls.business.routing.{routeLength, _}
import oscar.cbls.business.routing.invariants.group.{GlobalConstraintCore, RouteLength}
import oscar.cbls.business.routing.invariants.timeWindow.TimeWindowConstraint
import oscar.cbls.core.search.Best
import oscar.cbls.lib.constraint.EQ

import scala.collection.immutable.HashMap

/**
  * Created by fg on 12/05/17.
  */

object SimpleVRPWithTimeWindow extends App{
  val m = new Store(noCycle = false/*, checker = Some(new ErrorChecker)*/)
  val v = 10
  val n = 500
  val penaltyForUnrouted = 10000
  val symmetricDistance = RoutingMatrixGenerator.apply(n)._1
  val travelDurationMatrix = RoutingMatrixGenerator.generateLinearTravelTimeFunction(n,symmetricDistance)
  val (listOfChains,precedences) = RoutingMatrixGenerator.generateChainsPrecedence(n,v,((n-v)/3)*2,4)
  val (earliestArrivalTimes, latestLeavingTimes, taskDurations, maxWaitingDurations) = RoutingMatrixGenerator.generateFeasibleTimeWindows(n,v,travelDurationMatrix,listOfChains)

  val myVRP =  new VRP(m,n,v)
  val gc = GlobalConstraintCore(myVRP.routes,v)

  // Distance
  val routeLengths = Array.fill(v)(CBLSIntVar(m,0))
  val routeLength = new RouteLength(gc,n,v,routeLengths,(from: Long, to: Long) => symmetricDistance(from)(to))


  //Chains
  val precedenceRoute = myVRP.routes.createClone()
  val precedenceInvariant = precedence(precedenceRoute,precedences)
  val vehicleOfNodesNow = vehicleOfNodes(precedenceRoute,v)
  val precedencesConstraints = new ConstraintSystem(m)
  for(start <- precedenceInvariant.nodesStartingAPrecedence)
    precedencesConstraints.add(EQ(vehicleOfNodesNow(start),vehicleOfNodesNow(precedenceInvariant.nodesEndingAPrecedenceStartedAt(start).head)))
  precedencesConstraints.add(EQ(0,precedenceInvariant))
  val chainsExtension = chains(myVRP,listOfChains)

  //TimeWindow
  val timeWindowRoute = precedenceRoute.createClone()
  val timeWindowExtension = timeWindows(Some(earliestArrivalTimes), None, None, Some(latestLeavingTimes), taskDurations, None)
  val timeWindowViolations = Array.fill(v)(new CBLSIntVar(m, 0, Domain.coupleToDomain((0,1))))
  val timeMatrix = Array.tabulate(n)(from => Array.tabulate(n)(to => travelDurationMatrix.getTravelDuration(from, 0, to)))
  //println("travel durations: \n" + timeMatrix.zipWithIndex.map(from => from._2 -> from._1.zipWithIndex.map(to => "" + to._2 + " : " + to._1).mkString(", ")).mkString("\n"))

  val smartTimeWindowInvariant =
    TimeWindowConstraint(gc, n, v,
      earliestArrivalTimes,
      latestLeavingTimes,
      taskDurations,
      timeMatrix, timeWindowViolations)

  //Objective function
  val obj = new CascadingObjective(precedencesConstraints,
    new CascadingObjective(sum(timeWindowViolations),
      sum(routeLengths) + (penaltyForUnrouted*(n - length(myVRP.routes)))))

  m.close()

  val relevantPredecessorsOfNodes = TimeWindowHelper.relevantPredecessorsOfNodes(myVRP, timeWindowExtension, travelDurationMatrix)
  val relevantSuccessorsOfNodes = TimeWindowHelper.relevantSuccessorsOfNodes(myVRP, timeWindowExtension, travelDurationMatrix)

  def postFilter(node:Long): (Long) => Boolean = {
    (neighbor: Long) => {
      val successor = myVRP.nextNodeOf(neighbor)
      myVRP.isRouted(neighbor) &&
        (successor.isEmpty || relevantSuccessorsOfNodes(node).contains(successor.get))
    }
  }

  val closestRelevantPredecessorsByDistance = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance,relevantPredecessorsOfNodes)(_))

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
            () => ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP,chainsExtension), myVRP)
          Some(moveNeighborhood, Some(newTail))
      }
    }
  }

  val firstNodeOfChainMove = onePointMove(
    () => myVRP.routed.value.filter(chainsExtension.isHead),
    ()=> myVRP.kFirst(v*2,closestRelevantPredecessorsByDistance(_),postFilter), myVRP,neighborhoodName = "MoveHeadOfChain")

  def lastNodeOfChainMove(lastNode:Long) = onePointMove(
    () => List(lastNode),
    ()=> myVRP.kFirst(v*2,
      ChainsHelper.relevantNeighborsForLastNodeAfterHead(
        myVRP,
        chainsExtension,
        Some(relevantPredecessorsOfNodes(lastNode))),
      postFilter),
    myVRP,
    neighborhoodName = "MoveLastOfChain")

  val oneChainMove = {
    profile(dynAndThen(firstNodeOfChainMove,
      (moveMove: OnePointMoveMove) => {
        mu[OnePointMoveMove, Option[List[Long]]](
          lastNodeOfChainMove(chainsExtension.lastNodeInChainOfNode(moveMove.movedPoint)),
          nextMoveGenerator,
          None,
          Long.MaxValue,
          false)
      })name "OneChainMove")
  }

  def onePtMove(k:Long) = profile(onePointMove(myVRP.routed, () => myVRP.kFirst(k,closestRelevantPredecessorsByDistance(_),postFilter), myVRP))

  def segExchangeOnSegments(k: Long) = profile(
    segmentExchangeOnSegments(myVRP,
      () => Array.tabulate(v)(vehicle => intToLong(vehicle) -> ChainsHelper.computeCompleteSegments(myVRP,vehicle,chainsExtension)).toMap,
      ()=> closestRelevantPredecessorsByDistance(_),
      () => 0L until v,
      selectFirstSegmentBehavior = Best(),
      selectSecondSegmentBehavior = Best(),
      selectFirstVehicleBehavior = Best(),
      selectSecondVehicleBehavior = Best()
    ))

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
            () => ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP,chainsExtension), myVRP)
          Some(insertNeighborhood, Some(newTail))
      }
    }
  }

  val firstNodeOfChainInsertion = insertPointUnroutedFirst(() => myVRP.unrouted.value.filter(chainsExtension.isHead),()=> {
    myVRP.kFirst(v*2,closestRelevantPredecessorsByDistance(_), postFilter)
  }, myVRP,neighborhoodName = "InsertUF")

  def lastNodeOfChainInsertion(lastNode:Long) = insertPointUnroutedFirst(
    () => List(lastNode),
    ()=> myVRP.kFirst(
      v*2,
      ChainsHelper.relevantNeighborsForLastNodeAfterHead(
        myVRP,
        chainsExtension,
        Some(relevantPredecessorsOfNodes(lastNode))),
      postFilter),
    myVRP,
    neighborhoodName = "InsertUF")

  val oneChainInsert = {
    profile(dynAndThen(firstNodeOfChainInsertion,
      (insertMove: InsertPointMove) => {
        mu[InsertPointMove,Option[List[Long]]](
          lastNodeOfChainInsertion(chainsExtension.lastNodeInChainOfNode(insertMove.insertedPoint)),
          nextInsertGenerator,
          None,
          Long.MaxValue,
          false)
      })name "OneChainInsert")

  }


  // REMOVING

  val nextRemoveGenerator = {
    (exploredMoves:List[RemovePointMove], t:Option[List[Long]]) => {
      val chainTail: List[Long] = t match {
        case None =>
          val removedNode = exploredMoves.head.pointToRemove
          chainsExtension.nextNodesInChain(chainsExtension.firstNodeInChainOfNode(removedNode))
        case Some(tail: List[Long]) => tail
      }

      chainTail match {
        case Nil => None
        case head :: Nil => None
        case nextNodeToRemove :: newTail =>
          val insertNeighborhood = removePoint(() => Some(nextNodeToRemove), myVRP)
          Some(insertNeighborhood, Some(newTail))
      }
    }
  }

  val firstNodeOfChainRemoval = removePoint(() => myVRP.unrouted.value.filter(chainsExtension.isHead), myVRP,neighborhoodName = "RemovePoint")

  def lastNodeOfChainRemoval(lastNode:Long) = removePoint(
    () => List(lastNode),
    myVRP,
    neighborhoodName = "RemovePoint")

  val oneChainRemove = {
    profile(dynAndThen(firstNodeOfChainRemoval,
      (removalMove: RemovePointMove) => {
        mu[RemovePointMove,Option[List[Long]]](
          lastNodeOfChainRemoval(chainsExtension.lastNodeInChainOfNode(removalMove.pointToRemove)),
          nextRemoveGenerator,
          None,
          Long.MaxValue,
          false)
      })name "OneChainInsert")
  }

  //val routeUnroutedPoint =  Profile(new InsertPointUnroutedFirst(myVRP.unrouted,()=> myVRP.kFirst(10,filteredClosestRelevantNeighborsByDistance), myVRP,neighborhoodName = "InsertUF"))


  val search = bestSlopeFirst(List(oneChainInsert,oneChainMove,segExchangeOnSegments(5),onePtMove(20)))onExhaustRestartAfter(atomic(oneChainRemove.acceptAll(), _ > 1),3, obj)
  //val search = (BestSlopeFirst(List(routeUnroutdPoint2, routeUnroutdPoint, vlsn1pt)))


  search.verbose = 2
  //search.verboseWithExtraInfo(2, ()=> "" + myVRP)



  search.doAllMoves(obj=obj)

  println(myVRP)

  println(search.profilingStatistics)
}