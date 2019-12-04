package oscar.examples.cbls.routing

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.global.{GlobalConstraintCore, GlobalVehicleCapacityConstraint, RouteLength}

/**
  * Created by fg on 12/05/17.
  */

object SimpleVRPWithTimeWindowsAndVehicleContent extends App{
  val m = new Store(noCycle = false/*, checker = Some(new ErrorChecker)*/)
  val v = 10
  val n = 1000
  val penaltyForUnrouted = 10000
  val maxVehicleContent = 8
  val minVehicleContent = 4

  val symmetricDistance = RoutingMatrixGenerator.apply(n)._1
  val travelDurationMatrix = RoutingMatrixGenerator.generateLinearTravelTimeFunction(n,symmetricDistance)
  val (listOfChains,precedences) = RoutingMatrixGenerator.generateChainsPrecedence(n,v,(n-v)/2)
  val (earliestArrivalTimes, latestLeavingTimes, taskDurations, maxWaitingDurations) = RoutingMatrixGenerator.generateFeasibleTimeWindows(n,v,travelDurationMatrix,listOfChains)
  val maxTravelDurations = RoutingMatrixGenerator.generateMaxTravelDurations(listOfChains,earliestArrivalTimes,travelDurationMatrix)
  val contentsFlow = RoutingMatrixGenerator.generateContentFlow(n,listOfChains,maxVehicleContent)
  val vehiclesSize = RoutingMatrixGenerator.generateVehiclesSize(v,maxVehicleContent,minVehicleContent)

  val myVRP =  new VRP(m,n,v)
  TimeWindowHelper.reduceTimeWindows(myVRP,travelDurationMatrix,maxTravelDurations,earliestArrivalTimes,latestLeavingTimes,taskDurations)

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
  val capacityInvariant = GlobalVehicleCapacityConstraint(gc, n, v, vehiclesSize, contentsFlow, violationOfContentAtVehicle)

  //TimeWindow
  val timeWindowRoute = precedenceRoute.createClone()
  val timeWindowExtension = timeWindows(Some(earliestArrivalTimes), None, None, Some(latestLeavingTimes), taskDurations, None)
  val timeWindowConstraints = new ConstraintSystem(m)
  val timeWindowInvariant = forwardCumulativeIntegerIntegerDimensionOnVehicle(
    myVRP.routes,n,v,
    (fromNode,toNode,arrivalTimeAtFromNode,leaveTimeAtFromNode)=> {
      val arrivalTimeAtToNode = leaveTimeAtFromNode + travelDurationMatrix.getTravelDuration(fromNode,0,toNode)
      val leaveTimeAtToNode =
        if(toNode < v) 0
        else Math.max(arrivalTimeAtToNode,earliestArrivalTimes(toNode)) + taskDurations(toNode)
      (arrivalTimeAtToNode,leaveTimeAtToNode)
    },
    Array.tabulate(v)(x => new CBLSIntConst(0)),
    Array.tabulate(v)(x => new CBLSIntConst(earliestArrivalTimes(x)+taskDurations(x))),
    0,
    0,
    contentName = "Time at node"
  )
  val arrivalTimes = timeWindowInvariant.content1AtNode
  val leaveTimes = timeWindowInvariant.content2AtNode
  // TravelDuration constraint
  for(maxDetour <- maxTravelDurations){
    timeWindowConstraints.post(arrivalTimes(maxDetour._1.last) - leaveTimes(maxDetour._1.head) le maxDetour._2)
  }

  //Time window constraints
  val arrivalTimesAtEnd = timeWindowInvariant.content1AtEnd
  for(i <- 0 until n){
    if(i < v && latestLeavingTimes(i) != Long.MaxValue) {
      timeWindowConstraints.post((arrivalTimesAtEnd(i) le latestLeavingTimes(i)).nameConstraint("end of time for vehicle " + i))
    } else {
      if(latestLeavingTimes(i) != Long.MaxValue)
        timeWindowConstraints.post((leaveTimes(i) le latestLeavingTimes(i)).nameConstraint("end of time window on node " + i))
      if(maxWaitingDurations(i) != Long.MaxValue)
        timeWindowConstraints.post((arrivalTimes(i) ge earliestArrivalTimes(i)).nameConstraint("start of time window on node (with duration)" + i))
    }
  }
  //Constraints & objective
  val obj = new CascadingObjective(precedencesConstraints,
    new CascadingObjective(timeWindowConstraints,
      new CascadingObjective(sum(violationOfContentAtVehicle),
        sum(routeLengthPerVehicles) + (penaltyForUnrouted*(n - length(myVRP.routes))))))

  m.close()

  val relevantToTime = TimeWindowHelper.relevantPredecessorsOfNodes(myVRP, timeWindowExtension, travelDurationMatrix)
  val relevantToCapacity = CapacityHelper.relevantPredecessorsOfNodes(myVRP, maxVehicleContent, vehiclesSize, contentsFlow)

  val relevantPredecessorsOfNodes = relevantToTime.map(x => x._1 -> x._2.filter(relevantToCapacity(x._1)))
  val relevantSuccessorsOfNodes = TimeWindowHelper.relevantSuccessorsOfNodes(myVRP, timeWindowExtension, travelDurationMatrix)
  val closestRelevantNeighborsByDistance = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance,relevantPredecessorsOfNodes)(_))

  def relevantPredecessorsForLastNode(lastNode: Long) = ChainsHelper.relevantNeighborsForLastNodeAfterHead(myVRP,chainsExtension,Some(relevantPredecessorsOfNodes(lastNode)))(lastNode)
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