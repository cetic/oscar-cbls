package oscar.examples.cbls.routing

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.timeWindow.{NaiveTimeWindowConstraint, TransferFunction}

import scala.collection.immutable.HashSet

/**
  * Created by fg on 12/05/17.
  */
object SimpleVRPWithMaxDetours extends App{
  val m = new Store(noCycle = false/*, checker = Some(new ErrorChecker)*/)
  val v = 10
  val n = 1000
  val penaltyForUnrouted = 10000
  val symmetricDistance = RoutingMatrixGenerator.apply(n)._1
  val travelDurationMatrix = symmetricDistance
  val (listOfChains,precedences)= RoutingMatrixGenerator.generateChainsPrecedence(n,v,(n-v)/2)
  val singleNodeTransferFunctions = RoutingMatrixGenerator.generateFeasibleTransferFunctions(n,v,travelDurationMatrix,listOfChains)
  val maxTravelDurations = RoutingMatrixGenerator.generateMaxTravelDurations(listOfChains,singleNodeTransferFunctions.map(_.ea),travelDurationMatrix)
  val myVRP =  new VRP(m,n,v)
  NaiveTimeWindowConstraint.maxTransferFunctionWithTravelDurationRestriction(n,v,singleNodeTransferFunctions,maxTravelDurations, listOfChains, travelDurationMatrix)

  // Distance
  val totalRouteLength = routeLength(myVRP.routes,n,v,false,symmetricDistance,true)(0)

  //Chains
  val precedenceRoute = myVRP.routes.createClone()
  val precedenceInvariant = precedence(precedenceRoute,precedences)
  val vehicleOfNodesNow = vehicleOfNodes(precedenceRoute,v)
  val precedencesConstraints = new ConstraintSystem(m)
  for(start <- precedenceInvariant.nodesStartingAPrecedence)
    precedencesConstraints.add(vehicleOfNodesNow(start) === vehicleOfNodesNow(precedenceInvariant.nodesEndingAPrecedenceStartedAt(start).head))
  precedencesConstraints.add(0 === precedenceInvariant)
  val chainsExtension = chains(myVRP,listOfChains)

  //TimeWindow
  val timeWindowRoute = precedenceRoute.createClone()
  val timeWindowConstraint = NaiveTimeWindowConstraint(timeWindowRoute, n, v, singleNodeTransferFunctions, travelDurationMatrix)
  val timeWindowViolation = timeWindowConstraint.violation



  //Objective function
  val obj = new CascadingObjective(precedencesConstraints,
    new CascadingObjective(timeWindowViolation,
      totalRouteLength + (penaltyForUnrouted*(n - length(myVRP.routes)))))

  m.close()

  val relevantPredecessorsOfNodes = TransferFunction.relevantPredecessorsOfNodes(n, v, singleNodeTransferFunctions, travelDurationMatrix)
  val relevantSuccessorsOfNodes = TransferFunction.relevantSuccessorsOfNodes(n, v, singleNodeTransferFunctions, travelDurationMatrix)
  val closestRelevantNeighborsByDistance = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance,relevantPredecessorsOfNodes)(_))

  def relevantPredecessorsForLastNode(lastNode: Int) = ChainsHelper.relevantNeighborsForLastNodeAfterHead(myVRP,chainsExtension,Some(HashSet() ++ relevantPredecessorsOfNodes(lastNode)))(lastNode)
  val relevantPredecessorsForInternalNodes = ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP, chainsExtension)_

  def postFilter(node:Int): (Int) => Boolean = {
    val routedNode = myVRP.routed.value
    (neighbor: Int) => {
      val successor = myVRP.nextNodeOf(neighbor)
      routedNode.contains(neighbor) &&
        (successor.isEmpty || relevantSuccessorsOfNodes(node).exists(_ == successor.get))
    }
  }


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

  val firstNodeOfChainMove = onePointMove(() => myVRP.routed.value.filter(chainsExtension.isHead),()=> myVRP.kFirst(v*2,closestRelevantNeighborsByDistance(_),postFilter), myVRP,neighborhoodName = "MoveHeadOfChain")

  def lastNodeOfChainMove(lastNode:Int) = onePointMove(() => List(lastNode),()=> myVRP.kFirst(v*2,relevantPredecessorsForLastNode,postFilter), myVRP,neighborhoodName = "MoveLastOfChain")

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

  def onePtMove(k:Int) = profile(onePointMove(myVRP.routed, () => myVRP.kFirst(k,closestRelevantNeighborsByDistance(_),postFilter), myVRP)) name "One Point Move"


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

  val firstNodeOfChainInsertion = insertPointUnroutedFirst(() => chainsExtension.heads.filter(n => !myVRP.isRouted(n)),()=> myVRP.kFirst(v*2,closestRelevantNeighborsByDistance(_), postFilter), myVRP,neighborhoodName = "InsertUF")

  def lastNodeOfChainInsertion(lastNode:Int) = insertPointUnroutedFirst(() => List(lastNode),()=> myVRP.kFirst(v*2,relevantPredecessorsForLastNode,postFilter), myVRP,neighborhoodName = "InsertUF")

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

  val search = bestSlopeFirst(List(oneChainInsert,oneChainMove,onePtMove(20)))

  search.verbose = 1

  search.doAllMoves(obj=obj)

  println(myVRP)

  search.profilingStatistics
}