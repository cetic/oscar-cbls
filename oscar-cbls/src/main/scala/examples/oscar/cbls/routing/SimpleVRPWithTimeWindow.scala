package examples.oscar.cbls.routing

import oscar.cbls._
import oscar.cbls.algo.generator.RoutingMatrixGenerator
import oscar.cbls.business.routing.invariants.global.RouteLength
import oscar.cbls.business.routing.invariants.timeWindow.{TimeWindowConstraint, TransferFunction}
import oscar.cbls.business.routing.model.helpers.DistanceHelper
import oscar.cbls.business.routing._
import oscar.cbls.core.search.Best
import oscar.cbls.lib.constraint.EQ

import scala.collection.immutable.HashSet

/**
 * Created by fg on 12/05/17.
 */

object SimpleVRPWithTimeWindow extends App {
  val m = new Store(noCycle = false /*, checker = Some(new ErrorChecker)*/)
  val v = 10
  val n = 500
  val penaltyForUnrouted = 10000
  val symmetricDistance = RoutingMatrixGenerator.apply(n)._1
  val timeMatrix = symmetricDistance
  val (listOfChains, precedences) = RoutingMatrixGenerator.generateChainsPrecedence(n, v, ((n - v) / 3) * 2, 4)
  val singleNodeTransferFunctions = RoutingMatrixGenerator.generateFeasibleTransferFunctions(n, v, timeMatrix, listOfChains)

  val myVRP = new VRP(m, n, v)

  // Distance
  val routeLengths = Array.fill(v)(CBLSIntVar(m, 0))
  val routeLength = new RouteLength(myVRP.routes, n, v, routeLengths, (from: Int, to: Int) => symmetricDistance(from)(to))

  //Chains
  val precedenceRoute = myVRP.routes.createClone()
  val precedenceInvariant = precedence(precedenceRoute, precedences)
  val vehicleOfNodesNow = vehicleOfNodes(precedenceRoute, v)
  val precedencesConstraints = new ConstraintSystem(m)
  for (start <- precedenceInvariant.nodesStartingAPrecedence)
    precedencesConstraints.add(EQ(vehicleOfNodesNow(start), vehicleOfNodesNow(precedenceInvariant.nodesEndingAPrecedenceStartedAt(start).head)))
  precedencesConstraints.add(EQ(0, precedenceInvariant))
  val chainsExtension = chains(myVRP, listOfChains)

  //TimeWindow
  val timeWindowViolations = Array.fill(v)(new CBLSIntVar(m, 0, Domain.coupleToDomain((0, 1))))

  val smartTimeWindowInvariant =
    TimeWindowConstraint(myVRP.routes, n, v,
      singleNodeTransferFunctions,
      timeMatrix, timeWindowViolations)

  //Objective function
  val obj = new CascadingObjective(precedencesConstraints,
    new CascadingObjective(sum(timeWindowViolations),
      sum(routeLengths) + (penaltyForUnrouted * (n - length(myVRP.routes)))))

  m.close()

  val relevantPredecessorsOfNodes = TransferFunction.relevantPredecessorsOfNodes(n, v, singleNodeTransferFunctions, timeMatrix)
  val relevantSuccessorsOfNodes = TransferFunction.relevantSuccessorsOfNodes(n, v, singleNodeTransferFunctions, timeMatrix)

  def postFilter(node: Int): Int => Boolean = {
    (neighbor: Int) => {
      val successor = myVRP.nextNodeOf(neighbor)
      myVRP.isRouted(neighbor) &&
        (successor.isEmpty || relevantSuccessorsOfNodes(node).exists(_ == successor.get))
    }
  }

  val closestRelevantPredecessorsByDistance = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance, relevantPredecessorsOfNodes)(_))

  // MOVING


  val nextMoveGenerator = {
    (exploredMoves: List[OnePointMoveMove], t: Option[List[Int]]) => {
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
            () => ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP, chainsExtension), myVRP)
          Some(moveNeighborhood, Some(newTail))
      }
    }
  }

  val firstNodeOfChainMove = onePointMove(
    () => myVRP.routed.value.filter(chainsExtension.isHead),
    () => myVRP.kFirst(v * 2, closestRelevantPredecessorsByDistance(_), postFilter), myVRP, neighborhoodName = "MoveHeadOfChain")

  def lastNodeOfChainMove(lastNode: Int) = onePointMove(
    () => List(lastNode),
    () => myVRP.kFirst(v * 2,
      ChainsHelper.relevantNeighborsForLastNodeAfterHead(
        myVRP,
        chainsExtension,
        Some(HashSet() ++ relevantPredecessorsOfNodes(lastNode))),
      postFilter),
    myVRP,
    neighborhoodName = "MoveLastOfChain")

  val oneChainMove = {
    dynAndThen(firstNodeOfChainMove,
      (moveMove: OnePointMoveMove) => {
        mu[OnePointMoveMove, Option[List[Int]]](
          lastNodeOfChainMove(chainsExtension.lastNodeInChainOfNode(moveMove.movedPoint)),
          nextMoveGenerator,
          None,
          Long.MaxValue,
          intermediaryStops = false)
      }) name "OneChainMove"
  }

  def onePtMove(k: Int) = onePointMove(myVRP.routed, () => myVRP.kFirst(k, closestRelevantPredecessorsByDistance(_), postFilter), myVRP)

  def segExchangeOnSegments(k: Int) =
    segmentExchangeOnSegments(myVRP,
      () => Array.tabulate(v)(vehicle => vehicle -> ChainsHelper.computeCompleteSegments(myVRP, vehicle, chainsExtension)).toMap,
      () => closestRelevantPredecessorsByDistance(_),
      () => 0 until v,
      selectFirstSegmentBehavior = Best(),
      selectSecondSegmentBehavior = Best(),
      selectFirstVehicleBehavior = Best(),
      selectSecondVehicleBehavior = Best()
    )

  // INSERTING

  val nextInsertGenerator = {
    (exploredMoves: List[InsertPointMove], t: Option[List[Int]]) => {
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
            () => ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP, chainsExtension), myVRP)
          Some(insertNeighborhood, Some(newTail))
      }
    }
  }

  val firstNodeOfChainInsertion = insertPointUnroutedFirst(() => myVRP.unrouted.value.filter(chainsExtension.isHead), () => {
    myVRP.kFirst(v * 2, closestRelevantPredecessorsByDistance(_), postFilter)
  }, myVRP, neighborhoodName = "InsertUF")

  def lastNodeOfChainInsertion(lastNode: Int) = insertPointUnroutedFirst(
    () => List(lastNode),
    () => myVRP.kFirst(
      v * 2,
      ChainsHelper.relevantNeighborsForLastNodeAfterHead(
        myVRP,
        chainsExtension,
        Some(HashSet() ++ relevantPredecessorsOfNodes(lastNode))),
      postFilter),
    myVRP,
    neighborhoodName = "InsertUF")

  val oneChainInsert = {
    dynAndThen(firstNodeOfChainInsertion,
      (insertMove: InsertPointMove) => {
        mu[InsertPointMove, Option[List[Int]]](
          lastNodeOfChainInsertion(chainsExtension.lastNodeInChainOfNode(insertMove.insertedPoint)),
          nextInsertGenerator,
          None,
          Long.MaxValue,
          intermediaryStops = false)
      }) name "OneChainInsert"

  }

  // REMOVING

  val nextRemoveGenerator = {
    (exploredMoves: List[RemovePointMove], t: Option[List[Int]]) => {
      val chainTail: List[Int] = t match {
        case None =>
          val removedNode = exploredMoves.head.pointToRemove
          chainsExtension.nextNodesInChain(chainsExtension.firstNodeInChainOfNode(removedNode))
        case Some(tail: List[Int]) => tail
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

  val firstNodeOfChainRemoval = removePoint(() => myVRP.routed.value.filter(chainsExtension.isHead), myVRP, neighborhoodName = "RemovePoint")

  def lastNodeOfChainRemoval(lastNode: Int) = removePoint(
    () => List(lastNode),
    myVRP,
    neighborhoodName = "RemovePoint")

  val oneChainRemove = {
    dynAndThen(firstNodeOfChainRemoval,
      (removalMove: RemovePointMove) => {
        mu[RemovePointMove, Option[List[Int]]](
          lastNodeOfChainRemoval(chainsExtension.lastNodeInChainOfNode(removalMove.pointToRemove)),
          nextRemoveGenerator,
          None,
          Long.MaxValue,
          false)
      }) name "OneChainRemove"
  }

  //val routeUnroutedPoint =  Profile(new InsertPointUnroutedFirst(myVRP.unrouted,()=> myVRP.kFirst(10,filteredClosestRelevantNeighborsByDistance), myVRP,neighborhoodName = "InsertUF"))


  val search = bestSlopeFirst(List(oneChainInsert, oneChainMove, segExchangeOnSegments(5), onePtMove(20)))
    .onExhaustRestartAfter(atomic(oneChainRemove.acceptAll(), _ > 5), 3, obj)
  //val search = (BestSlopeFirst(List(routeUnroutdPoint2, routeUnroutdPoint, vlsn1pt)))


  search.verbose = 1
  //search.verboseWithExtraInfo(2, ()=> "" + myVRP)



  search.doAllMoves(obj = obj)

  println(myVRP)

  println(search.profilingStatistics)
}
