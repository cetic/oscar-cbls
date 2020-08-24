package oscar.examples.cbls.routing

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.global.{GlobalConstraintCore, RouteLength}
import oscar.cbls.business.routing.invariants.vehicleCapacity.GlobalVehicleCapacityConstraint
import oscar.cbls.core.computation.{CBLSIntVar, Store}
import oscar.cbls.core.constraint.ConstraintSystem

import scala.collection.immutable.HashSet

/**
  * Created by fg on 12/05/17.
  */

object SimplePDPTW extends App{
  val m = Store(noCycle = false)
  val v = 10
  val n = 500
  val penaltyForUnrouted = 10000
  val maxVehicleCapacity = 8
  val minVehicleCapacity = 4
  val symmetricDistance = RoutingMatrixGenerator.apply(n)._1
  val (listOfChains,precedences) = RoutingMatrixGenerator.generateChainsPrecedence(n,v,(n-v)/2)
  val contentsFlow = RoutingMatrixGenerator.generateContentFlow(n,listOfChains,maxVehicleCapacity)
  val vehiclesCapacity = RoutingMatrixGenerator.generateVehiclesSize(v,maxVehicleCapacity,minVehicleCapacity)

  val myVRP =  new VRP(m,n,v)

  // GC
  val gc = GlobalConstraintCore(myVRP.routes, v)

  // Distance
  val vehiclesRouteLength = Array.tabulate(v)(vehicle => CBLSIntVar(m, name = "Route length of vehicle " + vehicle))
  val routeLengthInvariant = new RouteLength(gc,n,v,vehiclesRouteLength,(from: Int, to: Int) => symmetricDistance(from)(to))

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
  val violationOfContentOfVehicle = Array.tabulate(v)(vehicle =>
    CBLSIntVar(myVRP.routes.model, name = "Violation of capacity of vehicle " + vehicle))
  val capacityInvariant = GlobalVehicleCapacityConstraint(gc, n, v, vehiclesCapacity, contentsFlow, violationOfContentOfVehicle)

  //Objective function
  val obj = new CascadingObjective(precedencesConstraints,
    new CascadingObjective(sum(violationOfContentOfVehicle),
      sum(vehiclesRouteLength) + (penaltyForUnrouted*(n - length(myVRP.routes)))))

  m.close()

  /*def postFilter(node:Int): (Int) => Boolean = {
    val enoughSpaceAfterNeighborNow: (Int,Int,Array[Int]) => Boolean =
      CapacityHelper.enoughSpaceAfterNeighbor(n,capacityInvariant)
    (neighbor: Int) => {
      myVRP.isRouted(neighbor) &&
        enoughSpaceAfterNeighborNow(node,neighbor,contentsFlow)
    }
  }*/

  val relevantPredecessors = capacityInvariant.relevantPredecessorsOfNodes

  val closestRelevantPredecessorsByDistance = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance,relevantPredecessors)(_))

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
            () => ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP,chainsExtension), myVRP)
          Some(moveNeighborhood, Some(newTail))
      }
    }
  }

  val firstNodeOfChainMove = onePointMove(
    () => myVRP.routed.value.filter(chainsExtension.isHead),
    ()=> myVRP.kFirst(v*2,closestRelevantPredecessorsByDistance(_)), myVRP,neighborhoodName = "MoveHeadOfChain")

  def lastNodeOfChainMove(lastNode:Int) = onePointMove(
    () => List(lastNode),
    ()=> myVRP.kFirst(v*2,
      ChainsHelper.relevantNeighborsForLastNodeAfterHead(
        myVRP,
        chainsExtension,
        Some(HashSet() ++ relevantPredecessors(lastNode)))),
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
          false)
      }) name "OneChainMove"
  }

  def onePtMove(k:Int) = profile(onePointMove(myVRP.routed, () => myVRP.kFirst(k,closestRelevantPredecessorsByDistance(_)), myVRP))

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
            () => ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP,chainsExtension), myVRP)
          Some(insertNeighborhood, Some(newTail))
      }
    }
  }

  val firstNodeOfChainInsertion = insertPointUnroutedFirst(() => myVRP.unrouted.value.filter(chainsExtension.isHead),()=> {
    myVRP.kFirst(v*2,closestRelevantPredecessorsByDistance(_))
  }, myVRP,neighborhoodName = "InsertUF")

  def lastNodeOfChainInsertion(lastNode:Int) = insertPointUnroutedFirst(
    () => List(lastNode),
    ()=> myVRP.kFirst(
      v*2,
      ChainsHelper.relevantNeighborsForLastNodeAfterHead(
        myVRP,
        chainsExtension)),
    myVRP,
    neighborhoodName = "InsertUF")

  val oneChainInsert = {
    dynAndThen(firstNodeOfChainInsertion,
      (insertMove: InsertPointMove) => {
        mu[InsertPointMove,Option[List[Int]]](
          lastNodeOfChainInsertion(chainsExtension.lastNodeInChainOfNode(insertMove.insertedPoint)),
          nextInsertGenerator,
          None,
          Long.MaxValue,
          false)
      }) name "OneChainInsert"

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

