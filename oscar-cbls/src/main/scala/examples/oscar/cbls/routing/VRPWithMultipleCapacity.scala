package examples.oscar.cbls.routing

import oscar.cbls._
import oscar.cbls.algo.generator.RoutingMatrixGenerator
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.global._
import oscar.cbls.business.routing.invariants.vehicleCapacity.GlobalVehicleMultipleCapacityConstraint
import oscar.cbls.core.computation.{CBLSIntVar, Store}
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.core.objective.CascadingObjective
import oscar.cbls.{length, precedence, sum}

import scala.collection.immutable.HashSet

object VRPWithMultipleCapacity extends App{

  val n: Int = 200
  val v: Int = 5
  val c: Int = 5

  new VRPWithMultipleCapacity(n,v,c,Array.fill(c)(20),0)
}

class VRPWithMultipleCapacity(n: Int, v: Int, c: Int, maxCapacityPerContentType: Array[Long], seed: Int){
  val m = Store(noCycle = false/*, checker = Some(new ErrorChecker)*/)
  val penaltyForUnrouted = 10000

  RoutingMatrixGenerator.random.setSeed(seed)

  val symmetricDistance = RoutingMatrixGenerator.apply(n)._1
  val (listOfChains,precedences) = RoutingMatrixGenerator.generateChainsPrecedence(n,v,(n-v)/2)
  //val maxTravelDurations = RoutingMatrixGenerator.generateMaxTravelDurations(listOfChains,singleNodeTransferFunctions.map(_.ea),travelDurationMatrix)
  val contentsFlow = RoutingMatrixGenerator.generateMultipleContentsFlow(n,c,listOfChains, maxCapacityPerContentType.map(_/2))
  val vehiclesSize = RoutingMatrixGenerator.generateMultipleContentVehicleSize(v,c,10,maxCapacityPerContentType,3)

  val myVRP =  new VRP(m,n,v)

  val contentRoute = myVRP.routes.createClone()
  val precedenceRoute = contentRoute.createClone()
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
  precedencesConstraints.add(0L === precedenceInvariant)
  val chainsExtension = chains(myVRP,listOfChains)

  // Vehicle content
  val violationOfContentAtVehicle = Array.tabulate(v)(vehicle => new CBLSIntVar(m, 0, 0 to Int.MaxValue, s"violation of capacity of vehicle $vehicle"))
  val capacityInvariant = GlobalVehicleMultipleCapacityConstraint(myVRP.routes, n, v, c, vehiclesSize, contentsFlow, violationOfContentAtVehicle)

  //Constraints & objective
  val obj = new CascadingObjective(sum(violationOfContentAtVehicle),
      new CascadingObjective(precedencesConstraints,
        sum(routeLengthPerVehicles) + (penaltyForUnrouted*(n - length(myVRP.routes)))))
  m.close()

  val relevantToCapacity = capacityInvariant.relevantPredecessorsOfNodes

  val relevantPredecessorsOfNodes = relevantToCapacity
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

  val firstNodeOfChainMove = onePointMove(() => myVRP.routed.value.filter(chainsExtension.isHead),()=> myVRP.kFirst(n,closestRelevantNeighborsByDistance(_)), myVRP,neighborhoodName = "MoveHeadOfChain")

  def lastNodeOfChainMove(lastNode:Int) = onePointMove(() => List(lastNode),()=> myVRP.kFirst(n,relevantPredecessorsForLastNode), myVRP,neighborhoodName = "MoveLastOfChain")

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

  def onePtMove(k:Int) = onePointMove(myVRP.routed, () => myVRP.kFirst(k,closestRelevantNeighborsByDistance(_)), myVRP) name "One Point Move"

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

  val firstNodeOfChainInsertion = insertPointUnroutedFirst(() => chainsExtension.heads.filter(n => !myVRP.isRouted(n)),()=> myVRP.kFirst(n,closestRelevantNeighborsByDistance(_)), myVRP,neighborhoodName = "InsertUF")

  def lastNodeOfChainInsertion(lastNode:Int) = insertPointUnroutedFirst(() => List(lastNode),()=> myVRP.kFirst(n,relevantPredecessorsForLastNode), myVRP,neighborhoodName = "InsertUF")

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
  println(search.getClass)
  //val search = (BestSlopeFirst(List(routeUnroutdPoint2, routeUnroutdPoint, vlsn1pt)))

  search.verbose = 0
  //search.verboseWithExtraInfo(4, ()=> "" + myVRP)

  search.doAllMoves(obj=obj)
  println(myVRP)

  println(search.profilingStatistics)
}
